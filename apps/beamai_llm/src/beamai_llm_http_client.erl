%%%-------------------------------------------------------------------
%%% @doc LLM HTTP 客户端公共模块
%%%
%%% 提供 LLM Provider 共用的 HTTP 请求和流式处理功能。
%%% 基于 beamai_http 构建，添加 LLM 特定的 SSE 解析和累加器。
%%%
%%% 设计原则：
%%%   - 使用 beamai_http 作为底层 HTTP 客户端
%%%   - 提供 LLM 特定的 SSE 解析和事件累加
%%%   - 使用回调函数处理 Provider 特定的差异
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_http_client).

-include_lib("beamai_core/include/beamai_common.hrl").

%% 同步请求 API
-export([request/4, request/5]).

%% 流式请求 API
-export([stream_request/5, stream_request/6]).

%% 连接池路由（供直接调 beamai_http 的 provider 复用，如智谱异步轮询）
-export([maybe_inject_pool/3]).

%% SSE 解析工具
-export([parse_sse/1, parse_sse_lines/2]).

%% 流式累加器
-export([init_stream_acc/0, finalize_stream/1]).

%%====================================================================
%% 类型定义
%%====================================================================

-type request_opts() :: #{
    timeout => pos_integer(),
    connect_timeout => pos_integer(),
    stream_timeout => pos_integer(),
    finalizer => stream_finalizer(),
    on_headers => header_handler(),
    %% 显式指定连接池，覆盖按请求形态的默认路由（Gun 后端：
    %% http_pool_short | http_pool_stream | http_pool_longpoll；
    %% 非 Gun 后端按其自身语义解释池名）
    pool => atom()
}.

%% 响应头处理器：从响应头提取要合并进 response.metadata 的 map
%% （如速率限制信息）。返回空 map 表示无需注入。
-type header_handler() :: fun(([{binary(), binary()}]) -> map()).

-type response_parser() :: fun((map()) -> {ok, map()} | {error, term()}).
-type event_accumulator() :: fun((map(), map()) -> map()).
-type stream_callback() :: fun((map()) -> any()).
-type stream_finalizer() :: fun((map()) -> {ok, map()} | {error, term()}).

-export_type([request_opts/0, response_parser/0, event_accumulator/0,
              stream_callback/0, stream_finalizer/0, header_handler/0]).

%%====================================================================
%% 同步请求 API
%%====================================================================

%% @doc 发送 HTTP POST 请求
%% 使用默认响应解析（返回原始 JSON）
-spec request(binary(), [{binary(), binary()}], map(), request_opts()) ->
    {ok, map()} | {error, term()}.
request(Url, Headers, Body, Opts) ->
    request(Url, Headers, Body, Opts, fun(R) -> {ok, R} end).

%% @doc 发送 HTTP POST 请求（带自定义响应解析器）
%% 使用 beamai_http 作为底层 HTTP 客户端
-spec request(binary(), [{binary(), binary()}], map(), request_opts(), response_parser()) ->
    {ok, map()} | {error, term()}.
request(Url, Headers, Body, Opts, ResponseParser) ->
    HttpOpts = maybe_inject_pool(chat, Opts, #{
        timeout => maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
        connect_timeout => maps:get(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT)
    }),
    %% 使用 beamai_http:request 直接传入 headers，避免 post_json 重复添加 Content-Type
    JsonBody = jsx:encode(Body),
    case maps:get(on_headers, Opts, undefined) of
        OnHeaders when is_function(OnHeaders, 1) ->
            request_with_headers(Url, Headers, JsonBody, HttpOpts, ResponseParser, OnHeaders);
        _ ->
            case beamai_http:request(post, Url, Headers, JsonBody, HttpOpts) of
                {ok, Response} ->
                    parse_response_body(Response, ResponseParser);
                {error, {http_error, Code, RespBody}} ->
                    {error, {http_error, Code, RespBody}};
                {error, Reason} ->
                    {error, {request_failed, Reason}}
            end
    end.

%% @private 请求并把响应头提取的元信息合并进 response.metadata
request_with_headers(Url, Headers, JsonBody, HttpOpts, ResponseParser, OnHeaders) ->
    case beamai_http:request_meta(post, Url, Headers, JsonBody, HttpOpts) of
        {ok, Response, Meta} ->
            case parse_response_body(Response, ResponseParser) of
                {ok, Resp} ->
                    RespHeaders = maps:get(headers, Meta, []),
                    HeaderMeta = safe_extract_headers(OnHeaders, RespHeaders),
                    {ok, inject_metadata(Resp, HeaderMeta)};
                Other ->
                    Other
            end;
        {error, {http_error, Code, RespBody, RespHeaders}} ->
            %% 4xx/5xx 带响应头：若有 Retry-After 则附带，供重试层按服务端建议退避
            case beamai_llm_provider_common:retry_after_ms(RespHeaders) of
                undefined -> {error, {http_error, Code, RespBody}};
                Ms -> {error, {http_error, Code, RespBody, #{retry_after_ms => Ms}}}
            end;
        {error, {http_error, Code, RespBody}} ->
            {error, {http_error, Code, RespBody}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%%====================================================================
%% 连接池路由
%%====================================================================

%% @doc 解析请求应使用的连接池并写入 HttpOpts。
%%
%% 优先级：
%% 1. 调用方在请求 Opts 里显式指定的 pool（按请求/按 provider 覆盖）——
%%    原样透传，不做后端门控：显式指定视为调用方对后端知情。Gun 后端下
%%    非法池名由 beamai_http_gun:resolve_pool_name/1 拒绝。
%% 2. 未指定时按请求形态走默认路由表——仅当活动后端是 Gun 后端。
%%    本模块与后端无关：它调 beamai_http，由后者分发到 http_backend 配置的
%%    任意后端。池名（http_pool_short 等）是 Gun 专有语义，自动泄漏给别的
%%    后端只会指向它那边从未启动过的池，因此非 Gun 后端（如测试 fake
%%    后端）下不自动注入。
%%
%% 默认路由表：chat -> short，stream -> stream，async_poll -> longpoll。
%% 导出供直接调 beamai_http 的 provider 复用（智谱异步轮询）。
-spec maybe_inject_pool(chat | stream | async_poll, request_opts(), map()) -> map().
maybe_inject_pool(RequestType, Opts, HttpOpts) ->
    case maps:find(pool, Opts) of
        {ok, Pool} ->
            HttpOpts#{pool => Pool};
        error ->
            case beamai_http:get_backend() of
                beamai_http_gun -> HttpOpts#{pool => select_pool(RequestType)};
                _ -> maps:remove(pool, HttpOpts)
            end
    end.

%% @private 请求形态 -> 池名
select_pool(chat)       -> http_pool_short;
select_pool(stream)     -> http_pool_stream;
select_pool(async_poll) -> http_pool_longpoll.

%% @private 解析响应体（map 直接解析；binary 先解 JSON）
parse_response_body(Response, ResponseParser) when is_map(Response) ->
    ResponseParser(Response);
parse_response_body(Response, ResponseParser) when is_binary(Response) ->
    case beamai_utils:parse_json(Response) of
        Parsed when map_size(Parsed) > 0 -> ResponseParser(Parsed);
        Empty ->
            logger:warning("Failed to parse response JSON: ~ts", [Response]),
            {error, {parse_error, Empty}}
    end;
parse_response_body(Response, _ResponseParser) ->
    logger:warning("Unexpected response type: ~p", [Response]),
    {error, {unexpected_response, Response}}.

%% @private 安全调用响应头处理器（异常或非 map 返回时降级为空）
safe_extract_headers(Fun, Headers) ->
    try Fun(Headers) of
        Map when is_map(Map) -> Map;
        _ -> #{}
    catch _:_ -> #{}
    end.

%% @private 把响应头元信息合并进 response 的 metadata 字段
inject_metadata(Resp, HeaderMeta) when is_map(Resp), is_map(HeaderMeta), map_size(HeaderMeta) > 0 ->
    Existing = maps:get(metadata, Resp, #{}),
    Resp#{metadata => maps:merge(Existing, HeaderMeta)};
inject_metadata(Resp, _HeaderMeta) ->
    Resp.

%%====================================================================
%% 流式请求 API
%%====================================================================

%% @doc 发送流式 HTTP 请求
%% 使用默认事件累加器（与之配对的默认 finalize_stream/1，可被 Opts 覆盖）
-spec stream_request(binary(), [{binary(), binary()}], map(), request_opts(), stream_callback()) ->
    {ok, map()} | {error, term()}.
stream_request(Url, Headers, Body, Opts, Callback) ->
    Opts1 = Opts#{finalizer => maps:get(finalizer, Opts, fun finalize_stream/1)},
    stream_request(Url, Headers, Body, Opts1, Callback, fun default_accumulator/2).

%% @doc 发送流式 HTTP 请求（带自定义事件累加器）
%% 使用 beamai_http:stream_request 作为底层，添加 SSE 解析
%%
%% Opts **必须**包含 finalizer，用于将累加结果转换为最终响应
%% （如转成与同步模式一致的 beamai_llm_response 结构）。自定义累加器的
%% 中间形态只有与之配对的 finalizer 能理解，漏传几乎必是 provider 实现
%% bug —— 返回 {error, {missing_stream_finalizer, _}}（经 beamai_llm_error
%% 归一后不可重试）而非静默退化为通用 finalize（那会丢 usage、分片
%% tool_calls 合并等统一响应结构）。
-spec stream_request(binary(), [{binary(), binary()}], map(), request_opts(),
                     stream_callback(), event_accumulator()) ->
    {ok, map()} | {error, term()}.
stream_request(Url, Headers, Body, Opts, Callback, Accumulator) ->
    case maps:get(finalizer, Opts, undefined) of
        F when is_function(F, 1) ->
            do_stream_request(Url, Headers, Body, Opts, Callback, Accumulator, F);
        _ ->
            {error, {missing_stream_finalizer,
                     <<"stream_request/6 with a custom accumulator requires a "
                       "paired finalizer in Opts">>}}
    end.

%% @private 实际执行流式请求（finalizer 已校验）
do_stream_request(Url, Headers, Body, Opts, Callback, Accumulator, Finalizer) ->
    OnHeaders = maps:get(on_headers, Opts, undefined),
    StreamBody = Body#{<<"stream">> => true},
    HttpOpts = maybe_inject_pool(stream, Opts, #{
        timeout => maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
        connect_timeout => maps:get(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT),
        headers => Headers,
        %% 设置了 on_headers 时，请求后端把首个响应头以 {http_headers, _} 回传
        forward_headers => is_function(OnHeaders, 1),
        init_acc => #{buffer => <<>>, acc => init_stream_acc(), callback => Callback, accumulator => Accumulator}
    }),
    %% 使用 beamai_http 的流式请求，传入 SSE 处理器
    case beamai_http:stream_request(post, Url, [], StreamBody, HttpOpts, fun sse_chunk_handler/2) of
        {ok, State} when is_map(State) ->
            finalize_stream_response(Finalizer, State, OnHeaders);
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 完成流式响应，并按需把响应头元信息注入 metadata
finalize_stream_response(Finalizer, State, OnHeaders) ->
    FinalAcc = maps:get(acc, State, State),
    case Finalizer(FinalAcc) of
        {ok, Resp} when is_function(OnHeaders, 1) ->
            RespHeaders = maps:get(resp_headers, State, []),
            {ok, inject_metadata(Resp, safe_extract_headers(OnHeaders, RespHeaders))};
        Other ->
            Other
    end.

%%====================================================================
%% SSE 处理核心
%%====================================================================

%% @private SSE 数据块处理器
%% 解析 SSE 事件并调用回调和累加器
-spec sse_chunk_handler(binary() | {http_headers, [{binary(), binary()}]}, map()) ->
    {continue, map()} | {done, map()}.
sse_chunk_handler({http_headers, RespHeaders}, State) ->
    %% 后端在 forward_headers 开启时回传首个响应头，暂存供 finalize 阶段提取
    {continue, State#{resp_headers => RespHeaders}};
sse_chunk_handler(Chunk, #{buffer := Buffer, acc := Acc, callback := Callback, accumulator := Accumulator} = State) ->
    {NewBuffer, Events} = parse_sse(<<Buffer/binary, Chunk/binary>>),
    NewAcc = process_events(Events, Acc, Callback, Accumulator),
    {continue, State#{buffer => NewBuffer, acc => NewAcc}}.

%% @private 处理事件列表
-spec process_events([map() | done | skip], map(), stream_callback(), event_accumulator()) -> map().
process_events([], Acc, _Callback, _Accumulator) ->
    Acc;
process_events([done | _], Acc, _Callback, _Accumulator) ->
    Acc;
process_events([skip | Rest], Acc, Callback, Accumulator) ->
    process_events(Rest, Acc, Callback, Accumulator);
process_events([Event | Rest], Acc, Callback, Accumulator) ->
    Callback(Event),
    NewAcc = Accumulator(Event, Acc),
    process_events(Rest, NewAcc, Callback, Accumulator).

%%====================================================================
%% SSE 解析
%%====================================================================

%% @doc 解析 SSE 数据
%% 返回 {未处理的剩余数据, 解析出的事件列表}
-spec parse_sse(binary()) -> {binary(), [map() | done | skip]}.
parse_sse(Data) ->
    Lines = binary:split(Data, <<"\n">>, [global]),
    parse_sse_lines(Lines, []).

%% @doc 解析 SSE 行列表
-spec parse_sse_lines([binary()], [map() | done | skip]) -> {binary(), [map() | done | skip]}.
parse_sse_lines([], Acc) ->
    {<<>>, lists:reverse(Acc)};
parse_sse_lines([<<>>], Acc) ->
    {<<>>, lists:reverse(Acc)};
parse_sse_lines([<<"data: [DONE]">> | Rest], Acc) ->
    parse_sse_lines(Rest, [done | Acc]);
parse_sse_lines([<<"data: ", Json/binary>> | Rest], Acc) ->
    Event = safe_decode_json(Json),
    parse_sse_lines(Rest, [Event | Acc]);
parse_sse_lines([LastLine], Acc) ->
    %% 最后一行可能是不完整的数据，保留到下次处理
    {LastLine, lists:reverse(Acc)};
parse_sse_lines([_ | Rest], Acc) ->
    parse_sse_lines(Rest, Acc).

%% @private 安全解析 JSON
-spec safe_decode_json(binary()) -> map() | skip.
safe_decode_json(Json) ->
    try jsx:decode(Json, [return_maps])
    catch _:_ -> skip
    end.

%%====================================================================
%% 流式累加器
%%====================================================================

%% @doc 初始化流式累加器
-spec init_stream_acc() -> map().
init_stream_acc() ->
    #{
        id => <<>>,
        model => <<>>,
        content => <<>>,
        tool_calls => [],
        finish_reason => <<>>
    }.

%% @doc 完成流式处理，生成最终结果
-spec finalize_stream(map()) -> {ok, map()}.
finalize_stream(Acc) ->
    {ok, Acc#{
        usage => #{
            prompt_tokens => 0,
            completion_tokens => 0,
            total_tokens => 0
        }
    }}.

%% @private 默认事件累加器（OpenAI 格式）
-spec default_accumulator(map(), map()) -> map().
default_accumulator(#{<<"choices">> := [#{<<"delta">> := Delta} | _]} = Event, Acc) ->
    Content = maps:get(<<"content">>, Delta, <<>>),
    ContentBin = beamai_utils:ensure_binary(Content),
    Acc#{
        id => maps:get(<<"id">>, Event, maps:get(id, Acc)),
        model => maps:get(<<"model">>, Event, maps:get(model, Acc)),
        content => <<(maps:get(content, Acc))/binary, ContentBin/binary>>
    };
default_accumulator(_, Acc) ->
    Acc.
