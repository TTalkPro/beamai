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
-module(llm_http_client).

-include_lib("beamai_core/include/beamai_common.hrl").

%% 同步请求 API
-export([request/4, request/5]).

%% 流式请求 API
-export([stream_request/5, stream_request/6]).

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
    stream_timeout => pos_integer()
}.

-type response_parser() :: fun((map()) -> {ok, map()} | {error, term()}).
-type event_accumulator() :: fun((map(), map()) -> map()).
-type stream_callback() :: fun((map()) -> any()).

-export_type([request_opts/0, response_parser/0, event_accumulator/0, stream_callback/0]).

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
    HttpOpts = #{
        timeout => maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
        connect_timeout => maps:get(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT),
        headers => Headers
    },
    case beamai_http:post_json(Url, Body, HttpOpts) of
        {ok, Response} when is_map(Response) ->
            ResponseParser(Response);
        {ok, Response} when is_binary(Response) ->
            %% 如果是 binary，尝试解析 JSON
            case beamai_utils:parse_json(Response) of
                Parsed when map_size(Parsed) > 0 -> ResponseParser(Parsed);
                Empty ->
                    error_logger:warning_msg("Failed to parse response JSON: ~ts~n", [Response]),
                    {error, {parse_error, Empty}}
            end;
        {ok, Response} ->
            %% 其他类型，记录并返回错误
            error_logger:warning_msg("Unexpected response type: ~p~n", [Response]),
            {error, {unexpected_response, Response}};
        {error, {http_error, Code, RespBody}} ->
            {error, {http_error, Code, RespBody}};
        {error, Reason} ->
            {error, {request_failed, Reason}}
    end.

%%====================================================================
%% 流式请求 API
%%====================================================================

%% @doc 发送流式 HTTP 请求
%% 使用默认事件累加器
-spec stream_request(binary(), [{binary(), binary()}], map(), request_opts(), stream_callback()) ->
    {ok, map()} | {error, term()}.
stream_request(Url, Headers, Body, Opts, Callback) ->
    stream_request(Url, Headers, Body, Opts, Callback, fun default_accumulator/2).

%% @doc 发送流式 HTTP 请求（带自定义事件累加器）
%% 使用 beamai_http:stream_request 作为底层，添加 SSE 解析
-spec stream_request(binary(), [{binary(), binary()}], map(), request_opts(),
                     stream_callback(), event_accumulator()) ->
    {ok, map()} | {error, term()}.
stream_request(Url, Headers, Body, Opts, Callback, Accumulator) ->
    StreamBody = Body#{<<"stream">> => true},
    HttpOpts = #{
        timeout => maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
        connect_timeout => maps:get(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT),
        headers => Headers,
        init_acc => #{buffer => <<>>, acc => init_stream_acc(), callback => Callback, accumulator => Accumulator}
    },
    %% 使用 beamai_http 的流式请求，传入 SSE 处理器
    case beamai_http:stream_request(post, Url, [], StreamBody, HttpOpts, fun sse_chunk_handler/2) of
        {ok, #{acc := FinalAcc}} ->
            finalize_stream(FinalAcc);
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% SSE 处理核心
%%====================================================================

%% @private SSE 数据块处理器
%% 解析 SSE 事件并调用回调和累加器
-spec sse_chunk_handler(binary(), map()) -> {continue, map()} | {done, map()}.
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
