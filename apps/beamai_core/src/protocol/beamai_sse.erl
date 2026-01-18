%%%-------------------------------------------------------------------
%%% @doc SSE (Server-Sent Events) 通用模块
%%%
%%% 提供 SSE 协议的解析和编码功能。
%%% 支持客户端解析和服务端编码两种场景。
%%%
%%% == SSE 格式规范 ==
%%%
%%% SSE 事件格式：
%%% ```
%%% event: <event-type>
%%% id: <event-id>
%%% data: <event-data>
%%% retry: <retry-interval>
%%%
%%% ```
%%%
%%% 注意：
%%% - 每个字段以换行符（\n）结尾
%%% - 事件以空行（\n\n）分隔
%%% - data 可以有多行，每行都以 "data: " 开头
%%% - 以冒号（:）开头的行是注释
%%%
%%% == 使用示例 ==
%%%
%%% 解析（客户端）：
%%% ```erlang
%%% {Remaining, Events} = beamai_sse:parse(ChunkBinary),
%%% lists:foreach(fun(Event) ->
%%%     #{event := Type, data := Data} = Event,
%%%     handle_event(Type, Data)
%%% end, Events).
%%% ```
%%%
%%% 编码（服务端）：
%%% ```erlang
%%% %% 发送事件
%%% EventBin = beamai_sse:encode_event(<<"message">>, JsonData),
%%% cowboy_req:stream_body(EventBin, nofin, Req).
%%%
%%% %% 发送心跳注释
%%% CommentBin = beamai_sse:encode_comment(<<"ping">>),
%%% cowboy_req:stream_body(CommentBin, nofin, Req).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_sse).

%%====================================================================
%% API 导出
%%====================================================================

%% 解析 API（客户端使用）
-export([
    parse/1,
    parse_line/2,
    extract_json_data/1
]).

%% 编码 API（服务端使用）
-export([
    encode_event/1,
    encode_event/2,
    encode_event/3,
    encode_data/1,
    encode_comment/1,
    encode_retry/1,
    encode_id/1
]).

%% 类型导出
-export_type([sse_event/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type sse_event() :: #{
    event => binary(),           %% 事件类型（默认 "message"）
    data => binary() | term(),   %% 事件数据
    id => binary(),              %% 事件 ID
    retry => pos_integer()       %% 重连间隔（毫秒）
}.

%%====================================================================
%% 解析 API
%%====================================================================

%% @doc 解析 SSE 数据流
%%
%% 从二进制数据中解析 SSE 事件。返回未处理的剩余数据和解析出的事件列表。
%%
%% @param Data 二进制数据
%% @returns {Remaining, [Event]}
%%   - Remaining: 不完整的数据，等待下次处理
%%   - [Event]: 解析出的事件列表
-spec parse(binary()) -> {binary(), [sse_event()]}.
parse(Data) ->
    %% 按双换行符分割事件
    case binary:split(Data, <<"\n\n">>, [global]) of
        [Single] ->
            %% 没有完整事件
            {Single, []};
        Parts ->
            %% 最后一部分可能不完整
            {Remaining, CompleteParts} = split_last(Parts),
            Events = [parse_event(P) || P <- CompleteParts, P =/= <<>>],
            {Remaining, Events}
    end.

%% @doc 解析单行 SSE 数据
%%
%% 解析 SSE 行并更新事件累加器。
%%
%% @param Line 单行数据
%% @param Acc 当前事件累加器
%% @returns 更新后的事件累加器
-spec parse_line(binary(), map()) -> map().
parse_line(<<>>, Acc) ->
    Acc;
parse_line(<<":", _Comment/binary>>, Acc) ->
    %% 注释行，忽略
    Acc;
parse_line(<<"event:", Rest/binary>>, Acc) ->
    Acc#{event => trim(Rest)};
parse_line(<<"event: ", Rest/binary>>, Acc) ->
    Acc#{event => trim(Rest)};
parse_line(<<"data:", Rest/binary>>, Acc) ->
    append_data(trim(Rest), Acc);
parse_line(<<"data: ", Rest/binary>>, Acc) ->
    append_data(trim(Rest), Acc);
parse_line(<<"id:", Rest/binary>>, Acc) ->
    Acc#{id => trim(Rest)};
parse_line(<<"id: ", Rest/binary>>, Acc) ->
    Acc#{id => trim(Rest)};
parse_line(<<"retry:", Rest/binary>>, Acc) ->
    case parse_integer(trim(Rest)) of
        {ok, N} -> Acc#{retry => N};
        error -> Acc
    end;
parse_line(<<"retry: ", Rest/binary>>, Acc) ->
    case parse_integer(trim(Rest)) of
        {ok, N} -> Acc#{retry => N};
        error -> Acc
    end;
parse_line(_Line, Acc) ->
    %% 未知字段，忽略
    Acc.

%% @doc 从 SSE 事件数据中提取 JSON
%%
%% 尝试将事件数据解析为 JSON。
%%
%% @param Event SSE 事件
%% @returns {ok, ParsedJson} | {error, Reason} | {raw, Data}
-spec extract_json_data(sse_event()) -> {ok, term()} | {error, term()} | {raw, binary()}.
extract_json_data(#{data := Data}) when is_binary(Data) ->
    case Data of
        <<"[DONE]">> ->
            {ok, done};
        _ ->
            try
                {ok, jsx:decode(Data, [return_maps])}
            catch
                _:_ -> {raw, Data}
            end
    end;
extract_json_data(#{data := Data}) ->
    {ok, Data};
extract_json_data(_) ->
    {error, no_data}.

%%====================================================================
%% 编码 API
%%====================================================================

%% @doc 编码 SSE 事件（只有数据）
%%
%% @param Data 事件数据（binary 或 map/list 会被 JSON 编码）
%% @returns iolist
-spec encode_event(term()) -> iolist().
encode_event(Data) ->
    encode_event(<<"message">>, Data).

%% @doc 编码 SSE 事件（指定类型）
%%
%% @param EventType 事件类型
%% @param Data 事件数据
%% @returns iolist
-spec encode_event(binary(), term()) -> iolist().
encode_event(EventType, Data) ->
    [
        <<"event: ">>, EventType, <<"\n">>,
        encode_data(Data),
        <<"\n">>
    ].

%% @doc 编码 SSE 事件（指定类型和 ID）
%%
%% @param EventType 事件类型
%% @param Data 事件数据
%% @param Id 事件 ID
%% @returns iolist
-spec encode_event(binary(), term(), binary() | integer()) -> iolist().
encode_event(EventType, Data, Id) ->
    [
        encode_id(Id),
        <<"event: ">>, EventType, <<"\n">>,
        encode_data(Data),
        <<"\n">>
    ].

%% @doc 编码 SSE data 字段
%%
%% 如果数据包含换行符，会分成多行。
%%
%% @param Data 数据（binary/map/list）
%% @returns iolist
-spec encode_data(term()) -> iolist().
encode_data(Data) when is_binary(Data) ->
    %% 处理多行数据
    Lines = binary:split(Data, <<"\n">>, [global]),
    [[<<"data: ">>, Line, <<"\n">>] || Line <- Lines];
encode_data(Data) when is_map(Data); is_list(Data) ->
    %% JSON 编码
    JsonBin = jsx:encode(Data),
    [<<"data: ">>, JsonBin, <<"\n">>];
encode_data(Data) ->
    %% 其他类型转为 binary
    [<<"data: ">>, to_binary(Data), <<"\n">>].

%% @doc 编码 SSE 注释
%%
%% 注释以冒号开头，可用于心跳。
%%
%% @param Comment 注释内容
%% @returns iolist
-spec encode_comment(binary()) -> iolist().
encode_comment(Comment) ->
    [<<": ">>, Comment, <<"\n\n">>].

%% @doc 编码 SSE retry 字段
%%
%% @param Milliseconds 重连间隔（毫秒）
%% @returns iolist
-spec encode_retry(pos_integer()) -> iolist().
encode_retry(Milliseconds) when is_integer(Milliseconds), Milliseconds > 0 ->
    [<<"retry: ">>, integer_to_binary(Milliseconds), <<"\n">>].

%% @doc 编码 SSE id 字段
%%
%% @param Id 事件 ID
%% @returns iolist
-spec encode_id(binary() | integer()) -> iolist().
encode_id(Id) when is_binary(Id) ->
    [<<"id: ">>, Id, <<"\n">>];
encode_id(Id) when is_integer(Id) ->
    [<<"id: ">>, integer_to_binary(Id), <<"\n">>].

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 解析单个 SSE 事件块
-spec parse_event(binary()) -> sse_event().
parse_event(EventData) ->
    Lines = binary:split(EventData, <<"\n">>, [global]),
    lists:foldl(fun parse_line/2, #{}, Lines).

%% @private 分割列表，取出最后一个元素
-spec split_last([binary()]) -> {binary(), [binary()]}.
split_last([]) ->
    {<<>>, []};
split_last([Single]) ->
    {Single, []};
split_last(List) ->
    [Last | RevRest] = lists:reverse(List),
    {Last, lists:reverse(RevRest)}.

%% @private 追加 data 到累加器
-spec append_data(binary(), map()) -> map().
append_data(NewData, #{data := Existing} = Acc) ->
    %% 多行 data，用换行符连接
    Acc#{data => <<Existing/binary, "\n", NewData/binary>>};
append_data(NewData, Acc) ->
    Acc#{data => NewData}.

%% @private 去除首尾空白
-spec trim(binary()) -> binary().
trim(Bin) ->
    %% 简单实现：只去除首部空格
    case Bin of
        <<" ", Rest/binary>> -> trim(Rest);
        <<"\t", Rest/binary>> -> trim(Rest);
        _ -> trim_trailing(Bin)
    end.

%% @private 去除尾部空白
-spec trim_trailing(binary()) -> binary().
trim_trailing(<<>>) ->
    <<>>;
trim_trailing(Bin) ->
    Size = byte_size(Bin),
    case binary:last(Bin) of
        $\r -> trim_trailing(binary:part(Bin, 0, Size - 1));
        $\n -> trim_trailing(binary:part(Bin, 0, Size - 1));
        $\s -> trim_trailing(binary:part(Bin, 0, Size - 1));
        $\t -> trim_trailing(binary:part(Bin, 0, Size - 1));
        _ -> Bin
    end.

%% @private 解析整数
-spec parse_integer(binary()) -> {ok, integer()} | error.
parse_integer(Bin) ->
    try
        {ok, binary_to_integer(Bin)}
    catch
        _:_ -> error
    end.

%% @private 转为 binary
-spec to_binary(term()) -> binary().
to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_binary(I) when is_integer(I) -> integer_to_binary(I);
to_binary(F) when is_float(F) -> float_to_binary(F, [{decimals, 6}, compact]);
to_binary(T) -> iolist_to_binary(io_lib:format("~p", [T])).
