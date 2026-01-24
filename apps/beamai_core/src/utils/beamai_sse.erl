%%%-------------------------------------------------------------------
%%% @doc Server-Sent Events (SSE) 解析和编码工具
%%%
%%% 提供 SSE 格式的解析和编码功能，被 MCP 传输层使用。
%%%
%%% == SSE 格式 ==
%%%
%%% SSE 事件由双换行符（\n\n）分隔。
%%% 每个事件可包含以下字段：
%%% - event: 事件类型（可选，默认 "message"）
%%% - data: 事件数据
%%% - id: 事件 ID（可选）
%%% - retry: 重连间隔（可选）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_sse).

-export([
    parse/1,
    encode_event/2,
    encode_event/3
]).

%%====================================================================
%% API
%%====================================================================

%% @doc 解析 SSE 数据流
%%
%% 将原始二进制数据解析为事件列表。
%% 不完整的数据（未以双换行结尾）将作为 Remaining 返回。
%%
%% @param Data 原始 SSE 数据
%% @returns {Remaining, Events} 其中 Events 是 map 列表
-spec parse(binary()) -> {binary(), [map()]}.
parse(Data) ->
    parse_events(Data, []).

%% @doc 编码 SSE 事件（无 ID）
%%
%% @param EventType 事件类型
%% @param Data 事件数据（binary 或可 JSON 编码的 term）
%% @returns SSE 格式的二进制数据
-spec encode_event(binary(), binary() | term()) -> binary().
encode_event(EventType, Data) ->
    DataBin = ensure_binary(Data),
    <<"event: ", EventType/binary, "\ndata: ", DataBin/binary, "\n\n">>.

%% @doc 编码 SSE 事件（带 ID）
-spec encode_event(binary(), binary() | term(), binary()) -> binary().
encode_event(EventType, Data, Id) ->
    DataBin = ensure_binary(Data),
    <<"id: ", Id/binary, "\nevent: ", EventType/binary, "\ndata: ", DataBin/binary, "\n\n">>.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 解析事件流
-spec parse_events(binary(), [map()]) -> {binary(), [map()]}.
parse_events(Data, Acc) ->
    case split_event(Data) of
        {EventBlock, Rest} ->
            Event = parse_event_block(EventBlock),
            parse_events(Rest, [Event | Acc]);
        incomplete ->
            {Data, lists:reverse(Acc)}
    end.

%% @private 从数据中分割出一个完整事件
-spec split_event(binary()) -> {binary(), binary()} | incomplete.
split_event(Data) ->
    case binary:split(Data, <<"\n\n">>) of
        [_] ->
            incomplete;
        [EventBlock, Rest] ->
            {EventBlock, Rest}
    end.

%% @private 解析单个事件块
-spec parse_event_block(binary()) -> map().
parse_event_block(Block) ->
    Lines = binary:split(Block, <<"\n">>, [global]),
    parse_event_lines(Lines, #{}).

%% @private 解析事件行
-spec parse_event_lines([binary()], map()) -> map().
parse_event_lines([], Acc) ->
    Acc;
parse_event_lines([Line | Rest], Acc) ->
    NewAcc = parse_event_line(Line, Acc),
    parse_event_lines(Rest, NewAcc).

%% @private 解析单行
-spec parse_event_line(binary(), map()) -> map().
parse_event_line(<<"event: ", Value/binary>>, Acc) ->
    Acc#{event => Value};
parse_event_line(<<"event:", Value/binary>>, Acc) ->
    Acc#{event => string:trim(Value)};
parse_event_line(<<"data: ", Value/binary>>, Acc) ->
    %% data 可以多行，用换行拼接
    case maps:get(data, Acc, undefined) of
        undefined -> Acc#{data => Value};
        Existing -> Acc#{data => <<Existing/binary, "\n", Value/binary>>}
    end;
parse_event_line(<<"data:", Value/binary>>, Acc) ->
    TrimmedValue = string:trim(Value),
    case maps:get(data, Acc, undefined) of
        undefined -> Acc#{data => TrimmedValue};
        Existing -> Acc#{data => <<Existing/binary, "\n", TrimmedValue/binary>>}
    end;
parse_event_line(<<"id: ", Value/binary>>, Acc) ->
    Acc#{id => Value};
parse_event_line(<<"id:", Value/binary>>, Acc) ->
    Acc#{id => string:trim(Value)};
parse_event_line(<<"retry: ", Value/binary>>, Acc) ->
    case catch binary_to_integer(Value) of
        N when is_integer(N) -> Acc#{retry => N};
        _ -> Acc
    end;
parse_event_line(<<": ", _/binary>>, Acc) ->
    %% 注释行，忽略
    Acc;
parse_event_line(<<":", _/binary>>, Acc) ->
    %% 注释行，忽略
    Acc;
parse_event_line(<<>>, Acc) ->
    %% 空行，忽略
    Acc;
parse_event_line(_Line, Acc) ->
    %% 未知格式，忽略
    Acc.

%% @private 确保数据为 binary
-spec ensure_binary(binary() | term()) -> binary().
ensure_binary(Data) when is_binary(Data) -> Data;
ensure_binary(Data) ->
    try
        jsx:encode(Data, [])
    catch
        _:_ -> iolist_to_binary(io_lib:format("~p", [Data]))
    end.
