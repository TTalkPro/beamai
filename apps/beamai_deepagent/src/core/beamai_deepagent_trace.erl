%%%-------------------------------------------------------------------
%%% @doc 深度 Agent 执行轨迹模块
%%%
%%% 提供执行轨迹管理的纯函数。
%%% 轨迹用于记录 Agent 执行过程中的关键事件。
%%%
%%% 功能:
%%%   - 创建新轨迹
%%%   - 添加轨迹条目
%%%   - 获取最近条目
%%%   - 格式化输出
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_trace).

-export([new/0, add/3, get_recent/2, format/1]).

%% 类型定义
-type entry() :: #{
    timestamp := integer(),  %% 时间戳（毫秒）
    type := atom(),          %% 事件类型
    data := term()           %% 事件数据
}.
-opaque t() :: [entry()].

-export_type([t/0, entry/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 创建新的空轨迹
-spec new() -> t().
new() -> [].

%% @doc 添加轨迹条目
%% 新条目添加到列表头部（最新在前）
-spec add(t(), atom(), term()) -> t().
add(Trace, Type, Data) ->
    Entry = #{
        timestamp => erlang:system_time(millisecond),
        type => Type,
        data => Data
    },
    [Entry | Trace].

%% @doc 获取最近 N 条轨迹
-spec get_recent(t(), pos_integer()) -> t().
get_recent(Trace, N) ->
    lists:sublist(Trace, N).

%% @doc 格式化轨迹为可读字符串
-spec format(t()) -> binary().
format(Trace) ->
    Lines = [format_entry(E) || E <- lists:reverse(Trace)],
    iolist_to_binary(lists:join(<<"\n">>, Lines)).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 格式化单条轨迹
-spec format_entry(entry()) -> binary().
format_entry(#{type := Type, data := Data}) ->
    TypeBin = atom_to_binary(Type),
    DataBin = truncate(format_data(Data), 100),
    <<"- ", TypeBin/binary, ": ", DataBin/binary>>.

%% @private 格式化数据
-spec format_data(term()) -> binary().
format_data(D) when is_binary(D) -> D;
format_data(D) when is_map(D) -> jsx:encode(D);
format_data(D) -> iolist_to_binary(io_lib:format("~p", [D])).

%% @private 截断过长字符串
-spec truncate(binary(), pos_integer()) -> binary().
truncate(Bin, Max) when byte_size(Bin) =< Max -> Bin;
truncate(Bin, Max) -> <<(binary:part(Bin, 0, Max - 3))/binary, "...">>.
