%%%-------------------------------------------------------------------
%%% @doc 基础工具处理器模块
%%%
%%% 实现基础工具的处理器函数：
%%% - handle_snapshot: 创建快照
%%% - handle_get_trace: 获取执行轨迹
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_base_handlers).

%%====================================================================
%% 导出 API
%%====================================================================

-export([
    handle_snapshot/2,
    handle_get_trace/2
]).

%%====================================================================
%% 处理器实现
%%====================================================================

%% @doc 处理 snapshot 工具调用
%%
%% 创建快照记录，包含时间戳信息。
%% 如果启用了存储，会持久化当前状态。
-spec handle_snapshot(map(), graph_state:state()) -> map().
handle_snapshot(Args, State) ->
    Label = maps:get(<<"label">>, Args),
    Notes = maps:get(<<"notes">>, Args, <<>>),
    Timestamp = erlang:system_time(millisecond),

    %% 尝试保存到存储（如果启用）
    SaveResult = case graph_state:get(State, storage, undefined) of
        undefined ->
            {error, storage_not_enabled};
        Storage ->
            Meta = #{
                label => Label,
                notes => Notes,
                timestamp => Timestamp,
                source => tool
            },
            beamai_deepagent_snapshot:save(Meta, State)
    end,

    %% 构建响应
    Response = #{
        action => snapshot,
        label => Label,
        notes => Notes,
        timestamp => Timestamp
    },

    %% 如果保存成功，添加 snapshot_id
    case SaveResult of
        {ok, CpId} ->
            Response#{snapshot_id => CpId, saved => true};
        {error, _Reason} ->
            Response#{saved => false, note => <<"Storage not enabled">>}
    end.

%% @doc 处理 get_trace 工具调用
%%
%% 获取最近的执行轨迹记录，用于调试和分析。
-spec handle_get_trace(map(), map()) -> map().
handle_get_trace(Args, State) ->
    Limit = maps:get(<<"limit">>, Args, 10),
    Trace = graph_state:get(State, trace, beamai_deepagent_trace:new()),
    RecentTrace = beamai_deepagent_trace:get_recent(Trace, Limit),
    #{
        action => get_trace,
        entries => RecentTrace,
        count => length(RecentTrace)
    }.
