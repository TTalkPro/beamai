%%%-------------------------------------------------------------------
%%% @doc Call Limit Middleware - 调用限制
%%%
%%% 限制 Agent 执行过程中的各种调用次数：
%%% - 模型调用次数限制
%%% - 工具调用次数限制
%%% - 总迭代次数限制
%%%
%%% == 配置选项 ==
%%%
%%% ```erlang
%%% {middleware_call_limit, #{
%%%     %% 模型调用限制
%%%     max_model_calls => 20,         %% 最大模型调用次数（默认 20）
%%%
%%%     %% 工具调用限制
%%%     max_tool_calls => 50,          %% 最大工具调用总次数（默认 50）
%%%     max_tool_calls_per_turn => 10, %% 每轮最大工具调用数（默认 10）
%%%
%%%     %% 迭代限制
%%%     max_iterations => 15,          %% 最大迭代次数（默认 15）
%%%
%%%     %% 超限后的行为
%%%     on_limit_exceeded => halt | warn_and_continue,
%%%
%%%     %% 自定义超限处理
%%%     limit_handler => fun(LimitType, Count, Max, State) -> ...
%%% }}
%%% ```
%%%
%%% == 计数器 ==
%%%
%%% Middleware 在图状态中维护以下计数器：
%%% - mw_model_call_count: 模型调用次数
%%% - mw_tool_call_count: 工具调用总次数
%%% - mw_iteration_count: 迭代次数
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_call_limit).

-behaviour(beamai_middleware).

%% Middleware 回调
-export([init/1, before_agent/2, before_model/2, before_tools/2, after_tools/2]).

%% 工具函数
-export([get_counts/1, reset_counts/1]).

%%====================================================================
%% 类型定义
%%====================================================================

-type limit_type() :: model_calls | tool_calls | tool_calls_per_turn | iterations.
-type on_limit_action() :: halt | warn_and_continue.

%% 内部检查结果类型
-type check_result() :: {exceeded, limit_type(), integer(), integer()}
                      | ok.

%%====================================================================
%% Middleware 回调
%%====================================================================

%% @doc 初始化 Middleware 状态
%%
%% 配置选项：
%%   - max_model_calls: 最大模型调用次数
%%   - max_tool_calls: 最大工具调用总次数
%%   - max_tool_calls_per_turn: 每轮最大工具调用数
%%   - max_iterations: 最大迭代次数
%%   - on_limit_exceeded: 超限后行为（halt | warn_and_continue）
%%   - limit_handler: 自定义超限处理函数
%%   - debug: 调试模式
-spec init(map()) -> map().
init(Opts) ->
    #{
        max_model_calls => maps:get(max_model_calls, Opts, 20),
        max_tool_calls => maps:get(max_tool_calls, Opts, 50),
        max_tool_calls_per_turn => maps:get(max_tool_calls_per_turn, Opts, 10),
        max_iterations => maps:get(max_iterations, Opts, 15),
        on_limit_exceeded => maps:get(on_limit_exceeded, Opts, halt),
        limit_handler => maps:get(limit_handler, Opts, undefined),
        debug => maps:get(debug, Opts, false)
    }.

%% @doc Agent 开始时初始化计数器
%%
%% 确保所有计数器存在于状态中。
-spec before_agent(map(), map()) -> beamai_middleware:middleware_result().
before_agent(State, _MwState) ->
    Updates = #{
        mw_model_call_count => graph:get(State, mw_model_call_count, 0),
        mw_tool_call_count => graph:get(State, mw_tool_call_count, 0),
        mw_iteration_count => graph:get(State, mw_iteration_count, 0),
        mw_current_turn_tool_calls => 0
    },
    {update, Updates}.

%% @doc 模型调用前检查限制
%%
%% 使用递归检查替代嵌套 case，降低代码复杂度。
%% 检查顺序：模型调用限制 -> 迭代限制
-spec before_model(map(), map()) -> beamai_middleware:middleware_result().
before_model(State, MwState) ->
    %% 构建检查列表
    Checks = build_model_checks(State, MwState),

    %% 执行检查
    case check_limits(Checks) of
        ok ->
            %% 所有检查通过，递增计数器
            increment_model_counters(State);
        {exceeded, Type, Count, Max} ->
            %% 检测到超限
            handle_limit_exceeded(Type, Count, Max, State, MwState)
    end.

%% @doc 工具执行前检查限制
%%
%% 检查顺序：每轮工具调用限制 -> 总工具调用限制
-spec before_tools(map(), map()) -> beamai_middleware:middleware_result().
before_tools(State, MwState) ->
    %% 构建检查列表
    Checks = build_tools_checks(State, MwState),

    %% 执行检查
    case check_limits(Checks) of
        ok ->
            ok;
        {exceeded, Type, Count, Max} ->
            handle_limit_exceeded(Type, Count, Max, State, MwState)
    end.

%% @doc 工具执行后更新计数
-spec after_tools(map(), map()) -> beamai_middleware:middleware_result().
after_tools(State, _MwState) ->
    ToolResults = graph:get(State, tool_results, []),
    ExecutedCount = length(ToolResults),

    TotalCount = graph:get(State, mw_tool_call_count, 0),
    CurrentTurnCount = graph:get(State, mw_current_turn_tool_calls, 0),

    {update, #{
        mw_tool_call_count => TotalCount + ExecutedCount,
        mw_current_turn_tool_calls => CurrentTurnCount + ExecutedCount
    }}.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 获取当前计数
-spec get_counts(map()) -> map().
get_counts(State) ->
    #{
        model_calls => graph:get(State, mw_model_call_count, 0),
        tool_calls => graph:get(State, mw_tool_call_count, 0),
        iterations => graph:get(State, mw_iteration_count, 0),
        current_turn_tool_calls => graph:get(State, mw_current_turn_tool_calls, 0)
    }.

%% @doc 重置计数（返回状态更新）
-spec reset_counts(map()) -> map().
reset_counts(_State) ->
    #{
        mw_model_call_count => 0,
        mw_tool_call_count => 0,
        mw_iteration_count => 0,
        mw_current_turn_tool_calls => 0
    }.

%%====================================================================
%% 内部函数 - 检查构建
%%====================================================================

%% @private 构建模型调用相关的检查列表
%%
%% 返回格式: [{Type, CurrentCount, MaxCount}, ...]
-spec build_model_checks(map(), map()) -> [{limit_type(), integer(), integer()}].
build_model_checks(State, MwState) ->
    #{max_model_calls := MaxCalls, max_iterations := MaxIters} = MwState,

    ModelCount = graph:get(State, mw_model_call_count, 0),
    IterCount = graph:get(State, mw_iteration_count, 0),

    [
        {model_calls, ModelCount, MaxCalls},
        {iterations, IterCount, MaxIters}
    ].

%% @private 构建工具调用相关的检查列表
-spec build_tools_checks(map(), map()) -> [{limit_type(), integer(), integer()}].
build_tools_checks(State, MwState) ->
    #{max_tool_calls := MaxToolCalls,
      max_tool_calls_per_turn := MaxPerTurn} = MwState,

    ToolCalls = graph:get(State, tool_calls, []),
    TotalToolCount = graph:get(State, mw_tool_call_count, 0),
    CurrentTurnCount = graph:get(State, mw_current_turn_tool_calls, 0),
    NewCallCount = length(ToolCalls),

    [
        {tool_calls_per_turn, CurrentTurnCount + NewCallCount, MaxPerTurn},
        {tool_calls, TotalToolCount + NewCallCount, MaxToolCalls}
    ].

%%====================================================================
%% 内部函数 - 限制检查
%%====================================================================

%% @private 检查限制列表
%%
%% 使用递归遍历检查列表，返回第一个超限的检查结果。
%% 如果所有检查都通过，返回 ok。
-spec check_limits([{limit_type(), integer(), integer()}]) -> check_result().
check_limits([]) ->
    ok;
check_limits([{Type, Count, Max} | Rest]) ->
    case Count >= Max of
        true ->
            %% 超限，立即返回
            {exceeded, Type, Count, Max};
        false ->
            %% 未超限，继续检查下一个
            check_limits(Rest)
    end.

%% @private 递增模型调用计数器
-spec increment_model_counters(map()) -> beamai_middleware:middleware_result().
increment_model_counters(State) ->
    ModelCount = graph:get(State, mw_model_call_count, 0),
    IterCount = graph:get(State, mw_iteration_count, 0),

    {update, #{
        mw_model_call_count => ModelCount + 1,
        mw_iteration_count => IterCount + 1,
        mw_current_turn_tool_calls => 0  %% 重置每轮工具计数
    }}.

%%====================================================================
%% 内部函数 - 超限处理
%%====================================================================

%% @private 处理超限情况
%%
%% 流程：
%%   1. 记录日志
%%   2. 调用自定义处理器（如果有）
%%   3. 执行默认动作
-spec handle_limit_exceeded(limit_type(), integer(), integer(), map(), map()) ->
    beamai_middleware:middleware_result().
handle_limit_exceeded(LimitType, Count, Max, State, MwState) ->
    #{on_limit_exceeded := Action,
      limit_handler := Handler,
      debug := Debug} = MwState,

    %% 记录日志
    LogMsg = io_lib:format("~p 超限: ~p/~p", [LimitType, Count, Max]),
    case Debug of
        true -> logger:info("[CallLimit] ~s", [LogMsg]);
        false -> logger:warning("~s", [LogMsg])
    end,

    %% 调用自定义处理器（如果有）
    case Handler of
        undefined ->
            handle_default_action(LimitType, Count, Max, Action);
        Handler when is_function(Handler, 4) ->
            try
                Handler(LimitType, Count, Max, State)
            catch
                _:Reason ->
                    logger:error("限制处理器异常: ~p", [Reason]),
                    handle_default_action(LimitType, Count, Max, Action)
            end
    end.

%% @private 默认超限处理
%%
%% halt: 中止执行，返回错误
%% warn_and_continue: 记录警告，继续执行
-spec handle_default_action(limit_type(), integer(), integer(), on_limit_action()) ->
    beamai_middleware:middleware_result().
handle_default_action(LimitType, Count, Max, halt) ->
    {halt, {limit_exceeded, #{
        type => LimitType,
        count => Count,
        max => Max,
        message => format_limit_message(LimitType, Count, Max)
    }}};
handle_default_action(LimitType, Count, Max, warn_and_continue) ->
    logger:warning("~s，继续执行", [format_limit_message(LimitType, Count, Max)]),
    ok.

%% @private 格式化限制消息
-spec format_limit_message(limit_type(), integer(), integer()) -> binary().
format_limit_message(model_calls, Count, Max) ->
    iolist_to_binary(io_lib:format("模型调用次数超限 (~p/~p)", [Count, Max]));
format_limit_message(tool_calls, Count, Max) ->
    iolist_to_binary(io_lib:format("工具调用总次数超限 (~p/~p)", [Count, Max]));
format_limit_message(tool_calls_per_turn, Count, Max) ->
    iolist_to_binary(io_lib:format("单轮工具调用次数超限 (~p/~p)", [Count, Max]));
format_limit_message(iterations, Count, Max) ->
    iolist_to_binary(io_lib:format("迭代次数超限 (~p/~p)", [Count, Max])).
