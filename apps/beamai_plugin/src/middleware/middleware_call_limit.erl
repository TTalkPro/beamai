%%%-------------------------------------------------------------------
%%% @doc 调用限制中间件
%%%
%%% 在执行过程中限制各类调用次数，防止无限循环或资源耗尽：
%%% - 模型/对话调用次数限制
%%% - 工具/函数调用次数限制（总数和每轮）
%%% - 迭代次数限制
%%%
%%% 使用的钩子：
%%% - pre_chat: 检查模型调用次数和迭代次数限制
%%% - pre_invocation: 检查工具调用次数限制
%%% - post_invocation: 更新工具调用计数器
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_call_limit).

-behaviour(beamai_middleware).

-export([init/1, pre_chat/2, pre_invocation/2, post_invocation/2]).

%%====================================================================
%% 中间件回调函数
%%====================================================================

%% @doc 初始化计数器和限制配置。
%%
%% 根据传入的配置选项设置各项限制阈值，并将所有计数器初始化为 0。
%%
%% 配置参数说明：
%% - max_model_calls: 模型最大调用次数（默认 20）
%% - max_tool_calls: 工具最大调用总次数（默认 50）
%% - max_tool_calls_per_turn: 每轮最大工具调用次数（默认 10）
%% - max_iterations: 最大迭代次数（默认 15）
%% - on_limit_exceeded: 超限处理方式，halt（中止）或 warn_and_continue（警告并继续）
%%
%% @param Opts 配置选项映射
%% @returns 包含限制阈值和计数器的初始状态映射
-spec init(map()) -> map().
init(Opts) ->
    #{
        max_model_calls => maps:get(max_model_calls, Opts, 20),
        max_tool_calls => maps:get(max_tool_calls, Opts, 50),
        max_tool_calls_per_turn => maps:get(max_tool_calls_per_turn, Opts, 10),
        max_iterations => maps:get(max_iterations, Opts, 15),
        on_limit_exceeded => maps:get(on_limit_exceeded, Opts, halt),
        %% 计数器，存储在中间件状态中
        model_call_count => 0,
        tool_call_count => 0,
        iteration_count => 0,
        current_turn_tool_calls => 0
    }.

%% @doc 检查模型调用次数和迭代次数限制。
%%
%% 在每次 LLM 调用之前执行，检查当前的模型调用次数和迭代次数
%% 是否已达到配置的上限。如果超限，根据 on_limit_exceeded 配置
%% 决定是中止执行还是仅发出警告。
%%
%% @param _FilterCtx 过滤器上下文（本钩子中未使用）
%% @param MwState 当前中间件状态，包含计数器和限制值
%% @returns ok（未超限时）或 {error, ...}（超限且配置为 halt 时）
-spec pre_chat(map(), map()) -> beamai_middleware:middleware_result().
pre_chat(_FilterCtx, MwState) ->
    #{max_model_calls := MaxCalls,
      max_iterations := MaxIters,
      model_call_count := ModelCount,
      iteration_count := IterCount,
      on_limit_exceeded := Action} = MwState,

    Checks = [
        {model_calls, ModelCount, MaxCalls},
        {iterations, IterCount, MaxIters}
    ],

    case check_limits(Checks) of
        ok ->
            %% 未超限，增加计数器（通过 continue 更新状态）
            ok;
        {exceeded, Type, Count, Max} ->
            handle_exceeded(Type, Count, Max, Action)
    end.

%% @doc 检查工具调用次数限制。
%%
%% 在每次工具执行之前调用，检查工具调用总次数和当前轮次的
%% 工具调用次数是否超限。
%%
%% @param _FilterCtx 过滤器上下文（本钩子中未使用）
%% @param MwState 当前中间件状态，包含工具调用计数器和限制值
%% @returns ok（未超限时）或 {error, ...}（超限且配置为 halt 时）
-spec pre_invocation(map(), map()) -> beamai_middleware:middleware_result().
pre_invocation(_FilterCtx, MwState) ->
    #{max_tool_calls := MaxToolCalls,
      max_tool_calls_per_turn := MaxPerTurn,
      tool_call_count := TotalCount,
      current_turn_tool_calls := TurnCount,
      on_limit_exceeded := Action} = MwState,

    Checks = [
        {tool_calls_per_turn, TurnCount, MaxPerTurn},
        {tool_calls, TotalCount, MaxToolCalls}
    ],

    case check_limits(Checks) of
        ok -> ok;
        {exceeded, Type, Count, Max} ->
            handle_exceeded(Type, Count, Max, Action)
    end.

%% @doc 更新工具调用计数器。
%%
%% 在工具执行完成后调用，将过滤器上下文原样传递继续执行。
%% 计数器的实际更新由外部状态管理机制负责。
%%
%% @param FilterCtx 过滤器上下文，包含工具执行结果
%% @param _MwState 当前中间件状态（本钩子中未使用）
%% @returns {continue, FilterCtx} 继续执行并传递上下文
-spec post_invocation(map(), map()) -> beamai_middleware:middleware_result().
post_invocation(FilterCtx, _MwState) ->
    %% 直接继续执行，计数器由外部管理
    {continue, FilterCtx}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 检查限制列表
%% 依次检查每个 {类型, 当前值, 最大值} 元组，
%% 如果发现当前值已达到或超过最大值，立即返回超限信息。
%% 所有检查通过则返回 ok。
check_limits([]) -> ok;
check_limits([{Type, Count, Max} | Rest]) ->
    case Count >= Max of
        true -> {exceeded, Type, Count, Max};
        false -> check_limits(Rest)
    end.

%% @private 处理超限情况
%% 根据 Action 参数决定处理方式：
%% - halt: 返回错误，中止执行链
%% - warn_and_continue: 记录警告日志，继续执行
handle_exceeded(Type, Count, Max, halt) ->
    {error, {limit_exceeded, #{
        type => Type,
        count => Count,
        max => Max,
        message => format_limit_message(Type, Count, Max)
    }}};
handle_exceeded(Type, Count, Max, warn_and_continue) ->
    logger:warning("~s", [format_limit_message(Type, Count, Max)]),
    ok.

%% @private 格式化超限提示信息
%% 根据不同的限制类型生成对应的可读错误消息。
format_limit_message(model_calls, Count, Max) ->
    iolist_to_binary(io_lib:format("Model call limit exceeded (~p/~p)", [Count, Max]));
format_limit_message(tool_calls, Count, Max) ->
    iolist_to_binary(io_lib:format("Tool call limit exceeded (~p/~p)", [Count, Max]));
format_limit_message(tool_calls_per_turn, Count, Max) ->
    iolist_to_binary(io_lib:format("Per-turn tool call limit exceeded (~p/~p)", [Count, Max]));
format_limit_message(iterations, Count, Max) ->
    iolist_to_binary(io_lib:format("Iteration limit exceeded (~p/~p)", [Count, Max])).
