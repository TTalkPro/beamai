%%%-------------------------------------------------------------------
%%% @doc Agent Middleware 运行器
%%%
%%% 负责 Middleware 链的管理和执行：
%%% - 初始化 Middleware 实例
%%% - 按优先级排序执行
%%% - 处理返回结果
%%% - 状态更新与流程控制
%%%
%%% == 执行顺序 ==
%%%
%%% Middleware 按 priority 升序执行（数值越小越先执行）。
%%% 默认优先级为 100。
%%%
%%% == 短路行为 ==
%%%
%%% 当某个 Middleware 返回 `{goto, _}`, `{halt, _}` 或 `{interrupt, _}` 时，
%%% 后续 Middleware 不再执行（短路）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_middleware_runner).

%% API 导出
-export([
    init/1,
    run_hook/3,
    run_hook/4,
    get_middleware_state/2,
    set_middleware_state/3
]).

%% 类型导出
-export_type([middleware_chain/0, run_result/0]).

%%====================================================================
%% 类型定义
%%====================================================================

%% Middleware 链（已初始化的 Middleware 列表）
-type middleware_chain() :: [beamai_middleware:middleware()].

%% 钩子执行结果
-type run_result() ::
    {ok, map()} |                          %% 成功，返回更新后的状态
    {goto, beamai_middleware:goto_target(), map()} |  %% 跳转
    {halt, term()} |                       %% 中止
    {interrupt, beamai_middleware:interrupt_action(), map()}.  %% 中断

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 初始化 Middleware 链
%%
%% 输入格式：
%% ```
%% [
%%     {middleware_module, Opts},
%%     {middleware_module, Opts, Priority},
%%     middleware_module  %% 使用默认选项和优先级
%% ]
%% ```
%%
%% 返回已初始化并按优先级排序的 Middleware 链。
-spec init([term()]) -> middleware_chain().
init(MiddlewareSpecs) ->
    Middlewares = lists:map(fun init_single/1, MiddlewareSpecs),
    %% 按优先级排序（升序）
    lists:sort(fun(#{priority := P1}, #{priority := P2}) -> P1 =< P2 end, Middlewares).

%% @doc 执行钩子（不带图状态选项）
-spec run_hook(atom(), map(), middleware_chain()) -> run_result().
run_hook(HookName, GraphState, Middlewares) ->
    run_hook(HookName, GraphState, Middlewares, #{}).

%% @doc 执行钩子
%%
%% 按顺序执行所有 Middleware 的指定钩子。
%% 累积状态更新，遇到控制流指令则短路。
%%
%% Opts:
%% - accumulate_updates: 是否累积所有 update（默认 true）
-spec run_hook(atom(), map(), middleware_chain(), map()) -> run_result().
run_hook(HookName, GraphState, Middlewares, Opts) ->
    AccumulateUpdates = maps:get(accumulate_updates, Opts, true),
    run_hook_loop(HookName, GraphState, Middlewares, #{}, AccumulateUpdates).

%% @doc 获取指定 Middleware 的内部状态
-spec get_middleware_state(module(), middleware_chain()) ->
    {ok, beamai_middleware:middleware_state()} | {error, not_found}.
get_middleware_state(Module, Middlewares) ->
    case lists:keyfind(Module, 1, [{M, Mw} || #{module := M} = Mw <- Middlewares]) of
        {Module, #{state := State}} -> {ok, State};
        false -> {error, not_found}
    end.

%% @doc 设置指定 Middleware 的内部状态
-spec set_middleware_state(module(), beamai_middleware:middleware_state(), middleware_chain()) ->
    middleware_chain().
set_middleware_state(Module, NewState, Middlewares) ->
    lists:map(fun(#{module := M} = Mw) when M =:= Module ->
                    Mw#{state => NewState};
                 (Mw) -> Mw
              end, Middlewares).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 初始化单个 Middleware
-spec init_single(term()) -> beamai_middleware:middleware().
init_single({Module, Opts, Priority}) when is_atom(Module), is_map(Opts), is_integer(Priority) ->
    State = call_init(Module, Opts),
    #{module => Module, state => State, priority => Priority};
init_single({Module, Opts}) when is_atom(Module), is_map(Opts) ->
    State = call_init(Module, Opts),
    #{module => Module, state => State, priority => 100};
init_single(Module) when is_atom(Module) ->
    State = call_init(Module, #{}),
    #{module => Module, state => State, priority => 100}.

%% @private 调用 Middleware 的 init 回调
-spec call_init(module(), map()) -> beamai_middleware:middleware_state().
call_init(Module, Opts) ->
    case erlang:function_exported(Module, init, 1) of
        true -> Module:init(Opts);
        false -> Opts  %% 没有 init，直接使用 Opts 作为状态
    end.

%% @private 循环执行钩子
-spec run_hook_loop(atom(), map(), middleware_chain(), map(), boolean()) -> run_result().
run_hook_loop(_HookName, GraphState, [], AccumulatedUpdates, _Accumulate) ->
    %% 所有 Middleware 执行完毕，合并更新
    FinalState = apply_updates(GraphState, AccumulatedUpdates),
    {ok, FinalState};
run_hook_loop(HookName, GraphState, [Mw | Rest], AccumulatedUpdates, Accumulate) ->
    #{module := Module, state := MwState} = Mw,
    case call_hook(Module, HookName, GraphState, MwState) of
        ok ->
            %% 无修改，继续
            run_hook_loop(HookName, GraphState, Rest, AccumulatedUpdates, Accumulate);

        {update, Updates} when is_map(Updates) ->
            %% 状态更新
            NewAccumulated = case Accumulate of
                true -> maps:merge(AccumulatedUpdates, Updates);
                false -> Updates
            end,
            NewGraphState = apply_updates(GraphState, Updates),
            run_hook_loop(HookName, NewGraphState, Rest, NewAccumulated, Accumulate);

        {goto, Target} ->
            %% 跳转，短路后续 Middleware
            FinalState = apply_updates(GraphState, AccumulatedUpdates),
            {goto, Target, FinalState};

        {update_goto, Updates, Target} when is_map(Updates) ->
            %% 更新并跳转
            AllUpdates = maps:merge(AccumulatedUpdates, Updates),
            FinalState = apply_updates(GraphState, AllUpdates),
            {goto, Target, FinalState};

        {halt, Reason} ->
            %% 中止执行
            {halt, Reason};

        {interrupt, Action} when is_map(Action) ->
            %% 中断等待确认
            FinalState = apply_updates(GraphState, AccumulatedUpdates),
            {interrupt, Action, FinalState};

        Other ->
            %% 未知返回值，记录警告并继续
            logger:warning("Middleware ~p:~p 返回未知结果: ~p", [Module, HookName, Other]),
            run_hook_loop(HookName, GraphState, Rest, AccumulatedUpdates, Accumulate)
    end.

%% @private 调用钩子函数
-spec call_hook(module(), atom(), map(), beamai_middleware:middleware_state()) ->
    beamai_middleware:middleware_result().
call_hook(Module, HookName, GraphState, MwState) ->
    case erlang:function_exported(Module, HookName, 2) of
        true ->
            try
                Module:HookName(GraphState, MwState)
            catch
                Class:Reason:Stack ->
                    logger:error("Middleware ~p:~p 执行异常: ~p:~p~n堆栈: ~p",
                                [Module, HookName, Class, Reason, Stack]),
                    ok  %% 异常时继续执行
            end;
        false ->
            ok  %% 钩子未实现，跳过
    end.

%% @private 应用状态更新
-spec apply_updates(map(), map()) -> map().
apply_updates(GraphState, Updates) when map_size(Updates) =:= 0 ->
    GraphState;
apply_updates(GraphState, Updates) ->
    %% 使用 graph:set 逐个更新，保持图状态的完整性
    maps:fold(fun(Key, Value, State) ->
        graph:set(State, Key, Value)
    end, GraphState, Updates).
