%%%-------------------------------------------------------------------
%%% @doc 中间件运行器与 Filter 桥接
%%%
%%% 本模块负责管理中间件链，并将其转换为 kernel filter 定义：
%%% - init/1: 从配置规格列表初始化中间件链
%%% - to_filters/1: 将中间件链转换为 kernel filter 定义
%%% - run_hook/3: 直接在中间件链上执行指定钩子
%%% - get_middleware_state/2: 获取指定模块的中间件状态
%%% - set_middleware_state/3: 设置指定模块的中间件状态
%%%
%%% 使用进程字典存储每条中间件链的状态信息。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_middleware_runner).

-export([
    init/1,
    to_filters/1,
    run_hook/3,
    get_middleware_state/2,
    set_middleware_state/3
]).

-export_type([middleware_chain/0]).

%%====================================================================
%% 类型定义
%%====================================================================

%% @type middleware_chain() :: [beamai_middleware:middleware()].
%% 中间件链类型，为按优先级排序的中间件列表。
%% 优先级数值越小，越先执行。
-type middleware_chain() :: [beamai_middleware:middleware()].

%%====================================================================
%% 公共 API
%%====================================================================

%% @doc 从配置规格列表初始化中间件链。
%%
%% 支持以下输入格式：
%% - {Module, Opts}: 指定模块和配置选项，使用默认优先级 100
%% - {Module, Opts, Priority}: 指定模块、配置选项和优先级
%% - Module: 仅指定模块名，使用空配置和默认优先级
%%
%% 初始化后的中间件链按优先级升序排列（优先级越小越先执行）。
%%
%% @param MiddlewareSpecs 中间件规格列表
%% @returns 按优先级排序的中间件链
-spec init([term()]) -> middleware_chain().
init(MiddlewareSpecs) ->
    Middlewares = lists:map(fun init_single/1, MiddlewareSpecs),
    lists:sort(fun(#{priority := P1}, #{priority := P2}) -> P1 =< P2 end, Middlewares).

%% @doc 将中间件链转换为 kernel filter 定义。
%%
%% 遍历四种钩子类型（pre_chat、post_chat、pre_invocation、post_invocation），
%% 为每种钩子生成对应的 filter_def()。中间件状态通过闭包捕获并保存在
%% 进程字典中，以便在 filter 执行时访问最新状态。
%%
%% @param Chain 已初始化的中间件链
%% @returns kernel filter 定义列表
-spec to_filters(middleware_chain()) -> [beamai_filter:filter_def()].
to_filters(Chain) ->
    %% 将中间件链存储到进程字典中，用于状态管理
    ChainRef = make_ref(),
    put({mw_chain, ChainRef}, Chain),
    Hooks = [pre_chat, post_chat, pre_invocation, post_invocation],
    lists:flatmap(fun(HookName) ->
        build_filters_for_hook(HookName, Chain, ChainRef)
    end, Hooks).

%% @doc 直接在中间件链上执行指定钩子。
%%
%% 不经过 filter 系统，直接遍历中间件链并依次调用指定的钩子函数。
%% 适用于需要在 filter 系统外部手动触发中间件逻辑的场景。
%%
%% @param HookName 钩子名称（pre_chat | post_chat | pre_invocation | post_invocation）
%% @param FilterCtx 过滤器上下文映射
%% @param Chain 中间件链
%% @returns filter 处理结果
-spec run_hook(beamai_middleware:hook_name(), map(), middleware_chain()) ->
    beamai_filter:filter_result().
run_hook(HookName, FilterCtx, Chain) ->
    run_hook_loop(HookName, FilterCtx, Chain).

%% @doc 获取指定模块的中间件状态。
%%
%% 在中间件链中查找指定模块，返回其当前状态。
%% 如果模块不在链中，返回 {error, not_found}。
%%
%% @param Module 目标中间件模块名
%% @param Middlewares 中间件链
%% @returns {ok, State} 或 {error, not_found}
-spec get_middleware_state(module(), middleware_chain()) ->
    {ok, beamai_middleware:middleware_state()} | {error, not_found}.
get_middleware_state(Module, Middlewares) ->
    case [Mw || #{module := M} = Mw <- Middlewares, M =:= Module] of
        [#{state := State} | _] -> {ok, State};
        [] -> {error, not_found}
    end.

%% @doc 设置指定模块的中间件状态。
%%
%% 在中间件链中找到指定模块并更新其状态，返回更新后的完整中间件链。
%% 如果模块不存在于链中，则返回原链不做修改。
%%
%% @param Module 目标中间件模块名
%% @param NewState 新的中间件状态
%% @param Middlewares 当前中间件链
%% @returns 更新后的中间件链
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

%% @private 初始化单个中间件规格
%% 支持三种格式：{Module, Opts, Priority}、{Module, Opts}、Module
init_single({Module, Opts, Priority}) when is_atom(Module), is_map(Opts), is_integer(Priority) ->
    State = call_init(Module, Opts),
    #{module => Module, state => State, priority => Priority};
init_single({Module, Opts}) when is_atom(Module), is_map(Opts) ->
    State = call_init(Module, Opts),
    #{module => Module, state => State, priority => 100};
init_single(Module) when is_atom(Module) ->
    State = call_init(Module, #{}),
    #{module => Module, state => State, priority => 100}.

%% @private 调用中间件模块的 init/1 回调
%% 如果模块未导出 init/1，则直接使用 Opts 作为初始状态
call_init(Module, Opts) ->
    code:ensure_loaded(Module),
    case erlang:function_exported(Module, init, 1) of
        true -> Module:init(Opts);
        false -> Opts
    end.

%% @private 为指定钩子构建 filter 定义列表
%% 检查中间件链中哪些模块实现了该钩子，然后创建一个聚合 filter
%% 该 filter 在执行时会依次调用所有实现了该钩子的中间件
build_filters_for_hook(HookName, Chain, ChainRef) ->
    FilterType = hook_to_filter_type(HookName),
    %% 检查哪些中间件实现了此钩子
    _ = [code:ensure_loaded(M) || #{module := M} <- Chain],
    ActiveMws = [Mw || #{module := M} = Mw <- Chain,
                        erlang:function_exported(M, HookName, 2)],
    case ActiveMws of
        [] -> [];
        _ ->
            %% 创建一个聚合 filter，运行该钩子下的所有中间件
            FilterName = iolist_to_binary(io_lib:format("middleware_~s", [HookName])),
            Priority = min_priority(ActiveMws),
            Handler = fun(FilterCtx) ->
                %% 获取当前中间件链状态（可能已被之前的调用更新）
                CurrentChain = case get({mw_chain, ChainRef}) of
                    undefined -> Chain;
                    C -> C
                end,
                Result = run_hook_loop(HookName, FilterCtx, CurrentChain),
                %% 返回钩子执行结果
                Result
            end,
            [beamai_filter:new(FilterName, FilterType, Handler, Priority)]
    end.

%% @private 将钩子名称映射到对应的 filter 类型
hook_to_filter_type(pre_chat) -> pre_chat;
hook_to_filter_type(post_chat) -> post_chat;
hook_to_filter_type(pre_invocation) -> pre_invocation;
hook_to_filter_type(post_invocation) -> post_invocation.

%% @private 获取中间件列表中的最小优先级值
%% 用于设置聚合 filter 的优先级，确保其在适当的时机执行
min_priority(Mws) ->
    lists:min([maps:get(priority, Mw, 100) || Mw <- Mws]).

%% @private 中间件链钩子执行循环
%% 依次调用链中每个中间件的指定钩子，根据返回值决定是否继续执行：
%% - ok: 继续执行下一个中间件
%% - {continue, NewCtx}: 使用更新后的上下文继续执行
%% - {skip, Value}: 立即中止链的执行，返回跳过结果
%% - {error, Reason}: 立即中止链的执行，返回错误
%% - 其他: 记录警告日志并继续执行
run_hook_loop(_HookName, FilterCtx, []) ->
    {continue, FilterCtx};
run_hook_loop(HookName, FilterCtx, [#{module := Module, state := MwState} | Rest]) ->
    case call_hook(Module, HookName, FilterCtx, MwState) of
        ok ->
            run_hook_loop(HookName, FilterCtx, Rest);
        {continue, NewCtx} when is_map(NewCtx) ->
            run_hook_loop(HookName, NewCtx, Rest);
        {skip, Value} ->
            {skip, Value};
        {error, Reason} ->
            {error, Reason};
        Other ->
            logger:warning("Middleware ~p:~p returned unknown: ~p", [Module, HookName, Other]),
            run_hook_loop(HookName, FilterCtx, Rest)
    end.

%% @private 安全调用中间件钩子函数
%% 先检查模块是否导出了该钩子函数，如果是则调用它。
%% 使用 try-catch 捕获异常，防止单个中间件的错误影响整条链的执行。
call_hook(Module, HookName, FilterCtx, MwState) ->
    case erlang:function_exported(Module, HookName, 2) of
        true ->
            try
                Module:HookName(FilterCtx, MwState)
            catch
                Class:Reason:Stack ->
                    logger:error("Middleware ~p:~p error: ~p:~p~nStack: ~p",
                                [Module, HookName, Class, Reason, Stack]),
                    ok
            end;
        false ->
            ok
    end.
