%%%-------------------------------------------------------------------
%%% @doc Agent 状态构建与 Kernel 集成
%%%
%%% 负责 agent 生命周期中的状态初始化工作：
%%%   - 从用户 Config 构建完整的 agent_state map
%%%   - 构建或接收 kernel（LLM 服务 + 插件 + 中间件）
%%%   - 注入 callback filters 到 kernel（实现 on_llm_call / on_tool_call）
%%%   - 组装发送给 kernel 的消息列表（system_prompt + history + user_msg）
%%%
%%% 设计决策：
%%%   - 使用 Map 而非 Record，灵活可序列化
%%%   - 支持两种 kernel 来源：预构建的 kernel 或从组件自动构建
%%%   - Callback 通过 kernel filter 注入，利用 filter 机制自动触发
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_state).

-export([create/1, build_kernel/1, inject_callback_filters/2]).
-export([store/1, conversation_id/1]).

-export_type([agent_state/0]).

%% 默认共享会话存储（单例）的注册名。各 agent 用各自的 conversation_id
%% 在同一 store 内分区，避免每个 agent 占用一个进程/动态原子。
-define(DEFAULT_STORE_NAME, beamai_agent_default_memory).

-type agent_state() :: #{
    '__agent__' := true,            %% 标识这是一个 agent 状态 map
    id := binary(),                 %% agent 唯一标识（自动生成或用户指定）
    name := binary(),               %% agent 显示名称
    kernel := beamai_kernel:kernel(), %% kernel 实例（含 LLM、plugins、filters、memory filter）
    store := beamai_chat_memory:handle() | undefined, %% 会话历史存储句柄（filter-memory）
    conversation_id := binary(),    %% 本 agent 的会话标识，用于在 store 内定位历史
    system_prompt := binary() | undefined, %% 系统提示词
    max_tool_iterations := pos_integer(),  %% tool loop 最大迭代次数
    callbacks := beamai_agent_callbacks:callbacks(), %% 回调函数表
    auto_save := boolean(),         %% 是否每轮自动保存
    turn_count := non_neg_integer(),%% 已完成的对话轮数
    metadata := map(),              %% 用户自定义元数据
    created_at := integer(),        %% 创建时间戳（毫秒）
    %% 中断相关
    interrupt_state := undefined | interrupt_state(), %% 中断时的完整上下文
    run_id := binary() | undefined, %% 当前执行的唯一 ID
    interrupt_tools := [map()]      %% 中断 tool 定义列表
}.

-type interrupt_state() :: #{
    status := interrupted,
    reason := term(),                     %% 中断原因
    pending_messages := [map()],          %% tool loop 中已累积的消息
    assistant_response := map(),          %% LLM 的响应（含 tool_calls）
    completed_tool_results := [map()],    %% 已完成的 tool 结果
    interrupted_tool_call := map() | undefined, %% 触发中断的 tool_call
    iteration := non_neg_integer(),       %% 当前 tool loop 迭代次数
    tool_calls_made := [map()],           %% 之前已执行的 tool 调用记录
    interrupt_type := tool_request | tool_result | callback,
    created_at := integer()
}.

-export_type([interrupt_state/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 从 Config 创建完整的 agent_state
%%
%% 这是 agent 初始化的核心入口，执行以下步骤：
%%   1. 从 Config 构建 kernel（或使用预构建的 kernel）
%%   2. 从 Config 提取 callbacks 配置
%%   3. 将 callback filters 注入到 kernel 中
%%   4. 组装完整的 agent_state map
%%
%% Config 支持的选项：
%%   kernel — 预构建的 kernel 实例（与 llm/plugins 互斥）
%%   llm — LLM 配置，支持 {Provider, Opts} 元组或 config() map
%%   plugins — 要加载的 plugin 模块列表 [module()]
%%   middlewares — middleware 配置列表 [{Module, Opts}]
%%   system_prompt — 系统提示词（binary）
%%   max_tool_iterations — 最大 tool loop 迭代次数（默认 10）
%%   callbacks — 观察性回调 map（参见 beamai_agent_callbacks:callbacks()）
%%   memory — 持久化后端实例
%%   auto_save — 是否每轮结束后自动保存（默认 false）
%%   id — agent ID（默认自动生成）
%%   name — agent 名称（默认 <<"agent">>）
%%   metadata — 用户自定义元数据 map
%%   kernel_settings — 创建新 kernel 时的设置项
%%
%% @param Config 配置选项 map
%% @returns {ok, agent_state()} 创建成功
%% @returns {error, {init_failed, Reason, Stack}} 创建失败
-spec create(map()) -> {ok, agent_state()} | {error, term()}.
create(Config) ->
    try
        Callbacks = maps:get(callbacks, Config, #{}),
        Kernel0 = build_kernel(Config),
        Kernel1 = inject_callback_filters(Kernel0, Callbacks),
        %% 解析会话存储并挂载 filter-memory：跨轮对话历史由 Memory filter
        %% 按 conversation_id 管理（不再用 agent_state.messages 自累积）。
        {Store, Kernel} = setup_memory(Kernel1, Config),
        Id = maps:get(id, Config, beamai_id:gen_id(<<"agent">>)),
        State = #{
            '__agent__' => true,
            id => Id,
            name => maps:get(name, Config, <<"agent">>),
            kernel => Kernel,
            store => Store,
            conversation_id => maps:get(conversation_id, Config, beamai_id:gen_id(<<"conv">>)),
            system_prompt => maps:get(system_prompt, Config, undefined),
            max_tool_iterations => maps:get(max_tool_iterations, Config, 10),
            callbacks => Callbacks,
            auto_save => maps:get(auto_save, Config, false),
            turn_count => 0,
            metadata => maps:get(metadata, Config, #{}),
            created_at => erlang:system_time(millisecond),
            %% 中断相关字段
            interrupt_state => undefined,
            run_id => undefined,
            interrupt_tools => maps:get(interrupt_tools, Config, [])
        },
        {ok, State}
    catch
        error:Reason:Stack ->
            {error, {init_failed, Reason, Stack}}
    end.

%% @doc 获取 agent 的会话存储句柄
-spec store(agent_state()) -> beamai_chat_memory:handle() | undefined.
store(#{store := Store}) -> Store.

%% @doc 获取 agent 的会话标识
-spec conversation_id(agent_state()) -> binary().
conversation_id(#{conversation_id := ConvId}) -> ConvId.

%% @doc 从 Config 构建 Kernel 实例
%%
%% 支持两种模式：
%%   1. 直接使用预构建的 kernel（Config 中包含 kernel 键）
%%   2. 从组件自动构建（依次添加 LLM 服务、plugins、middlewares）
%%
%% 自动构建顺序：
%%   new(Settings) → add_llm → add_plugins → add_middlewares
%%
%% @param Config 配置 map
%% @returns 构建完成的 kernel 实例
-spec build_kernel(map()) -> map().
build_kernel(#{kernel := Kernel}) when is_map(Kernel) ->
    Kernel;
build_kernel(Config) ->
    Settings = maps:get(kernel_settings, Config, #{}),
    K0 = beamai_kernel:new(Settings),
    K1 = add_llm(K0, Config),
    K2 = add_plugins(K1, Config),
    K3 = add_middlewares(K2, Config),
    K3.

%% @doc 注入 callback filters 到 kernel
%%
%% 根据 callbacks 中注册的回调类型，向 kernel 注入对应的洋葱式 filter：
%%   - 若注册了 on_llm_call: 注入 around_chat filter（每次 LLM 调用前触发）
%%   - 若注册了 on_tool_call: 注入 around_tool filter（每次工具调用前触发）
%%
%% Filter 设计要点（around 闭包模型）：
%%   - 使用 order 9999（最外层之后/最内层，最后注册）；回调仅作观察，不改写请求
%%   - 前置触发回调后，原样 Next(Req) 透传，不影响执行流程
%%   - 回调异常由 beamai_agent_callbacks:invoke/3 内部捕获
%%   - chat 请求形如 #{messages, context, opts}；tool 请求形如 #{tool, args, context}，
%%     其中 tool 为 tool_spec map（名称在 name 键）
%%
%% @param Kernel kernel 实例
%% @param Callbacks 回调注册表
%% @returns 注入 filter 后的 kernel
-spec inject_callback_filters(beamai_kernel:kernel(), beamai_agent_callbacks:callbacks()) ->
    beamai_kernel:kernel().
inject_callback_filters(Kernel, Callbacks) ->
    HasLlmCb = maps:is_key(on_llm_call, Callbacks),
    HasToolCb = maps:is_key(on_tool_call, Callbacks),
    K1 = case HasLlmCb of
        true ->
            %% 注入 around_chat filter：每次 LLM 调用前触发 on_llm_call 回调
            LlmFilter = beamai_filter:new(
                <<"agent_on_llm_call">>,
                #{around_chat => fun(Req, _FCtx, Next) ->
                    Messages = maps:get(messages, Req, []),
                    beamai_agent_callbacks:invoke(on_llm_call, [Messages, #{}], Callbacks),
                    Next(Req)
                end},
                9999
            ),
            beamai_kernel:add_filter(Kernel, LlmFilter);
        false ->
            Kernel
    end,
    case HasToolCb of
        true ->
            %% 注入 around_tool filter：每次工具调用前触发 on_tool_call 回调
            ToolFilter = beamai_filter:new(
                <<"agent_on_tool_call">>,
                #{around_tool => fun(Req, _FCtx, Next) ->
                    ToolSpec = maps:get(tool, Req, #{}),
                    ToolName = maps:get(name, ToolSpec, <<>>),
                    Args = maps:get(args, Req, #{}),
                    beamai_agent_callbacks:invoke(on_tool_call, [ToolName, Args], Callbacks),
                    Next(Req)
                end},
                9999
            ),
            beamai_kernel:add_filter(K1, ToolFilter);
        false ->
            K1
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 解析会话存储并挂载 filter-memory
%%
%% Config 的 memory 选项：
%%   - 缺省：使用懒启动的共享默认 store（注册名 ?DEFAULT_STORE_NAME）
%%   - false | none：不启用记忆（不挂 Memory filter，store=undefined）
%%   - 句柄 {Module, Ref}：使用调用方自管的 store（可多 agent 共享，生命周期自负责）
%%
%% 在 build_kernel + inject_callback_filters 之后挂载，确保即便传入预构建
%% kernel（build_kernel 原样返回）也能获得会话记忆。
-spec setup_memory(beamai_kernel:kernel(), map()) ->
    {beamai_chat_memory:handle() | undefined, beamai_kernel:kernel()}.
setup_memory(Kernel, Config) ->
    case maps:get(memory, Config, default) of
        false -> {undefined, Kernel};
        none -> {undefined, Kernel};
        default ->
            Store = ensure_default_store(),
            {Store, beamai_kernel:with_memory(Kernel, Store)};
        Handle when is_tuple(Handle) ->
            {Handle, beamai_kernel:with_memory(Kernel, Handle)}
    end.

%% @private 懒启动共享默认会话 store（幂等、单例）
%%
%% 用固定注册名避免动态原子增长；各 agent 以各自 conversation_id 在其中分区。
%% 已启动则复用；未启动则启动并 unlink，使其作为独立单例存活、不随调用进程退出
%% 而终止（需要可监督/可控生命周期的调用方应通过 Config 的 memory 选项传入自管 store）。
-spec ensure_default_store() -> beamai_chat_memory:handle().
ensure_default_store() ->
    Name = ?DEFAULT_STORE_NAME,
    case whereis(Name) of
        undefined ->
            case beamai_chat_memory_ets:start_link(Name) of
                {ok, Pid} -> unlink(Pid);
                {error, {already_started, _Pid}} -> ok
            end;
        _Pid ->
            ok
    end,
    beamai_chat_memory_ets:handle(Name).

%% @private 添加 LLM 服务到 kernel
%%
%% 支持三种 LLM 配置格式：
%%   - undefined: 不添加 LLM（kernel 仅用于函数调用）
%%   - config() map: 已通过 beamai_chat_completion:create 创建的配置
%%   - {Provider, Opts} 元组: 自动调用 create 构建配置
add_llm(Kernel, Config) ->
    case maps:get(llm, Config, undefined) of
        undefined ->
            Kernel;
        LlmConfig when is_map(LlmConfig) ->
            beamai_kernel:add_service(Kernel, LlmConfig);
        {Provider, Opts} ->
            LlmCfg = beamai_chat_completion:create(Provider, Opts),
            beamai_kernel:add_service(Kernel, LlmCfg)
    end.

%% @private 加载工具模块到 kernel
%%
%% 遍历 plugins 列表中的模块，逐个用 beamai_kernel 原语加载：
%% 每个模块需实现 beamai_tool_behaviour 回调（tools/0），
%% 若模块还定义了 filters/0，则一并加载其过滤器。
add_plugins(Kernel, Config) ->
    Plugins = maps:get(plugins, Config, []),
    lists:foldl(fun load_plugin/2, Kernel, Plugins).

%% @private 加载单个工具模块（tools/0 + 可选 filters/0）
load_plugin(Module, Kernel) ->
    K1 = beamai_kernel:add_tool_module(Kernel, Module),
    case erlang:function_exported(Module, filters, 0) of
        true ->
            lists:foldl(fun(F, K) -> beamai_kernel:add_filter(K, F) end,
                        K1, Module:filters());
        false ->
            K1
    end.

%% @private middlewares 在 SimpleAgent 中不支持
%%
%% middleware 子系统属于 beamai_extra 扩展能力。本核心 agent 仅做忽略并告警。
add_middlewares(Kernel, Config) ->
    case maps:get(middlewares, Config, []) of
        [] -> Kernel;
        _ ->
            logger:warning("beamai_agent: middlewares 已忽略（需 beamai_extra 扩展支持）"),
            Kernel
    end.
