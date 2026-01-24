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

-export([create/1, build_kernel/1, build_messages/2, inject_callback_filters/2]).

-export_type([agent_state/0]).

-type agent_state() :: #{
    '__agent__' := true,            %% 标识这是一个 agent 状态 map
    id := binary(),                 %% agent 唯一标识（自动生成或用户指定）
    name := binary(),               %% agent 显示名称
    kernel := beamai_kernel:kernel(), %% kernel 实例（含 LLM、plugins、filters）
    messages := [map()],            %% 对话消息历史
    system_prompt := binary() | undefined, %% 系统提示词
    max_tool_iterations := pos_integer(),  %% tool loop 最大迭代次数
    callbacks := beamai_agent_callbacks:callbacks(), %% 回调函数表
    memory := term() | undefined,   %% 持久化后端实例
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
        Kernel = inject_callback_filters(Kernel0, Callbacks),
        State = #{
            '__agent__' => true,
            id => maps:get(id, Config, beamai_id:gen_id(<<"agent">>)),
            name => maps:get(name, Config, <<"agent">>),
            kernel => Kernel,
            messages => [],
            system_prompt => maps:get(system_prompt, Config, undefined),
            max_tool_iterations => maps:get(max_tool_iterations, Config, 10),
            callbacks => Callbacks,
            memory => maps:get(memory, Config, undefined),
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
-spec build_kernel(map()) -> beamai_kernel:kernel().
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
%% 根据 callbacks 中注册的回调类型，向 kernel 注入对应的 filter：
%%   - 若注册了 on_llm_call: 注入 pre_chat filter（每次 LLM 调用前触发）
%%   - 若注册了 on_tool_call: 注入 pre_invocation filter（每次函数调用前触发）
%%
%% Filter 设计要点：
%%   - 使用 priority 9999（最低优先级），确保最后执行，不影响其他 filter
%%   - Filter handler 通过闭包捕获 Callbacks 引用
%%   - Filter 始终返回 {continue, FilterCtx}，不修改执行流程
%%   - 回调异常由 beamai_agent_callbacks:invoke/3 内部捕获
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
            %% 注入 pre_chat filter：每次 LLM 调用前触发 on_llm_call 回调
            LlmFilter = beamai_filter:new(
                <<"agent_on_llm_call">>,
                pre_chat,
                fun(FilterCtx) ->
                    Messages = maps:get(messages, FilterCtx, []),
                    Meta = maps:get(metadata, FilterCtx, #{}),
                    beamai_agent_callbacks:invoke(on_llm_call, [Messages, Meta], Callbacks),
                    {continue, FilterCtx}
                end,
                9999
            ),
            beamai_kernel:add_filter(Kernel, LlmFilter);
        false ->
            Kernel
    end,
    case HasToolCb of
        true ->
            %% 注入 pre_invocation filter：每次函数调用前触发 on_tool_call 回调
            ToolFilter = beamai_filter:new(
                <<"agent_on_tool_call">>,
                pre_invocation,
                fun(FilterCtx) ->
                    FuncDef = maps:get(function, FilterCtx, #{}),
                    FuncName = maps:get(name, FuncDef, <<>>),
                    Args = maps:get(args, FilterCtx, #{}),
                    beamai_agent_callbacks:invoke(on_tool_call, [FuncName, Args], Callbacks),
                    {continue, FilterCtx}
                end,
                9999
            ),
            beamai_kernel:add_filter(K1, ToolFilter);
        false ->
            K1
    end.

%% @doc 组装发送给 kernel 的消息列表
%%
%% 按以下顺序拼接消息：
%%   1. system_prompt（如果存在且非空，包装为 system 角色消息）
%%   2. 历史消息（之前各轮的 user + assistant 消息）
%%   3. 当前用户消息
%%
%% @param State agent 状态（从中提取 system_prompt 和 messages）
%% @param UserMsg 当前用户消息 map（#{role => user, content => ...}）
%% @returns 完整的消息列表，可直接传给 kernel:invoke_chat
-spec build_messages(agent_state(), map()) -> [map()].
build_messages(#{system_prompt := SysPrompt, messages := History}, UserMsg) ->
    MaybeSys = case SysPrompt of
        undefined -> [];
        <<>> -> [];
        P -> [#{role => system, content => P}]
    end,
    MaybeSys ++ History ++ [UserMsg].

%%====================================================================
%% 内部函数
%%====================================================================

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

%% @private 加载 plugins 到 kernel
%%
%% 遍历 plugins 列表中的模块，逐个调用 beamai_plugins:load 加载。
%% 每个模块需实现 beamai_plugin_behaviour 回调。
add_plugins(Kernel, Config) ->
    Plugins = maps:get(plugins, Config, []),
    beamai_plugins:load_all(Kernel, Plugins).

%% @private 添加 middlewares 到 kernel
%%
%% 将 middleware 配置列表转换为 kernel filters 并注入。
%% 空列表时不做任何操作。
add_middlewares(Kernel, Config) ->
    case maps:get(middlewares, Config, []) of
        [] -> Kernel;
        Middlewares -> beamai_plugins:with_middleware(Kernel, Middlewares)
    end.
