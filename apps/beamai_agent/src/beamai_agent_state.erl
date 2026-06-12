%%%-------------------------------------------------------------------
%%% @doc Agent 状态构建与 Kernel 集成
%%%
%%% 负责 agent 生命周期中的状态初始化工作：
%%%   - 从用户 Config 构建完整的 agent_state map
%%%   - 构建或接收 kernel（LLM 服务 + 工具插件）
%%%   - 解析 memory provider（与 kernel 正交，互不感知）
%%%
%%% 设计决策：
%%%   - 使用 Map 而非 Record，灵活可序列化
%%%   - kernel 与 memory 是两个正交的创建参数：kernel 管 LLM/工具调用，
%%%     memory 管跨轮会话历史，二者任意组合（预构建 kernel + 自定义 provider 等）
%%%   - agent 本身没有 filter 概念：回调（callbacks）是 agent 唯一的观察扩展点，
%%%     由 tool loop 直接触发；filter 属于 kernel 层（想用就自建 kernel 传入）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_state).

-export([create/1, build_kernel/1]).
-export([memory/1, conversation_id/1]).

-export_type([agent_state/0]).

%% 默认共享会话存储（单例）的注册名。各 agent 用各自的 conversation_id
%% 在同一 store 内分区，避免每个 agent 占用一个进程/动态原子。
-define(DEFAULT_STORE_NAME, beamai_agent_default_memory).

-type agent_state() :: #{
    '__agent__' := true,            %% 标识这是一个 agent 状态 map
    id := binary(),                 %% agent 唯一标识（自动生成或用户指定）
    name := binary(),               %% agent 显示名称
    kernel := beamai_kernel:kernel(), %% kernel 实例（LLM 服务 + 工具）
    memory := beamai_memory_provider:provider() | undefined, %% 记忆 provider（策略+存储）
    conversation_id := binary(),    %% 本 agent 的会话标识，用于在记忆内定位历史
    system_prompt := binary() | undefined, %% 系统提示词
    max_tool_iterations := pos_integer(),  %% tool loop 最大迭代次数
    parallel_tools := boolean(),           %% 一轮多个 tool_call 是否并发执行（默认 true）
    callbacks := beamai_agent_callbacks:callbacks(), %% 回调函数表
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
    messages := [map()],                  %% 中断时本轮已累积的完整 messages（供 resume 续接）
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
%%   2. 从 Config 解析 memory provider（callbacks 与 memory 均由 tool loop 显式编排，
%%      不向 kernel 注入 filter）
%%   3. 组装完整的 agent_state map
%%
%% Config 支持的选项：
%%   kernel — 预构建的 kernel 实例（与 llm/plugins 互斥）
%%   llm — LLM 配置，支持 {Provider, Opts} 元组或 config() map
%%   plugins — 要加载的 plugin 模块列表 [module()]
%%   system_prompt — 系统提示词（binary）
%%   max_tool_iterations — 最大 tool loop 迭代次数（默认 10）
%%   parallel_tools — 一轮多个 tool_call 是否并发执行（默认 true；false 则串行）
%%   callbacks — 观察性回调 map（参见 beamai_agent_callbacks:callbacks()）
%%   memory — 会话记忆（解析为 beamai_memory_provider，与 kernel 正交）：
%%            缺省用默认 provider+共享 store；{window,N} 套滑动窗口；
%%            {store,Handle} 指定存储后端；{Mod,Ref} 自定义 provider（摘要/RAG…）；
%%            false|none 关闭记忆。详见 setup_memory/1
%%   conversation_id — 会话标识（默认自动生成）
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
        Kernel = build_kernel(Config),
        %% 解析记忆 provider：跨轮历史由 Agent 在 tool loop 里显式委托给 provider，
        %% 不再注入 kernel filter（callbacks 同理，全在 loop 触发）。
        Memory = setup_memory(Config),
        Id = maps:get(id, Config, beamai_id:gen_id(<<"agent">>)),
        State = #{
            '__agent__' => true,
            id => Id,
            name => maps:get(name, Config, <<"agent">>),
            kernel => Kernel,
            memory => Memory,
            conversation_id => maps:get(conversation_id, Config, beamai_id:gen_id(<<"conv">>)),
            system_prompt => maps:get(system_prompt, Config, undefined),
            max_tool_iterations => maps:get(max_tool_iterations, Config, 10),
            parallel_tools => maps:get(parallel_tools, Config, true),
            callbacks => Callbacks,
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

%% @doc 获取 agent 的记忆 provider（无记忆时 undefined）
-spec memory(agent_state()) -> beamai_memory_provider:provider() | undefined.
memory(#{memory := Memory}) -> Memory.

%% @doc 获取 agent 的会话标识
-spec conversation_id(agent_state()) -> binary().
conversation_id(#{conversation_id := ConvId}) -> ConvId.

%% @doc 从 Config 构建 Kernel 实例
%%
%% 支持两种模式：
%%   1. 直接使用预构建的 kernel（Config 中包含 kernel 键）
%%   2. 从组件自动构建（依次添加 LLM 服务、plugins）
%%
%% 自动构建顺序：
%%   new(Settings) → add_llm → add_plugins
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
    add_plugins(K1, Config).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 解析记忆 provider（Agent 在 tool loop 里显式使用，不挂 kernel filter）
%%
%% Config 的 memory 选项（统一解析为一个 beamai_memory_provider:provider/0）：
%%   - 缺省（default）：默认 provider 包共享默认 store（懒启动，注册名 ?DEFAULT_STORE_NAME）
%%   - false | none：不启用记忆（memory=undefined，仅在本轮内累积、不持久）
%%   - {window, N}：默认 provider 带 N 条滑动窗口（默认 store），发给 LLM 时只保留
%%     最近 N 条非系统消息（全量仍持久于底层），防止长对话撑爆 context window
%%   - {store, Handle}：默认 provider 包指定存储后端句柄（无窗口）
%%   - {Module, Ref}：直接作为 provider（须实现 beamai_memory_provider 协议；
%%     自定义记忆策略：摘要/RAG/token 窗口…）
%%
%% 注：默认为无界增长，长对话需显式选 {window, N} 或自管裁剪/摘要 provider。
%% 想对自管存储套窗口：beamai_memory_provider_default:new(Handle, N) 作为 provider 传入。
-spec setup_memory(map()) -> beamai_memory_provider:provider() | undefined.
setup_memory(Config) ->
    case maps:get(memory, Config, default) of
        false -> undefined;
        none -> undefined;
        default ->
            beamai_memory_provider:default(ensure_default_store());
        {window, MaxMessages} when is_integer(MaxMessages), MaxMessages > 0 ->
            beamai_memory_provider_default:new(ensure_default_store(), MaxMessages);
        {store, Handle} when is_tuple(Handle) ->
            beamai_memory_provider:default(Handle);
        Provider when is_tuple(Provider) ->
            %% 自定义 provider（{Module, Ref}，须实现 beamai_memory_provider）
            Provider
    end.

%% @private 确保共享默认会话 store 运行（幂等、单例），返回句柄
%%
%% 用固定注册名避免动态原子增长；各 agent 以各自 conversation_id 在其中分区。
%% 优先把 store 纳入 beamai_agent 监督树（崩溃可重启）；当 beamai_agent 未作为
%% OTP 应用启动时（库式直接调用 / 裸 eunit），回退到懒启动并 unlink 的孤儿单例。
-spec ensure_default_store() -> beamai_chat_memory:handle().
ensure_default_store() ->
    Name = ?DEFAULT_STORE_NAME,
    case whereis(Name) of
        Pid when is_pid(Pid) ->
            beamai_chat_memory_ets:handle(Name);
        undefined ->
            case whereis(beamai_agent_sup) of
                SupPid when is_pid(SupPid) ->
                    %% 监督树在线：把 store 作为 permanent 子进程纳入
                    beamai_agent_sup:ensure_store(Name);
                undefined ->
                    %% app 未启动：回退到孤儿单例
                    start_orphan_store(Name)
            end
    end.

%% @private 懒启动孤儿 store（unlink，使其不随调用进程退出而终止）
%%
%% 孤儿 store 无监督树保护：崩溃即丢失全部跨轮会话历史且不会重启。
%% 记 warning 提示生产环境应将 beamai_agent 作为 OTP 应用启动。
-spec start_orphan_store(atom()) -> beamai_chat_memory:handle().
start_orphan_store(Name) ->
    logger:warning(
        "beamai_agent app not started; falling back to unsupervised orphan memory store ~p. "
        "Start beamai_agent as an OTP application to get a supervised store.", [Name]),
    case beamai_chat_memory_ets:start_link(Name) of
        {ok, Pid} -> unlink(Pid);
        {error, {already_started, _Pid}} -> ok
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
%% 整体委托 beamai_kernel:add_tool_module/2（每个模块需实现
%% beamai_tool_behaviour 的 tools/0；模块若定义 filters/0，由 kernel
%% 原语自行处理——agent 层不感知 filter）。
add_plugins(Kernel, Config) ->
    Plugins = maps:get(plugins, Config, []),
    lists:foldl(fun(Module, K) -> beamai_kernel:add_tool_module(K, Module) end,
                Kernel, Plugins).
