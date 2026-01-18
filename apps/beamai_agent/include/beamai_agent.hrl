%%%-------------------------------------------------------------------
%%% @doc beamai_agent 共享头文件
%%%
%%% 定义 Agent 状态和回调记录，供子模块共享使用。
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AGENT_SIMPLE_HRL).
-define(AGENT_SIMPLE_HRL, true).

%% 回调处理器记录 (类似 LangChain BaseCallbackHandler)
-record(callbacks, {
    %% LLM 回调
    on_llm_start        :: function() | undefined,  %% fun(Prompts, Metadata) -> ok
    on_llm_end          :: function() | undefined,  %% fun(Response, Metadata) -> ok
    on_llm_error        :: function() | undefined,  %% fun(Error, Metadata) -> ok
    on_llm_new_token    :: function() | undefined,  %% fun(Token, Metadata) -> ok (流式)
    %% Tool 回调
    on_tool_start       :: function() | undefined,  %% fun(ToolName, Input, Metadata) -> ok
    on_tool_end         :: function() | undefined,  %% fun(ToolName, Output, Metadata) -> ok
    on_tool_error       :: function() | undefined,  %% fun(ToolName, Error, Metadata) -> ok
    %% Agent 回调
    on_agent_action     :: function() | undefined,  %% fun(Action, Metadata) -> ok
    on_agent_finish     :: function() | undefined,  %% fun(Result, Metadata) -> ok
    %% Chain 回调
    on_chain_start      :: function() | undefined,  %% fun(Input, Metadata) -> ok
    on_chain_end        :: function() | undefined,  %% fun(Output, Metadata) -> ok
    on_chain_error      :: function() | undefined,  %% fun(Error, Metadata) -> ok
    %% Retriever 回调 (RAG 相关)
    on_retriever_start  :: function() | undefined,  %% fun(Query, Metadata) -> ok
    on_retriever_end    :: function() | undefined,  %% fun(Documents, Metadata) -> ok
    on_retriever_error  :: function() | undefined,  %% fun(Error, Metadata) -> ok
    %% 其他回调
    on_text             :: function() | undefined,  %% fun(Text, Metadata) -> ok
    on_retry            :: function() | undefined,  %% fun(RetryState, Metadata) -> ok
    on_custom_event     :: function() | undefined   %% fun(EventName, Data, Metadata) -> ok
}).

%% 内部状态
-record(state, {
    id              :: binary(),            %% Agent ID
    name            :: binary(),            %% Agent 名称
    system_prompt   :: binary(),            %% 系统提示词
    tools           :: [map()],             %% 工具定义列表
    tool_handlers   :: #{binary() => function()}, %% 工具处理器映射
    llm_config      :: map(),               %% LLM 配置
    graph           :: map() | undefined,   %% 编译后的图
    max_iterations  :: pos_integer(),       %% 最大迭代次数
    %% 对话历史
    messages        :: [map()],             %% 对话消息列表（可能已压缩，用于 LLM 调用）
    full_messages   :: [map()],             %% 完整对话历史（用于审计、调试、回溯）
    %% Scratchpad
    scratchpad      :: [map()],             %% 中间步骤记录
    %% 用户自定义上下文
    %% 用于存储用户自定义数据，会被自动持久化到 checkpoint
    %% Tool 和 Middleware 可以通过 graph:get(State, context, #{}) 读取
    %% 通过返回 {ok, Result, #{context => NewContext}} 或 {update, #{context => NewContext}} 修改
    context         :: map(),               %% 用户自定义上下文数据
    %% 人机协作（由 middleware_human_approval 处理）
    pending_action   :: map() | undefined,  %% 等待确认的动作
    %% 结构化输出
    response_format  :: map() | undefined,  %% 输出格式约束
    %% 回调处理器
    callbacks       :: #callbacks{},        %% 回调处理器集合
    %% Middleware 系统
    middlewares     :: [term()],            %% Middleware 配置列表
    middleware_chain :: list() | undefined, %% 已初始化的 Middleware 链
    %% Checkpoint/Storage
    storage         :: beamai_memory:memory() | undefined, %% 存储后端 (beamai_memory 实例)
    auto_checkpoint :: boolean(),           %% 是否自动保存检查点
    %% 运行时元数据
    run_id          :: binary() | undefined %% 当前运行 ID
}).

-endif. %% AGENT_SIMPLE_HRL
