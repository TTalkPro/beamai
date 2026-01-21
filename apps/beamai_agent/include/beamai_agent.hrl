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

%% Agent 配置（创建时设置，运行期间不变）
-record(agent_config, {
    id              :: binary(),                       %% Agent ID
    name            :: binary(),                       %% Agent 名称
    system_prompt   :: binary(),                       %% 系统提示词
    tools           :: [map()],                        %% 工具定义列表
    tool_handlers   :: #{binary() => function()},     %% 工具处理器映射
    llm_config      :: map(),                          %% LLM 配置
    graph           :: map() | undefined,              %% 编译后的图
    max_iterations  :: pos_integer(),                  %% 最大迭代次数
    response_format :: map() | undefined,              %% 输出格式约束
    callbacks       :: #callbacks{},                   %% 回调处理器集合
    middlewares     :: [term()],                       %% Middleware 配置列表
    middleware_chain :: list() | undefined,            %% 已初始化的 Middleware 链
    storage         :: beamai_memory:memory() | undefined, %% 存储后端
    meta            :: map()                           %% 进程级元数据
}).

%% Agent 运行时状态
-record(state, {
    config          :: #agent_config{},        %% 配置（不可变）
    %% 以下字段映射到 graph_state
    messages        :: [map()],                %% 对话消息（可能已压缩）
    full_messages   :: [map()],                %% 完整对话历史
    scratchpad      :: [map()],                %% 中间步骤记录
    context         :: map(),                  %% 用户自定义上下文
    pending_action  :: map() | undefined,      %% 等待确认的动作
    run_id          :: binary() | undefined    %% 当前运行 ID
}).

-endif. %% AGENT_SIMPLE_HRL
