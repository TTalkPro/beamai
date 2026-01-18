%%%-------------------------------------------------------------------
%%% @doc Agent 行为定义
%%%
%%% 定义所有 Agent 必须实现的行为接口。
%%% 提供 Agent 生命周期、消息处理和工具执行的回调规范。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_behaviour).

%% 类型定义
-type agent_id() :: binary().
-type agent_state() :: term().
-type message() :: #{
    role := user | assistant | system | tool,
    content := binary(),
    name => binary(),
    tool_call_id => binary(),
    tool_calls => [tool_call()]
}.
-type tool_call() :: #{
    id := binary(),
    type := function,
    function := #{
        name := binary(),
        arguments := binary()
    }
}.
-type tool() :: #{
    type := function,
    function := #{
        name := binary(),
        description := binary(),
        parameters := map()
    }
}.
-type tool_result() :: #{
    tool_call_id := binary(),
    content := binary()
}.
-type run_result() :: #{
    status := completed | tool_use | error | max_iterations,
    messages := [message()],
    final_response => binary(),
    error => term()
}.

-export_type([agent_id/0, agent_state/0, message/0, tool_call/0,
              tool/0, tool_result/0, run_result/0]).

%%====================================================================
%% 回调函数定义
%%====================================================================

%%  使用给定选项初始化 Agent
%% 返回初始状态
-callback init(AgentId :: agent_id(), Opts :: map()) ->
    {ok, State :: agent_state()} | {error, Reason :: term()}.

%%  获取 Agent 的系统提示词
-callback get_system_prompt(State :: agent_state()) ->
    binary().

%%  获取 Agent 可用的工具列表
-callback get_tools(State :: agent_state()) ->
    [tool()].

%%  执行工具调用（不带 context）
%% 返回工具执行结果
-callback execute_tool(ToolName :: binary(),
                       Arguments :: map(),
                       State :: agent_state()) ->
    {ok, Result :: binary(), NewState :: agent_state()} |
    {error, Reason :: term(), NewState :: agent_state()}.

%%  执行工具调用（带 context 注入和更新）
%%
%% Context 包含当前会话上下文，工具可以读取用户信息等。
%% 返回值可以包含 context 更新，框架会自动合并到存储中。
%%
%% 返回格式：
%% - {ok, Result, NewState}: 不更新 context
%% - {ok, Result, ContextUpdates, NewState}: 更新 context
%% - {error, Reason, NewState}: 错误
-callback execute_tool(ToolName :: binary(),
                       Arguments :: map(),
                       Context :: map(),
                       State :: agent_state()) ->
    {ok, Result :: binary(), NewState :: agent_state()} |
    {ok, Result :: binary(), ContextUpdates :: map(), NewState :: agent_state()} |
    {error, Reason :: term(), NewState :: agent_state()}.

%%  发送消息到 LLM 之前调用
%% 允许修改消息内容
-callback pre_llm_call(Messages :: [message()], State :: agent_state()) ->
    {ok, ModifiedMessages :: [message()], NewState :: agent_state()}.

%%  从 LLM 收到响应后调用
%% 允许处理响应内容
-callback post_llm_call(Response :: message(), State :: agent_state()) ->
    {ok, ProcessedResponse :: message(), NewState :: agent_state()}.

%%  Agent 运行完成时调用
-callback on_complete(Result :: run_result(), State :: agent_state()) ->
    {ok, NewState :: agent_state()}.

%%  Agent 遇到错误时调用
-callback on_error(Error :: term(), State :: agent_state()) ->
    {ok, NewState :: agent_state()}.

%%  判断工具执行后是否继续运行
%% 返回 true 继续，false 停止
-callback should_continue(Messages :: [message()],
                          IterationCount :: non_neg_integer(),
                          State :: agent_state()) ->
    {boolean(), NewState :: agent_state()}.

%%  终止回调 - 清理资源
-callback terminate(Reason :: term(), State :: agent_state()) ->
    ok.

%%  处理自定义 gen_server 调用
%% 允许 Agent 实现处理自定义消息（如 add_tool、remove_tool 等）
-callback handle_call(Request :: term(), State :: agent_state()) ->
    {reply, Reply :: term(), NewState :: agent_state()} |
    {noreply, NewState :: agent_state()} |
    not_handled.

%%  从检查点恢复时调用
%% 允许 Agent 在恢复后执行自定义初始化逻辑
%%
%% 参数：
%% - CheckpointId: 恢复的检查点 ID
%% - Metadata: 检查点中保存的元数据
%% - State: 当前 Agent 状态
%%
%% 返回：
%% - {ok, NewState}: 更新后的状态
%% - ignore: 忽略此回调
-callback on_restore(CheckpointId :: binary(),
                     Metadata :: map(),
                     State :: agent_state()) ->
    {ok, NewState :: agent_state()} | ignore.

%%====================================================================
%% 可选回调（带默认实现）
%%====================================================================

%%  获取中断配置
%% 返回 interrupt_before 和 interrupt_after 配置
-callback get_interrupt_config(State :: agent_state()) ->
    #{
        interrupt_before => [atom()],
        interrupt_after => [atom()],
        has_handler => boolean()
    }.

%%  获取响应格式配置
%% 返回结构化输出的格式约束
%%
%% 返回格式：
%% - undefined: 无格式要求（默认）
%% - #{type := json, schema := Schema}: JSON 输出，带可选的 JSON Schema
%% - #{type := json, response_format := #{type := <<"json_schema">>, ...}}: 原生 JSON Mode (OpenAI)
%% - #{type := xml}: XML 输出
%% - #{type := csv}: CSV 输出
%%
%% 示例：
%% %% 简单 JSON
%% get_response_format(State) -> #{type => json}.
%%
%% %% 带 JSON Schema
%% get_response_format(State) -> #{
%%     type => json,
%%     schema => #{
%%         <<"type">> => <<"object">>,
%%         <<"properties">> => #{
%%             <<"name">> => #{<<"type">> => <<"string">>},
%%             <<"age">> => #{<<"type">> => <<"integer">>}
%%         },
%%         <<"required">> => [<<"name">>]
%%     }
%% }.
%%
%% %% OpenAI 原生 JSON Mode
%% get_response_format(State) -> #{
%%     type => json,
%%     response_format => #{<<"type">> => <<"json_object">>}
%% }.
-callback get_response_format(State :: agent_state()) ->
    undefined |
    #{type := json, schema => map(), response_format => map()} |
    #{type := xml} |
    #{type := csv}.

-optional_callbacks([
    execute_tool/4,   %% 带 context 的工具执行（可选，优先于 execute_tool/3）
    pre_llm_call/2,
    post_llm_call/2,
    on_complete/2,
    on_error/2,
    should_continue/3,
    terminate/2,
    handle_call/2,
    on_restore/3,
    get_interrupt_config/1,
    get_response_format/1
]).
