%%%-------------------------------------------------------------------
%%% @doc ChatClient：一次 LLM 调用的入口（对标 Spring AI 的 ChatClient）
%%%
%%% ChatClient 是框架的基础设施层，只提供两类原子能力——单次 LLM 调用
%%% （invoke_chat）与单次工具调用（invoke_tool），各自经过洋葱式 Filter 链。
%%% 它**不**负责 ReAct 工具调用循环（LLM ↔ Tool 的多轮编排是 Agent 的职责，
%%% 见 beamai_agent / beamai_agent_tool_loop）。
%%%
%%% 职责：
%%% - 管理工具注册
%%% - 持有 LLM 服务配置
%%% - 执行洋葱式 Filter 链
%%% - invoke_chat：单次 Chat Completion（经 around_chat 链）
%%% - invoke_tool：单次工具执行（经 around_tool 链）
%%%
%%% **重试不在链上**：它在 beamai_chat_model 内部，位于整个 filter 栈**之下**。
%%% filter 看到的是「一次逻辑调用」，重试重入碰不到任何 filter——around_chat 上
%%% 的记忆/记账因此每轮只跑一次。要观测每次真实尝试用 chat opts 的 `on_retry`
%%% 回调；要按会话改重试参数，把 max_retries 放进 chat opts。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_chat_client).

%% Build API
-export([new/0, new/1, new/2]).
-export([add_tool/2]).
-export([add_tools/2]).
-export([add_tool_module/2]).
-export([add_chat_model/2]).

%% Invoke API（只发起一次 LLM 调用；工具执行见 beamai_tool_executor）
-export([invoke_chat/3]).
-export([invoke_chat_stream/4]).

%% 访问器（持有什么就交出什么，不代答工具问题）
-export([tools/1]).
-export([filters/1]).
-export([chat_model/1]).
-export([state_slots/1]).

%% Types
-export_type([chat_client/0, chat_client_settings/0, chat_opts/0]).

-type chat_client() :: #{
    '__chat_client__' := true,
    tools := beamai_tool_registry:t(),
    chat_model := beamai_chat_behaviour:config() | undefined,
    filters := [beamai_filter:filter()],
    settings := chat_client_settings()
}.

-type chat_client_settings() :: #{
    default_timeout => pos_integer(),
    atom() => term()
}.

-type chat_opts() :: #{
    tools => [map()],
    tool_choice => auto | none | required,
    context => beamai_context:t(),
    system_prompts => [map()],
    %% 仅 invoke_chat_stream/4：provider 原始流事件的汇（见该函数文档）。
    %% ChatClient 层自己消费，不下发给 provider。
    on_raw_event => fun((map()) -> ok),
    atom() => term()
}.

%% 状态槽声明（存于 settings.state_slots）：工具 writes 折叠进 state 时，
%% 声明槽过其 reducer，未声明槽 last-writer（见 beamai_context:apply_writes/3）。

%%====================================================================
%% Build API
%%====================================================================

%% @doc 创建空 ChatClient（默认配置，无 filter）
-spec new() -> chat_client().
new() ->
    new(#{}, []).

%% @doc 创建 ChatClient（自定义配置，无 filter）
%%
%% @param Settings 配置项（如 #{default_timeout => 30000}）
%% @returns ChatClient 实例
-spec new(chat_client_settings()) -> chat_client().
new(Settings) ->
    new(Settings, []).

%% @doc 创建 ChatClient（自定义配置 + 一次性给出全量 filter）
%%
%% Filters 在构建时一次性给出，**注册顺序即层序**：列表靠前 = 外层
%% （前置先执行、后置后执行）。构建后不可增量追加——需要记忆时把
%% `beamai_memory_filter:memory_filter(Store)` 放在列表**首位**（最外层：
%% 先展开完整历史，再让内层 filter 处理）。
%%
%% @param Settings 配置项
%% @param Filters filter 列表（beamai_filter:new/2,3 创建）
%% @returns ChatClient 实例
-spec new(chat_client_settings(), [beamai_filter:filter()]) -> chat_client().
new(Settings, Filters) when is_map(Settings), is_list(Filters) ->
    #{
        '__chat_client__' => true,
        tools => #{},
        chat_model => undefined,
        filters => Filters,
        settings => Settings
    }.

%% @doc 注册工具到 ChatClient
%%
%% 工具以其名称为键存入 tools Map。重名工具会被覆盖。
%%
%% @param ChatClient ChatClient 实例
%% @param Tool 工具定义（需包含 name 字段）
%% @returns 更新后的 ChatClient
-spec add_tool(chat_client(), beamai_tool:tool_spec()) -> chat_client().
add_tool(#{tools := Tools} = ChatClient, Tool) ->
    ChatClient#{tools => beamai_tool_registry:add(Tools, Tool)}.

%% @doc 批量注册工具到 ChatClient
%%
%% @param ChatClient ChatClient 实例
%% @param ToolList 工具定义列表
%% @returns 更新后的 ChatClient
-spec add_tools(chat_client(), [beamai_tool:tool_spec()]) -> chat_client().
add_tools(#{tools := Tools} = ChatClient, ToolList) ->
    ChatClient#{tools => beamai_tool_registry:add_many(Tools, ToolList)}.

%% @doc 从模块自动加载并注册工具
%%
%% 模块需实现 beamai_tool_behaviour，至少实现 tools/0 回调。
%% 加载失败时抛出 {tool_module_load_failed, Module, Reason} 错误。
%% 注：模块只提供工具；filter 一律在 new/2 构建时一次性给出。
%%
%% @param ChatClient ChatClient 实例
%% @param Module 实现了工具回调的模块
%% @returns 更新后的 ChatClient
-spec add_tool_module(chat_client(), module()) -> chat_client().
add_tool_module(#{tools := Tools} = ChatClient, Module) ->
    ChatClient#{tools => beamai_tool_registry:from_module(Tools, Module)}.

%% @doc 设置 LLM 服务配置
%%
%% 配置通过 beamai_chat_model:create/2 创建。
%% 设置后可使用 invoke_chat/3。
%%
%% @param ChatClient ChatClient 实例
%% @param LlmConfig LLM 配置 Map
%% @returns 更新后的 ChatClient
-spec add_chat_model(chat_client(), beamai_chat_behaviour:config()) -> chat_client().
add_chat_model(ChatClient, ChatModel) ->
    ChatClient#{chat_model => ChatModel}.

%%====================================================================
%% Invoke API
%%====================================================================

%% @doc 发送 Chat Completion 请求（不含工具调用循环）
%%
%% 执行流程：chat filter 洋葱链（around_chat：前置改写请求 → LLM 调用 → 后置改写响应）。
%% ChatClient 需先通过 add_chat_model/2 配置 LLM。
%%
%% Opts 可含 system_prompts：作为临时**最内层** filter 注入（追加在全量 filter
%% 之后），在所有 filter 之后、LLM 之前前置系统消息且不入存储——memory filter
%% 展开的历史永远不含系统提示，用户 chat filter 看到的 messages 也不含系统提示。
%%
%% @param ChatClient ChatClient 实例
%% @param Messages 消息列表（[#{role => ..., content => ...}]）
%% @param Opts Chat 选项
%% @returns {ok, 响应 Map, 更新后上下文} | {error, 原因}
-spec invoke_chat(chat_client(), [map()], chat_opts()) ->
    {ok, map(), beamai_context:t()} | {error, term()}.
invoke_chat(ChatClient, Messages, Opts) ->
    case chat_model(ChatClient) of
        {ok, LlmConfig} ->
            #{filters := Filters0} = ChatClient,
            %% 绑 ChatClient 进 context（与 invoke_tool 一致）：让 around_chat filter 可经
            %% beamai_context:get_chat_client/1 拿到 ChatClient 做组合（如调工具/查 specs）。
            Context = beamai_context:with_chat_client(
                        maps:get(context, Opts, beamai_context:new()), ChatClient),
            %% system_prompts 作为临时最内层 chat filter 注入（追加在列表尾 =
            %% 最内层）：在全部 filter 之后、LLM 之前前置系统消息，不入存储。
            SystemPrompts = maps:get(system_prompts, Opts, []),
            Filters = Filters0 ++ system_prompt_filter(SystemPrompts),
            run_chat(LlmConfig, Filters, Messages, Opts, Context);
        error ->
            {error, no_chat_model}
    end.

%% @doc 流式 Chat Completion（经完整 around_chat 链）
%%
%% 与 invoke_chat/3 走**同样的** chat + llm 两层洋葱（Memory / system_prompt 等
%% 行为完全一致），区别仅在最内层 terminal 调用 provider 的 stream_chat：流式 token
%% 经 TokenCallback 实时回传，链最终仍返回汇聚后的统一响应（供 Memory filter
%% 落库、供工具循环判定 tool_calls）。
%%
%% 要求 provider 的 stream_chat 返回汇聚后的统一 beamai_chat_response。
%%
%% == 两条出站通道 ==
%%
%% 1. **TokenCallback** —— 归一化后的文本 token（经 token_transform 链）。
%% 2. **opts 里的 `on_raw_event`** —— provider **原样解出的** SSE chunk，不归一化。
%%
%% 通道 2 存在的理由正是通道 1 与统一响应都覆盖不到的那些东西：tool_calls 的
%% arguments 增量、thinking / reasoning 增量、逐块 usage。它们各家形状不同
%% （binary 键），要用就得按 provider 分支解读——这是刻意的：一旦在这里归一化，
%% 就等于替所有宿主决定"哪些字段值得保留"，而这条通道的用户（前端事件流、
%% 审计、trace）要的恰恰是没被裁剪过的原文。
%%
%% @param ChatClient ChatClient 实例
%% @param Messages 消息列表
%% @param Opts Chat 选项（同 invoke_chat/3，另可给 `on_raw_event`）
%% @param TokenCallback fun((Token :: binary(), Meta :: map()) -> ok)，逐 token 回调
%% @returns {ok, 响应 Map, 更新后上下文} | {error, 原因}
-spec invoke_chat_stream(chat_client(), [map()], chat_opts(),
                         fun((binary(), map()) -> ok)) ->
    {ok, map(), beamai_context:t()} | {error, term()}.
invoke_chat_stream(ChatClient, Messages, Opts, TokenCallback) ->
    case chat_model(ChatClient) of
        {ok, LlmConfig} ->
            #{filters := Filters0} = ChatClient,
            Context = beamai_context:with_chat_client(
                        maps:get(context, Opts, beamai_context:new()), ChatClient),
            SystemPrompts = maps:get(system_prompts, Opts, []),
            Filters = Filters0 ++ system_prompt_filter(SystemPrompts),
            run_chat_stream(LlmConfig, Filters, Messages, Opts, Context, TokenCallback);
        error ->
            {error, no_chat_model}
    end.

%%====================================================================
%% Query API
%%====================================================================

%% @doc 取出工具注册表（交给 beamai_tool_registry / beamai_tool_executor 用）
%%
%% 对应 Spring 把 defaultTools 放进 ChatOptions 交给 ToolCallingManager：ChatClient
%% 只交出自己持有的东西，「有哪些工具、schema 是什么、能不能并发」一律问
%% beamai_tool_registry。
-spec tools(chat_client()) -> beamai_tool_registry:t().
tools(#{tools := Tools}) -> Tools.

%% @doc 取出 filter 链（四条链共用一份列表，注册顺序即层序）
-spec filters(chat_client()) -> [beamai_filter:filter()].
filters(#{filters := Filters}) -> Filters.

%% @doc 获取 ChatClient 的 LLM 服务配置
%%
%% 未配置 LLM 时返回 error。
-spec chat_model(chat_client()) -> {ok, beamai_chat_behaviour:config()} | error.
chat_model(#{chat_model := undefined}) -> error;
chat_model(#{chat_model := Model}) -> {ok, Model}.

%% @doc 获取 ChatClient 的状态槽声明（未配置返回 #{}）
%%
%% 供 tool 批次折叠工具 writes 时按槽路由 reducer（见 beamai_context:apply_writes/3）。
-spec state_slots(chat_client()) -> beamai_context:state_slots().
state_slots(#{settings := Settings}) -> maps:get(state_slots, Settings, #{});
state_slots(_) -> #{}.

%% @private provider 原始流事件的汇：宿主经 opts 的 on_raw_event 登记
%%
%% 未登记即空操作（零开销退化）。异常吞掉并记 warning：sink 是旁路观察者，
%% 不该让它把正在进行的流打断——与 agent 侧回调"永不打断主流程"同一约定。
raw_event_sink(Opts) ->
    case maps:get(on_raw_event, Opts, undefined) of
        Fun when is_function(Fun, 1) ->
            fun(Event) ->
                try
                    _ = Fun(Event),
                    ok
                catch
                    Class:Reason:Stack ->
                        logger:warning("beamai_chat_client on_raw_event crashed: ~p:~p",
                                       [Class, Reason], #{stacktrace => Stack}),
                        ok
                end
            end;
        _ ->
            fun(_Event) -> ok end
    end.

%%====================================================================
%% 内部函数 - 辅助
%%====================================================================

%% @private 构造 system_prompts 临时注入 filter（仅 around_chat）
%%
%% invoke 时追加在 filters 列表尾 = **最内层**：在全部 filter 之后、LLM 之前
%% 前置系统消息，且不写入存储。memory filter（列表首位、最外层）先展开完整
%% 历史，系统提示在最内层才注入，故永远不会被存进历史。
system_prompt_filter([]) ->
    [];
system_prompt_filter(SystemPrompts) ->
    [beamai_filter:new(<<"system_prompt">>, #{
        around_chat => fun(#{messages := Msgs} = Req, _FCtx, Next) ->
            Next(Req#{messages => SystemPrompts ++ Msgs})
        end
    })].

%% @private 运行 chat 洋葱（around_chat 链 → LLM 调用）
%%
%% Request `#{messages, context, opts}` → Response `#{response, context}`，
%% 最内层 terminal 为真正的 LLM 调用（其内部的重试对本链不可见）。
run_chat(LlmConfig, Filters, Messages, Opts, Context) ->
    Req = #{messages => Messages, context => Context, opts => Opts},
    case beamai_filter_chain:run(Filters, around_chat, chat_terminal(LlmConfig), Req) of
        {ok, #{response := Response, context := Ctx}} -> {ok, Response, Ctx};
        {error, _} = Err -> Err
    end.

%% @private chat 链最内层：真正调用 LLM（出错时 throw，由链统一捕获）
chat_terminal(LlmConfig) ->
    Module = maps:get(module, LlmConfig, beamai_chat_model),
    fun(#{messages := Messages, opts := Opts, context := Ctx}) ->
        case Module:chat(LlmConfig, Messages, Opts) of
            {ok, Response} -> #{response => Response, context => Ctx};
            {error, Reason} -> throw(Reason)
        end
    end.

%% @private 运行流式 chat 洋葱（与 run_chat 同链，仅最内层 terminal 不同）
%%
%% filters 上声明的 token_transform 在 terminal 内按注册顺序组装成 token 变换链，
%% 作用于送往 TokenCallback 的出站流；最终归一化响应不经过它。
%%
%% Req 带 `stream => true`：供 chat filter 判定本次是不是流式（流式路径没有重试——
%% token 已投递出去，重跑会让下游看到重复内容）。
run_chat_stream(LlmConfig, Filters, Messages, Opts, Context, TokenCallback) ->
    Req = #{messages => Messages, context => Context, opts => Opts, stream => true},
    TokenXfs = lists:filtermap(fun(F) ->
        case beamai_filter:hook(F, token_transform) of
            undefined -> false;
            Xf -> {true, Xf}
        end
    end, Filters),
    Terminal = stream_chat_terminal(LlmConfig, TokenXfs, TokenCallback),
    case beamai_filter_chain:run(Filters, around_chat, Terminal, Req) of
        {ok, #{response := Response, context := Ctx}} -> {ok, Response, Ctx};
        {error, _} = Err -> Err
    end.

%% @private 流式 chat 链最内层：调用 provider stream_chat，token 经回调实时回传，
%% 返回汇聚后的统一响应（出错时 throw，由链统一捕获）。
%%
%% token_transform 链在 terminal **每次执行**时现场实例化（chat filter 重入 Next
%% 时每次流各自新状态）；Flush 只在 stream_chat 正常返回后调一次——错误路径
%% 不 flush（缓冲丢弃，半截答案不外泄）。
stream_chat_terminal(LlmConfig, TokenXfs, TokenCallback) ->
    Module = maps:get(module, LlmConfig, beamai_chat_model),
    fun(#{messages := Messages, opts := Opts, context := Ctx}) ->
        %% on_llm_new_token 由 beamai_chat_model 的流式包装识别并逐 token 调用；
        %% 原始 event 回调转交宿主登记的 on_raw_event（未登记即空操作）。
        %%
        %% Opts 取自**链上的请求**而非入口参数：filter 改写过的 opts 在这里生效，
        %% 于是 filter 也能给这次流临时挂上／摘掉 raw 事件汇。
        {WrappedCb, Flush} = beamai_token_stream:wrap(TokenXfs, TokenCallback),
        RawCb = raw_event_sink(Opts),
        %% on_raw_event 是 ChatClient 层自己消费的选项，剥掉再下发——provider 只该
        %% 看到与这次请求有关的模型参数（同 beamai_chat_model 的 MODEL_LEVEL_OPTS）。
        StreamOpts = maps:without([on_raw_event], Opts#{on_llm_new_token => WrappedCb}),
        case Module:stream_chat(LlmConfig, Messages, RawCb, StreamOpts) of
            {ok, Response} ->
                ok = Flush(),
                #{response => Response, context => Ctx};
            {error, Reason} ->
                throw(Reason)
        end
    end.
