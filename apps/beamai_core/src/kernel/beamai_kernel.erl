%%%-------------------------------------------------------------------
%%% @doc Kernel 核心：基础设施（工具管理、LLM 服务、Filter）
%%%
%%% Kernel 是框架的基础设施层，只提供两类原子能力——单次 LLM 调用
%%% （invoke_chat）与单次工具调用（invoke_tool），各自经过洋葱式 Filter 链。
%%% 它**不**负责 ReAct 工具调用循环（LLM ↔ Tool 的多轮编排是 Agent 的职责，
%%% 见 beamai_agent / beamai_agent_tool_loop）。
%%%
%%% 职责：
%%% - 管理工具注册
%%% - 持有 LLM 服务配置
%%% - 执行洋葱式 Filter 链（chat / tool 各自前后一对 hook）
%%% - invoke_chat：单次 Chat Completion（经 around_chat 链）
%%% - invoke_tool：单次工具执行（经 around_tool 链）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_kernel).

%% Build API
-export([new/0, new/1, new/2]).
-export([add_tool/2]).
-export([add_tools/2]).
-export([add_tool_module/2]).
-export([add_service/2]).

%% Invoke API（仅单次 chat / tool；ReAct 循环属于 Agent 层）
-export([invoke_tool/4]).
-export([invoke_chat/3]).
-export([invoke_chat_stream/4]).

%% Query API
-export([get_tool/2]).
-export([list_tools/1]).
-export([get_tools_by_tag/2]).
-export([get_tool_specs/1]).
-export([get_tool_schemas/1, get_tool_schemas/2]).
-export([get_service/1]).
-export([state_slots/1]).
-export([serial_tool/2]).
-export([return_direct_tool/2]).

%% Types
-export_type([kernel/0, kernel_settings/0, chat_opts/0]).

-type kernel() :: #{
    '__kernel__' := true,
    tools := #{binary() => beamai_tool:tool_spec()},
    llm_config := beamai_chat_behaviour:config() | undefined,
    filters := [beamai_filter:filter()],
    settings := kernel_settings()
}.

-type kernel_settings() :: #{
    default_timeout => pos_integer(),
    atom() => term()
}.

-type chat_opts() :: #{
    tools => [map()],
    tool_choice => auto | none | required,
    context => beamai_context:t(),
    system_prompts => [map()],
    atom() => term()
}.

%% 状态槽声明（存于 settings.state_slots）：工具 writes 折叠进 state 时，
%% 声明槽过其 reducer，未声明槽 last-writer（见 beamai_context:apply_writes/3）。

%%====================================================================
%% Build API
%%====================================================================

%% @doc 创建空 Kernel（默认配置，无 filter）
-spec new() -> kernel().
new() ->
    new(#{}, []).

%% @doc 创建 Kernel（自定义配置，无 filter）
%%
%% @param Settings 配置项（如 #{default_timeout => 30000}）
%% @returns Kernel 实例
-spec new(kernel_settings()) -> kernel().
new(Settings) ->
    new(Settings, []).

%% @doc 创建 Kernel（自定义配置 + 一次性给出全量 filter）
%%
%% Filters 在构建时一次性给出，**注册顺序即层序**：列表靠前 = 外层
%% （前置先执行、后置后执行）。构建后不可增量追加——需要记忆时把
%% `beamai_memory_filter:memory_filter(Store)` 放在列表**首位**（最外层：
%% 先展开完整历史，再让内层 filter 处理）。
%%
%% @param Settings 配置项
%% @param Filters filter 列表（beamai_filter:new/2,3 创建）
%% @returns Kernel 实例
-spec new(kernel_settings(), [beamai_filter:filter()]) -> kernel().
new(Settings, Filters) when is_map(Settings), is_list(Filters) ->
    #{
        '__kernel__' => true,
        tools => #{},
        llm_config => undefined,
        filters => Filters,
        settings => Settings
    }.

%% @doc 注册工具到 Kernel
%%
%% 工具以其名称为键存入 tools Map。重名工具会被覆盖。
%%
%% @param Kernel Kernel 实例
%% @param Tool 工具定义（需包含 name 字段）
%% @returns 更新后的 Kernel
-spec add_tool(kernel(), beamai_tool:tool_spec()) -> kernel().
add_tool(#{tools := Tools} = Kernel, #{name := Name} = Tool) ->
    Kernel#{tools => Tools#{Name => Tool}}.

%% @doc 批量注册工具到 Kernel
%%
%% @param Kernel Kernel 实例
%% @param ToolList 工具定义列表
%% @returns 更新后的 Kernel
-spec add_tools(kernel(), [beamai_tool:tool_spec()]) -> kernel().
add_tools(Kernel, ToolList) ->
    lists:foldl(fun(Tool, K) -> add_tool(K, Tool) end, Kernel, ToolList).

%% @doc 从模块自动加载并注册工具
%%
%% 模块需实现 beamai_tool_behaviour，至少实现 tools/0 回调。
%% 加载失败时抛出 {tool_module_load_failed, Module, Reason} 错误。
%% 注：模块只提供工具；filter 一律在 new/2 构建时一次性给出。
%%
%% @param Kernel Kernel 实例
%% @param Module 实现了工具回调的模块
%% @returns 更新后的 Kernel
-spec add_tool_module(kernel(), module()) -> kernel().
add_tool_module(Kernel, Module) ->
    case beamai_tool:from_module(Module) of
        {ok, Tools} ->
            add_tools(Kernel, Tools);
        {error, Reason} ->
            erlang:error({tool_module_load_failed, Module, Reason})
    end.

%% @doc 设置 LLM 服务配置
%%
%% 配置通过 beamai_chat_completion:create/2 创建。
%% 设置后可使用 invoke_chat/3。
%%
%% @param Kernel Kernel 实例
%% @param LlmConfig LLM 配置 Map
%% @returns 更新后的 Kernel
-spec add_service(kernel(), beamai_chat_behaviour:config()) -> kernel().
add_service(Kernel, LlmConfig) ->
    Kernel#{llm_config => LlmConfig}.

%%====================================================================
%% Invoke API
%%====================================================================

%% @doc 调用 Kernel 中注册的工具
%%
%% 执行流程：查找工具 → tool filter 洋葱链（around_tool：前置改写参数 → 工具
%% 执行 → 后置改写结果）。上下文会自动关联当前 Kernel 引用。
%%
%% Context 为只读运行环境（自动绑定当前 Kernel 引用）；工具写状态经返回值
%% 的 Writes 表达（第三元），本函数原样透出，由调用方（tool 批次）折叠进 state。
%%
%% @param Kernel Kernel 实例
%% @param ToolName 工具名称
%% @param Args 调用参数
%% @param Context 执行上下文（只读环境）
%% @returns {ok, 结果, Writes} | {error, 原因}
-spec invoke_tool(kernel(), binary(), beamai_tool:args(), beamai_context:t()) ->
    {ok, term(), beamai_context:writes()} | {error, term()}.
invoke_tool(#{filters := Filters} = Kernel, ToolName, Args, Context0) ->
    case get_tool(Kernel, ToolName) of
        {ok, ToolSpec} ->
            Context = beamai_context:with_kernel(Context0, Kernel),
            run_tool(Filters, ToolSpec, Args, Context);
        error ->
            {error, {tool_not_found, ToolName}}
    end.

%% @doc 发送 Chat Completion 请求（不含工具调用循环）
%%
%% 执行流程：chat filter 洋葱链（around_chat：前置改写请求 → LLM 调用 → 后置改写响应）。
%% Kernel 需先通过 add_service/2 配置 LLM。
%%
%% Opts 可含 system_prompts：作为临时**最内层** filter 注入（追加在全量 filter
%% 之后），在所有 filter 之后、LLM 之前前置系统消息且不入存储——memory filter
%% 展开的历史永远不含系统提示，用户 chat filter 看到的 messages 也不含系统提示。
%%
%% @param Kernel Kernel 实例
%% @param Messages 消息列表（[#{role => ..., content => ...}]）
%% @param Opts Chat 选项
%% @returns {ok, 响应 Map, 更新后上下文} | {error, 原因}
-spec invoke_chat(kernel(), [map()], chat_opts()) ->
    {ok, map(), beamai_context:t()} | {error, term()}.
invoke_chat(Kernel, Messages, Opts) ->
    case get_service(Kernel) of
        {ok, LlmConfig} ->
            #{filters := Filters0} = Kernel,
            %% 绑 kernel 进 context（与 invoke_tool 一致）：让 around_chat filter 可经
            %% beamai_context:get_kernel/1 拿到 kernel 做组合（如调工具/查 specs）。
            Context = beamai_context:with_kernel(
                        maps:get(context, Opts, beamai_context:new()), Kernel),
            %% system_prompts 作为临时最内层 chat filter 注入（追加在列表尾 =
            %% 最内层）：在全部 filter 之后、LLM 之前前置系统消息，不入存储。
            SystemPrompts = maps:get(system_prompts, Opts, []),
            Filters = Filters0 ++ system_prompt_filter(SystemPrompts),
            run_chat(LlmConfig, Filters, Messages, Opts, Context);
        error ->
            {error, no_llm_service}
    end.

%% @doc 流式 Chat Completion（经完整 around_chat 链）
%%
%% 与 invoke_chat/3 走**同一条** filter 洋葱链（Memory / system_prompt 等行为
%% 完全一致），区别仅在最内层 terminal 调用 provider 的 stream_chat：流式 token
%% 经 TokenCallback 实时回传，链最终仍返回汇聚后的统一响应（供 Memory filter
%% 落库、供工具循环判定 tool_calls）。
%%
%% 要求 provider 的 stream_chat 返回汇聚后的统一 beamai_llm_response。
%%
%% @param Kernel Kernel 实例
%% @param Messages 消息列表
%% @param Opts Chat 选项（同 invoke_chat/3）
%% @param TokenCallback fun((Token :: binary(), Meta :: map()) -> ok)，逐 token 回调
%% @returns {ok, 响应 Map, 更新后上下文} | {error, 原因}
-spec invoke_chat_stream(kernel(), [map()], chat_opts(),
                         fun((binary(), map()) -> ok)) ->
    {ok, map(), beamai_context:t()} | {error, term()}.
invoke_chat_stream(Kernel, Messages, Opts, TokenCallback) ->
    case get_service(Kernel) of
        {ok, LlmConfig} ->
            #{filters := Filters0} = Kernel,
            Context = beamai_context:with_kernel(
                        maps:get(context, Opts, beamai_context:new()), Kernel),
            SystemPrompts = maps:get(system_prompts, Opts, []),
            Filters = Filters0 ++ system_prompt_filter(SystemPrompts),
            run_chat_stream(LlmConfig, Filters, Messages, Opts, Context, TokenCallback);
        error ->
            {error, no_llm_service}
    end.

%%====================================================================
%% Query API
%%====================================================================

%% @doc 按名称查找 Kernel 中注册的工具
%%
%% @param Kernel Kernel 实例
%% @param ToolName 工具名称
%% @returns {ok, 工具定义} | error
-spec get_tool(kernel(), binary()) -> {ok, beamai_tool:tool_spec()} | error.
get_tool(#{tools := Tools}, ToolName) ->
    maps:find(ToolName, Tools).

%% @doc 列出 Kernel 中所有注册的工具
-spec list_tools(kernel()) -> [beamai_tool:tool_spec()].
list_tools(#{tools := Tools}) ->
    maps:values(Tools).

%% @doc 按标签查找工具
%%
%% @param Kernel Kernel 实例
%% @param Tag 标签
%% @returns 匹配的工具列表
-spec get_tools_by_tag(kernel(), binary()) -> [beamai_tool:tool_spec()].
get_tools_by_tag(#{tools := Tools}, Tag) ->
    [T || T <- maps:values(Tools), beamai_tool:has_tag(T, Tag)].

%% @doc 获取所有工具的统一 tool spec 列表
%%
%% 返回包含 name、description、parameters 的中间格式。
-spec get_tool_specs(kernel()) -> [map()].
get_tool_specs(Kernel) ->
    Tools = list_tools(Kernel),
    [beamai_tool:to_tool_spec(T) || T <- Tools].

%% @doc 获取所有工具的 tool schema（默认 OpenAI 格式）
-spec get_tool_schemas(kernel()) -> [map()].
get_tool_schemas(Kernel) ->
    get_tool_schemas(Kernel, openai).

%% @doc 获取所有工具的 tool schema（指定提供商格式）
%%
%% @param Kernel Kernel 实例
%% @param Provider 提供商标识（openai | anthropic）
%% @returns tool schema 列表
-spec get_tool_schemas(kernel(), openai | anthropic | atom()) -> [map()].
get_tool_schemas(Kernel, Provider) ->
    Tools = list_tools(Kernel),
    [beamai_tool:to_tool_schema(T, Provider) || T <- Tools].

%% @doc 获取 Kernel 的 LLM 服务配置
%%
%% 未配置 LLM 时返回 error。
-spec get_service(kernel()) -> {ok, beamai_chat_behaviour:config()} | error.
get_service(#{llm_config := undefined}) -> error;
get_service(#{llm_config := Config}) -> {ok, Config}.

%% @doc 获取 Kernel 的状态槽声明（未配置返回 #{}）
%%
%% 供 tool 批次折叠工具 writes 时按槽路由 reducer（见 beamai_context:apply_writes/3）。
-spec state_slots(kernel()) -> beamai_context:state_slots().
state_slots(#{settings := Settings}) -> maps:get(state_slots, Settings, #{});
state_slots(_) -> #{}.

%% @doc 按工具名查询该工具是否标记为串行（有副作用、需顺序执行）
%%
%% 未注册的工具名返回 false（不因未知工具强制整批退化）。
-spec serial_tool(kernel(), binary()) -> boolean().
serial_tool(Kernel, ToolName) ->
    case get_tool(Kernel, ToolName) of
        {ok, ToolSpec} -> beamai_tool:is_serial(ToolSpec);
        error -> false
    end.

%% @doc 按工具名查询该工具结果是否直接作为最终答案（不回灌模型）
%%
%% 未注册的工具名返回 false：未知工具不该触发直返（直返会终止循环、丢弃
%% 同批其余结果，未知名字上取保守值）。
-spec return_direct_tool(kernel(), binary()) -> boolean().
return_direct_tool(Kernel, ToolName) ->
    case get_tool(Kernel, ToolName) of
        {ok, ToolSpec} -> beamai_tool:is_return_direct(ToolSpec);
        error -> false
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

%% @private 运行 chat filter 洋葱链（用 around_chat hook）
%%
%% Request `#{messages, context, opts}` → Response `#{response, context}`，
%% 最内层 terminal 为真正的 LLM 调用。
run_chat(LlmConfig, Filters, Messages, Opts, Context) ->
    Req = #{messages => Messages, context => Context, opts => Opts},
    Terminal = chat_terminal(LlmConfig),
    case beamai_filter_chain:run(Filters, around_chat, Terminal, Req) of
        {ok, #{response := Response, context := Ctx}} -> {ok, Response, Ctx};
        {error, _} = Err -> Err
    end.

%% @private chat 链最内层：真正调用 LLM（出错时 throw，由链统一捕获）
chat_terminal(LlmConfig) ->
    Module = maps:get(module, LlmConfig, beamai_chat_completion),
    fun(#{messages := Messages, opts := Opts, context := Ctx}) ->
        case Module:chat(LlmConfig, Messages, Opts) of
            {ok, Response} -> #{response => Response, context => Ctx};
            {error, Reason} -> throw(Reason)
        end
    end.

%% @private 运行流式 chat filter 洋葱链（与 run_chat 同链，仅 terminal 不同）
%%
%% filters 上声明的 token_transform（第四钩子）在 terminal 内按注册顺序组装成
%% token 变换链，作用于送往 TokenCallback 的出站流；最终归一化响应不经过它。
run_chat_stream(LlmConfig, Filters, Messages, Opts, Context, TokenCallback) ->
    Req = #{messages => Messages, context => Context, opts => Opts},
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
%% token_transform 链在 terminal **每次执行**时现场实例化（chat filter 重入 Next 时
%% 每次流各自新状态）；Flush 只在 stream_chat 正常返回后调一次——错误路径
%% 不 flush（缓冲丢弃，半截答案不外泄）。
stream_chat_terminal(LlmConfig, TokenXfs, TokenCallback) ->
    Module = maps:get(module, LlmConfig, beamai_chat_completion),
    fun(#{messages := Messages, opts := Opts, context := Ctx}) ->
        %% on_llm_new_token 由 beamai_chat_completion 的流式包装识别并逐 token 调用；
        %% 原始 event 回调用空操作（统一响应由 stream_chat 返回值给出）。
        {WrappedCb, Flush} = beamai_token_stream:wrap(TokenXfs, TokenCallback),
        StreamOpts = Opts#{on_llm_new_token => WrappedCb},
        case Module:stream_chat(LlmConfig, Messages, fun(_Event) -> ok end, StreamOpts) of
            {ok, Response} ->
                ok = Flush(),
                #{response => Response, context => Ctx};
            {error, Reason} ->
                throw(Reason)
        end
    end.

%% @private 运行 tool filter 洋葱链（用 around_tool hook）
%%
%% Request `#{tool, args, context}` → Response `#{result, writes, context}`，
%% 最内层 terminal 为真正的工具执行。`writes` 为工具写意图（纯数据），透出给
%% 调用方折叠进 state；`context` 仅承载 filter 私有状态合并（框架用）。
run_tool(Filters, ToolSpec, Args, Context) ->
    Req = #{tool => ToolSpec, args => Args, context => Context},
    Terminal = tool_terminal(),
    case beamai_filter_chain:run(Filters, around_tool, Terminal, Req) of
        {ok, #{result := Value} = Resp} -> {ok, Value, maps:get(writes, Resp, #{})};
        {error, _} = Err -> Err
    end.

%% @private tool 链最内层：真正执行工具（出错时 throw，由链统一捕获）
%%
%% 归一工具返回：`{ok,V}` → 空 writes；`{ok,V,W}` → W 为写意图。
%% Context 只读透传（filter 私有状态由链在外层合并）。
tool_terminal() ->
    fun(#{tool := ToolSpec, args := Args, context := Ctx}) ->
        case beamai_tool:invoke(ToolSpec, Args, Ctx) of
            {ok, Value} -> #{result => Value, writes => #{}, context => Ctx};
            {ok, Value, Writes} -> #{result => Value, writes => Writes, context => Ctx};
            {error, Reason} -> throw(Reason)
        end
    end.
