%%%-------------------------------------------------------------------
%%% @doc Kernel 核心：工具管理、LLM 服务、Filter、工具调用循环
%%%
%%% Kernel 是框架的中枢，负责：
%%% - 管理工具注册
%%% - 持有 LLM 服务配置
%%% - 执行洋葱式 Filter 链（chat / tool 各自前后一对 hook）
%%% - 驱动工具调用循环（LLM ↔ Tool）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_kernel).

%% Build API
-export([new/0, new/1]).
-export([add_tool/2]).
-export([add_tools/2]).
-export([add_tool_module/2]).
-export([add_service/2]).
-export([add_filter/2]).
-export([with_memory/2]).

%% Invoke API
-export([invoke/3]).
-export([invoke_tool/4]).
-export([invoke_chat/3]).

%% Query API
-export([get_tool/2]).
-export([list_tools/1]).
-export([get_tools_by_tag/2]).
-export([get_tool_specs/1]).
-export([get_tool_schemas/1, get_tool_schemas/2]).
-export([get_service/1]).

%% Types
-export_type([kernel/0, kernel_settings/0, chat_opts/0]).

-type kernel() :: #{
    '__kernel__' := true,
    tools := #{binary() => beamai_tool:tool_spec()},
    llm_config := beamai_chat_behaviour:config() | undefined,
    filters := [beamai_filter:filter()],
    memory := beamai_chat_memory:handle() | undefined,
    settings := kernel_settings()
}.

-type kernel_settings() :: #{
    max_tool_iterations => pos_integer(),
    default_timeout => pos_integer(),
    atom() => term()
}.

-type chat_opts() :: #{
    tools => [map()],
    tool_choice => auto | none | required,
    max_tool_iterations => pos_integer(),
    context => beamai_context:t(),
    system_prompts => [map()],
    atom() => term()
}.

%%====================================================================
%% Build API
%%====================================================================

%% @doc 创建空 Kernel（默认配置）
-spec new() -> kernel().
new() ->
    new(#{}).

%% @doc 创建 Kernel（自定义配置）
%%
%% @param Settings 配置项（如 #{max_tool_iterations => 5}）
%% @returns Kernel 实例
-spec new(kernel_settings()) -> kernel().
new(Settings) ->
    #{
        '__kernel__' => true,
        tools => #{},
        llm_config => undefined,
        filters => [],
        memory => undefined,
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
%%
%% @param Kernel Kernel 实例
%% @param Module 实现了工具回调的模块
%% @returns 更新后的 Kernel
-spec add_tool_module(kernel(), module()) -> kernel().
add_tool_module(Kernel, Module) ->
    case beamai_tool:from_module(Module) of
        {ok, Tools} ->
            %% 如果模块实现了 filters/0，也注册 filter
            K1 = add_tools(Kernel, Tools),
            maybe_add_filters(K1, Module);
        {error, Reason} ->
            erlang:error({tool_module_load_failed, Module, Reason})
    end.

%% @private 如果模块实现了 filters/0，添加 filter
maybe_add_filters(Kernel, Module) ->
    case erlang:function_exported(Module, filters, 0) of
        true ->
            Filters = Module:filters(),
            lists:foldl(fun(F, K) -> add_filter(K, F) end, Kernel, Filters);
        false ->
            Kernel
    end.

%% @doc 设置 LLM 服务配置
%%
%% 配置通过 beamai_chat_completion:create/2 创建。
%% 设置后可使用 invoke_chat/3 和 invoke/3。
%%
%% @param Kernel Kernel 实例
%% @param LlmConfig LLM 配置 Map
%% @returns 更新后的 Kernel
-spec add_service(kernel(), beamai_chat_behaviour:config()) -> kernel().
add_service(Kernel, LlmConfig) ->
    Kernel#{llm_config => LlmConfig}.

%% @doc 注册 filter 到 Kernel
%%
%% filter 追加到现有列表，执行时按 order 排序（越小越外层）。
%% 一个 filter 可含 around_chat/around_tool 任意子集，
%% chat 链用其 around_chat、tool 链用其 around_tool。
%%
%% @param Kernel Kernel 实例
%% @param Filter filter 定义（通过 beamai_filter:new/2,3,4 创建）
%% @returns 更新后的 Kernel
-spec add_filter(kernel(), beamai_filter:filter()) -> kernel().
add_filter(#{filters := Filters} = Kernel, Filter) ->
    Kernel#{filters => Filters ++ [Filter]}.

%% @doc 启用会话记忆：绑定 store 句柄并挂载 Memory filter
%%
%% 挂载后，invoke/3 进入 delta 模式：每轮只把新消息交给 chat filter 链，
%% 由 Memory filter（around_chat：存+展开历史、调 LLM、存回复）按 conversation_id
%% 管理历史。未启用记忆时 invoke/3 为单次无状态调用（工具循环内部本地累积）。
%%
%% @param Kernel Kernel 实例
%% @param Store 会话存储句柄（beamai_chat_memory:handle/0，如
%%        beamai_chat_memory_ets:handle(Name)）
%% @returns 更新后的 Kernel
-spec with_memory(kernel(), beamai_chat_memory:handle()) -> kernel().
with_memory(Kernel, Store) ->
    K1 = add_filter(Kernel, beamai_memory_filter:memory_filter(Store)),
    K1#{memory => Store}.

%%====================================================================
%% Invoke API
%%====================================================================

%% @doc 调用 Kernel 中注册的工具
%%
%% 执行流程：查找工具 → tool filter 洋葱链（around_tool：前置改写参数 → 工具
%% 执行 → 后置改写结果）。上下文会自动关联当前 Kernel 引用。
%%
%% @param Kernel Kernel 实例
%% @param ToolName 工具名称
%% @param Args 调用参数
%% @param Context 执行上下文
%% @returns {ok, 结果, 更新后上下文} | {error, 原因}
-spec invoke_tool(kernel(), binary(), beamai_tool:args(), beamai_context:t()) ->
    {ok, term(), beamai_context:t()} | {error, term()}.
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
%% @param Kernel Kernel 实例
%% @param Messages 消息列表（[#{role => ..., content => ...}]）
%% @param Opts Chat 选项
%% @returns {ok, 响应 Map, 更新后上下文} | {error, 原因}
-spec invoke_chat(kernel(), [map()], chat_opts()) ->
    {ok, map(), beamai_context:t()} | {error, term()}.
invoke_chat(Kernel, Messages, Opts) ->
    case get_service(Kernel) of
        {ok, LlmConfig} ->
            #{filters := Filters} = Kernel,
            Context = maps:get(context, Opts, beamai_context:new()),
            run_chat(LlmConfig, Filters, Messages, Opts, Context);
        error ->
            {error, no_llm_service}
    end.

%% @doc 发送 Chat Completion 请求并驱动工具调用循环
%%
%% 自动将 Kernel 中所有注册工具转为 tool specs 传给 LLM。
%% LLM 返回 tool_calls 时自动执行对应工具，将结果拼入消息后再次请求 LLM，
%% 循环直到 LLM 返回文本响应或达到最大迭代次数。
%%
%% Kernel 不再累积消息：每次只传入本轮新消息（delta）。
%% - 启用记忆（with_memory/2）时进入 delta 模式：每轮只把新消息交给
%%   pipeline，由 Memory 过滤器按 conversation_id 存储并展开完整历史。
%%   context 无 conversation_id 时生成临时 id，invoke 结束后清理。
%% - 未启用记忆时为 full 模式：工具循环内部本地累积消息，不跨 invoke 持久化。
%% system_prompts 作为临时 chat filter(order -500，内层) 的 around_chat 注入，不进入存储。
%%
%% @param Kernel Kernel 实例（需注册工具和 LLM 服务）
%% @param Messages 本轮新消息（通常是单条用户消息）
%% @param Opts Chat 选项（可设置 system_prompts、max_tool_iterations、tool_choice、context）
%% @returns {ok, 最终响应 Map, 更新后上下文} | {error, 原因}
-spec invoke(kernel(), [map()], chat_opts()) ->
    {ok, map(), beamai_context:t()} | {error, term()}.
invoke(Kernel, Messages, Opts) ->
    case get_service(Kernel) of
        {ok, LlmConfig} ->
            #{filters := Filters0, memory := Memory} = Kernel,
            ToolSpecs = get_tool_specs(Kernel),
            ChatOpts = Opts#{tools => ToolSpecs, tool_choice => maps:get(tool_choice, Opts, auto)},
            Context0 = maps:get(context, Opts, beamai_context:new()),
            SystemPrompts = maps:get(system_prompts, Opts, []),
            %% system_prompts 临时注入 filter（不入存储），与已注册 filter 合并
            Filters = Filters0 ++ system_prompt_filter(SystemPrompts),
            MaxIter = maps:get(max_tool_iterations, Opts,
                maps:get(max_tool_iterations, maps:get(settings, Kernel, #{}), 10)),
            %% 根据是否启用记忆决定模式与会话标识
            {Mode, Context, ConvId, Ephemeral} = resolve_conversation(Context0, Memory),
            try
                tool_calling_loop(Kernel, LlmConfig, Filters, Messages, ChatOpts, Context, Mode, MaxIter)
            after
                cleanup_ephemeral(Memory, ConvId, Ephemeral)
            end;
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

%%====================================================================
%% 内部函数 - 工具调用循环
%%====================================================================

%% @private 工具调用循环主体
%%
%% 每次迭代通过 chat filter 洋葱链调用 LLM，确保 filter 的 around_chat 生效。
%% LLM 返回 tool_calls 时：解析调用 → 执行工具 → 拼接结果 → 再次请求 LLM。
%% 迭代次数耗尽返回 max_tool_iterations 错误。
%% LLM 返回纯文本响应时终止循环。
%%
%% Mode 决定每轮传给 chat 链的消息：
%% - delta：只传本轮新消息，由 Memory filter 存储并展开完整历史。
%%   assistant 回复由 Memory filter 的 around_chat 存储，下一轮 delta = 工具结果。
%% - full：本地累积完整对话，每轮传全量（无记忆时保证工具循环上下文连续）。
tool_calling_loop(_Kernel, _LlmConfig, _Filters, _Msgs, _Opts, _Context, _Mode, 0) ->
    {error, max_tool_iterations};
tool_calling_loop(Kernel, LlmConfig, Filters, Msgs, Opts, Context, Mode, N) ->
    case run_chat(LlmConfig, Filters, Msgs, Opts, Context) of
        {ok, Response, Ctx0} ->
            %% 使用 beamai_llm_response 访问器统一处理响应
            case beamai_llm_response:has_tool_calls(Response) of
                true ->
                    TCs = beamai_llm_response:tool_calls(Response),
                    {ToolResults, Ctx1} = execute_tool_calls(Kernel, TCs, Ctx0),
                    NextMsgs = next_messages(Mode, Msgs, Response, ToolResults),
                    tool_calling_loop(Kernel, LlmConfig, Filters, NextMsgs, Opts, Ctx1, Mode, N - 1);
                false ->
                    {ok, Response, Ctx0}
            end;
        {error, _} = Err ->
            Err
    end.

%% @private 计算下一轮消息
%% - delta 模式：下一轮只需工具结果（assistant 回复已由 Memory 过滤器存储）
%% - full 模式：本地拼接 assistant 回复 + 工具结果到完整对话末尾
next_messages(delta, _Msgs, _Response, ToolResults) ->
    ToolResults;
next_messages(full, Msgs, Response, ToolResults) ->
    AssistantMsg = beamai_message:tool_calls(beamai_llm_response:tool_calls(Response)),
    Msgs ++ [AssistantMsg | ToolResults].

%% @private 批量执行 tool_calls 列表
%%
%% 逐个解析 tool_call 结构并调用对应工具，
%% 将结果编码为 tool 角色消息并累积返回。context 在工具间透传
%% （工具可修改 context），但消息不再写回 context。
execute_tool_calls(Kernel, ToolCalls, Context) ->
    lists:foldl(fun(TC, {ResultsAcc, CtxAcc}) ->
        {Id, Name, Args} = beamai_tool:parse_tool_call(TC),
        {ResultContent, NewCtx} = case invoke_tool(Kernel, Name, Args, CtxAcc) of
            {ok, Value, UpdatedCtx} -> {beamai_tool:encode_result(Value), UpdatedCtx};
            {error, Reason} -> {beamai_tool:encode_result(#{error => Reason}), CtxAcc}
        end,
        Msg = beamai_message:tool_result(Id, Name, ResultContent),
        {ResultsAcc ++ [Msg], NewCtx}
    end, {[], Context}, ToolCalls).

%%====================================================================
%% 内部函数 - 辅助
%%====================================================================

%% @private 解析会话模式与标识
%%
%% 返回 {Mode, Context, ConvId, Ephemeral}：
%% - 无记忆：{full, Context0, undefined, false}
%% - 有记忆 + context 已有 conv_id：{delta, Context0, ConvId, false}
%% - 有记忆 + context 无 conv_id：生成临时 id {delta, Context', ConvId, true}
resolve_conversation(Context0, undefined) ->
    {full, Context0, undefined, false};
resolve_conversation(Context0, _Memory) ->
    case beamai_context:conversation_id(Context0) of
        undefined ->
            ConvId = beamai_id:gen_id(<<"conv">>),
            {delta, beamai_context:with_conversation_id(Context0, ConvId), ConvId, true};
        ConvId ->
            {delta, Context0, ConvId, false}
    end.

%% @private 清理临时会话
cleanup_ephemeral(_Memory, _ConvId, false) ->
    ok;
cleanup_ephemeral(Memory, ConvId, true) ->
    beamai_chat_memory:mem_clear(Memory, ConvId).

%% @private 构造 system_prompts 临时注入 filter（仅 around_chat，order -500）
%%
%% 在 Memory 展开完整历史之后（memory order 更小，更外层）、LLM 之前前置
%% 系统消息，且不写入存储。
system_prompt_filter([]) ->
    [];
system_prompt_filter(SystemPrompts) ->
    [beamai_filter:new(<<"system_prompt">>, #{
        around_chat => fun(#{messages := Msgs} = Req, _FCtx, Next) ->
            Next(Req#{messages => SystemPrompts ++ Msgs})
        end
    }, -500)].

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

%% @private 运行 tool filter 洋葱链（用 around_tool hook）
%%
%% Request `#{tool, args, context}` → Response `#{result, context}`，
%% 最内层 terminal 为真正的工具执行。
run_tool(Filters, ToolSpec, Args, Context) ->
    Req = #{tool => ToolSpec, args => Args, context => Context},
    Terminal = tool_terminal(),
    case beamai_filter_chain:run(Filters, around_tool, Terminal, Req) of
        {ok, #{result := Value, context := Ctx}} -> {ok, Value, Ctx};
        {error, _} = Err -> Err
    end.

%% @private tool 链最内层：真正执行工具（出错时 throw，由链统一捕获）
tool_terminal() ->
    fun(#{tool := ToolSpec, args := Args, context := Ctx}) ->
        case beamai_tool:invoke(ToolSpec, Args, Ctx) of
            {ok, Value} -> #{result => Value, context => Ctx};
            {ok, Value, NewCtx} -> #{result => Value, context => NewCtx};
            {error, Reason} -> throw(Reason)
        end
    end.
