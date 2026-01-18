%%%-------------------------------------------------------------------
%%% @doc 初始化模块
%%%
%%% 负责 Agent 的初始化逻辑：
%%% - 状态初始化
%%% - 工具初始化
%%% - 系统提示词构建
%%% - 格式化输出指令追加
%%%
%%% == LLM 配置 ==
%%%
%%% LLM 配置必须通过 llm_client:create/2 创建：
%%% ```
%%% LLM = llm_client:create(anthropic, #{
%%%     model => <<"glm-4.7">>,
%%%     api_key => ApiKey,
%%%     base_url => <<"https://open.bigmodel.cn/api/anthropic">>
%%% }),
%%%
%%% {ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
%%%     llm => LLM,
%%%     tools => Tools
%%% }).
%%% ```
%%%
%%% == 工具配置 ==
%%%
%%% 直接传入工具列表：
%%% ```
%%% #{tools => [Tool1, Tool2]}
%%% ```
%%%
%%% 如需从 Provider 获取工具，使用 beamai_tool_registry 在外部构建：
%%% ```
%%% %% 方式 1: 使用 registry 构建
%%% Registry = beamai_tool_registry:new(),
%%% R1 = beamai_tool_registry:add_tools(Registry, [MyTool]),
%%% R2 = beamai_tool_registry:add_provider(R1, beamai_tool_provider_mcp, #{mcp_tag => file}),
%%% Tools = beamai_tool_registry:build(R2),
%%% #{tools => Tools}
%%%
%%% %% 方式 2: 使用便捷函数
%%% Tools = beamai_tool_registry:from_config(#{
%%%     tools => [MyTool],
%%%     providers => [{beamai_tool_provider_mcp, #{mcp_tag => file}}]
%%% }),
%%% #{tools => Tools}
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_init).

-include("beamai_agent.hrl").
-include_lib("beamai_tools/include/beamai_tools.hrl").

%% API 导出
-export([
    create_state/2,
    build_prompt/1
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 创建初始状态
%%
%% 根据配置选项创建完整的 Agent 状态。
%% LLM 配置必须通过 llm_client:create/2 创建。
-spec create_state(binary(), map()) -> {ok, #state{}} | {error, term()}.
create_state(Id, Opts) ->
    %% 验证 LLM 配置
    case validate_llm_config(Opts) of
        ok ->
            case beamai_agent_runner:build_graph(Opts) of
                {ok, Graph} ->
                    State = build_state(Id, Opts, Graph),
                    FinalState = beamai_agent_checkpoint:maybe_restore(Opts, State),
                    {ok, FinalState};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 验证 LLM 配置
%%
%% LLM 配置必须通过 llm_client:create/2 创建，包含 '__llm_client__' => true 标记。
-spec validate_llm_config(map()) -> ok | {error, term()}.
validate_llm_config(Opts) ->
    case maps:get(llm, Opts, undefined) of
        undefined ->
            %% 没有 LLM 配置，允许（某些场景可能不需要 LLM）
            ok;
        LLMConfig when is_map(LLMConfig) ->
            case llm_client:is_valid_config(LLMConfig) of
                true ->
                    ok;
                false ->
                    {error, {invalid_llm_config,
                             <<"LLM config must be created using llm_client:create/2. "
                               "Example: LLM = llm_client:create(anthropic, #{model => <<\"glm-4.7\">>, ...})">>}}
            end;
        _ ->
            {error, {invalid_llm_config, <<"llm config must be a map">>}}
    end.

%% @doc 构建系统提示词
%%
%% 根据配置构建提示词，可追加格式化输出指令。
-spec build_prompt(map()) -> binary().
build_prompt(Opts) ->
    BasePrompt = maps:get(system_prompt, Opts, default_prompt()),
    case maps:get(response_format, Opts, undefined) of
        undefined -> BasePrompt;
        Format -> append_format_instructions(BasePrompt, Format)
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 构建状态记录
-spec build_state(binary(), map(), map()) -> #state{}.
build_state(Id, Opts, Graph) ->
    %% 收集工具：直接传入的 + Provider 提供的
    Tools = collect_tools(Opts),
    %% 提取 LLM 配置（已在 create_state 中验证过）
    LLMConfig = maps:get(llm, Opts, #{}),
    %% 获取 Middleware 配置
    Middlewares = maps:get(middlewares, Opts, []),
    MiddlewareChain = case Middlewares of
        [] -> undefined;
        _ -> beamai_middleware_runner:init(Middlewares)
    end,

    #state{
        id = Id,
        name = maps:get(name, Opts, Id),
        system_prompt = build_prompt(Opts),
        tools = Tools,
        tool_handlers = beamai_nodes:build_tool_handlers(Tools),
        llm_config = LLMConfig,
        graph = Graph,
        max_iterations = maps:get(max_iterations, Opts, 10),
        messages = [],
        full_messages = [],
        scratchpad = [],
        context = maps:get(context, Opts, #{}),  %% 用户自定义上下文
        pending_action = undefined,
        response_format = maps:get(response_format, Opts, undefined),
        callbacks = beamai_agent_callbacks:init(Opts),
        middlewares = Middlewares,
        middleware_chain = MiddlewareChain,
        storage = beamai_agent_checkpoint:init_storage(Id, Opts),
        auto_checkpoint = maps:get(auto_checkpoint, Opts, false),
        run_id = undefined
    }.

%% @private 默认系统提示词
-spec default_prompt() -> binary().
default_prompt() ->
    <<"You are a helpful AI assistant. Use available tools when needed. ",
      "Be accurate and concise.">>.

%% @private 追加格式化输出指令
-spec append_format_instructions(binary(), map()) -> binary().
append_format_instructions(Prompt, #{type := json_object} = Format) ->
    Schema = maps:get(schema, Format, #{}),
    SchemaJson = jsx:encode(Schema),
    <<Prompt/binary, "\n\n",
      "IMPORTANT: You must respond with valid JSON matching this schema:\n",
      SchemaJson/binary,
      "\n\nDo not include any text outside the JSON object.">>;
append_format_instructions(Prompt, #{type := json_array}) ->
    <<Prompt/binary, "\n\n",
      "IMPORTANT: You must respond with a valid JSON array. ",
      "Do not include any text outside the JSON array.">>;
append_format_instructions(Prompt, _) ->
    Prompt.

%%====================================================================
%% 工具收集
%%====================================================================

%% @private 获取工具列表
%%
%% 直接从 tools 参数获取。如需从 Provider 获取工具，
%% 请在外部使用 beamai_tool_registry 构建后传入。
-spec collect_tools(map()) -> [tool_def()].
collect_tools(Opts) ->
    maps:get(tools, Opts, []).
