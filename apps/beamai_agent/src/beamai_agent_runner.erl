%%%-------------------------------------------------------------------
%%% @doc 图执行模块
%%%
%%% 负责 Agent 图的执行逻辑：
%%% - 图构建
%%% - 图执行
%%% - 路由决策
%%% - 中断处理
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_runner).

-include("beamai_agent.hrl").

%% API 导出
-export([
    build_graph/1,
    execute/3,
    rebuild_graph/1
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 构建 Agent 执行图
%%
%% 返回编译后的图，可用于 graph:run/2。
%% 支持 Middleware 配置，当提供 middlewares 选项时使用 Middleware 节点。
-spec build_graph(map()) -> {ok, map()} | {error, term()}.
build_graph(Opts) ->
    LLMConfig = maps:get(llm, Opts, #{}),
    Tools = maps:get(tools, Opts, []),
    MaxIterations = maps:get(max_iterations, Opts, 10),
    ResponseFormat = maps:get(response_format, Opts, undefined),
    Middlewares = maps:get(middlewares, Opts, []),

    %% 构建工具处理器映射
    ToolHandlers = beamai_nodes:build_tool_handlers(Tools),

    %% 根据是否有 Middleware 选择创建节点的方式
    Nodes = case Middlewares of
        [] ->
            %% 无 Middleware，使用原有节点
            create_nodes(LLMConfig, ToolHandlers, ResponseFormat);
        _ ->
            %% 有 Middleware，使用 Middleware 节点
            create_middleware_nodes(LLMConfig, ToolHandlers, ResponseFormat, Middlewares)
    end,

    %% 构建图
    build_graph_from_nodes(Nodes, MaxIterations).

%% @doc 执行图
%%
%% 执行已编译的图，返回结果。
%% 会将新消息追加到历史 messages 中，执行后保存完整对话历史。
%%
%% 消息压缩功能由 middleware_summarization 中间件提供，
%% 在 before_model 钩子中自动处理。
-spec execute(binary(), map(), #state{}) ->
    {ok, map(), #state{}} | {error, term(), #state{}}.
execute(Msg, _Opts, #state{graph = Graph, system_prompt = Prompt,
                            tools = Tools, max_iterations = MaxIter,
                            messages = HistoryMsgs,
                            full_messages = HistoryFullMsgs,
                            context = Context,
                            callbacks = Callbacks, run_id = RunId,
                            id = AgentId, name = AgentName} = State) ->
    %% 构建回调元数据
    CallbackMeta = #{
        agent_id => AgentId,
        agent_name => AgentName,
        run_id => RunId,
        timestamp => erlang:system_time(millisecond)
    },

    %% 将新的 user message 追加到历史消息
    %% messages: 可能已压缩，用于 LLM 调用
    %% full_messages: 完整历史，用于审计和持久化
    NewUserMsg = #{role => user, content => Msg},
    AllMessages = HistoryMsgs ++ [NewUserMsg],
    AllFullMessages = HistoryFullMsgs ++ [NewUserMsg],

    %% 构建初始状态（包含历史消息和回调）
    %% 消息压缩由 middleware_summarization 在 before_model 钩子中处理
    InitState = graph:state(#{
        messages => AllMessages,
        full_messages => AllFullMessages,
        system_prompt => Prompt,
        tools => [to_tool_spec(T) || T <- Tools],
        max_iterations => MaxIter,
        iteration => 0,
        scratchpad => [],
        context => Context,  %% 用户自定义上下文，Tool 和 Middleware 可访问
        callbacks => beamai_agent_callbacks:to_map(Callbacks),
        callback_meta => CallbackMeta
    }),

    %% 执行图 (使用 Pregel 引擎)
    handle_graph_result(graph:run(Graph, InitState), State).

%% @doc 重新构建图
%%
%% 从当前状态重新构建图（用于工具/配置变更后）。
-spec rebuild_graph(#state{}) -> {ok, #state{}} | {error, term()}.
rebuild_graph(#state{tools = Tools, system_prompt = Prompt,
                     llm_config = LLMConfig, max_iterations = MaxIter,
                     middlewares = Middlewares, response_format = RF} = State) ->
    Opts = #{
        tools => Tools,
        system_prompt => Prompt,
        llm => LLMConfig,
        max_iterations => MaxIter,
        middlewares => Middlewares,
        response_format => RF
    },
    case build_graph(Opts) of
        {ok, NewGraph} ->
            {ok, State#state{graph = NewGraph}};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 内部函数 - 图构建
%%====================================================================

%% @private 创建所有节点（无 Middleware）
-spec create_nodes(map(), map(), map() | undefined) -> map().
create_nodes(LLMConfig, ToolHandlers, ResponseFormat) ->
    #{
        llm_call => beamai_nodes:llm_node(LLMConfig),
        record_llm => beamai_nodes:scratchpad_node(llm_response),
        validate_response => beamai_nodes:validate_node(ResponseFormat),
        check_before_tools => fun(State) -> {ok, State} end,
        execute_tools => beamai_nodes:tool_node(ToolHandlers),
        record_tools => beamai_nodes:scratchpad_node(tool_result),
        check_after_tools => fun(State) -> {ok, State} end,
        increment_iter => beamai_nodes:iteration_node()
    }.

%% @private 创建带 Middleware 的节点
-spec create_middleware_nodes(map(), map(), map() | undefined, list()) -> map().
create_middleware_nodes(LLMConfig, ToolHandlers, ResponseFormat, Middlewares) ->
    #{
        llm_call => beamai_middleware_nodes:llm_node(LLMConfig, Middlewares),
        record_llm => beamai_nodes:scratchpad_node(llm_response),
        validate_response => beamai_nodes:validate_node(ResponseFormat),
        %% Middleware 模式下，中断检查由 HumanApprovalMiddleware 处理
        check_before_tools => fun(State) -> {ok, State} end,
        execute_tools => beamai_middleware_nodes:tool_node(ToolHandlers, Middlewares),
        record_tools => beamai_nodes:scratchpad_node(tool_result),
        check_after_tools => fun(State) -> {ok, State} end,
        increment_iter => beamai_nodes:iteration_node()
    }.

%% @private 从节点构建图
-spec build_graph_from_nodes(map(), pos_integer()) -> {ok, map()} | {error, term()}.
build_graph_from_nodes(Nodes, MaxIterations) ->
    %% 创建构建器
    B0 = graph:builder(#{
        max_iterations => MaxIterations * 2
    }),

    %% 添加所有节点
    B1 = add_all_nodes(B0, Nodes),

    %% 添加边
    B2 = add_all_edges(B1),

    %% 设置入口并编译
    B3 = graph:set_entry(B2, llm_call),
    graph:compile(B3).

%% @private 添加所有节点
-spec add_all_nodes(map(), map()) -> map().
add_all_nodes(Builder, Nodes) ->
    NodeList = [
        {llm_call, maps:get(llm_call, Nodes)},
        {record_llm, maps:get(record_llm, Nodes)},
        {validate_response, maps:get(validate_response, Nodes)},
        {check_before_tools, maps:get(check_before_tools, Nodes)},
        {execute_tools, maps:get(execute_tools, Nodes)},
        {record_tools, maps:get(record_tools, Nodes)},
        {check_after_tools, maps:get(check_after_tools, Nodes)},
        {increment_iter, maps:get(increment_iter, Nodes)}
    ],
    lists:foldl(
        fun({Name, Node}, Acc) -> graph:add_node(Acc, Name, Node) end,
        Builder,
        NodeList
    ).

%% @private 添加所有边
-spec add_all_edges(map()) -> map().
add_all_edges(Builder) ->
    %% 顺序边
    B1 = graph:add_edge(Builder, llm_call, record_llm),
    B2 = graph:add_edge(B1, record_llm, validate_response),
    B3 = graph:add_edge(B2, execute_tools, record_tools),
    B4 = graph:add_edge(B3, record_tools, check_after_tools),
    B5 = graph:add_edge(B4, increment_iter, llm_call),

    %% 条件边
    B6 = graph:add_conditional_edge(B5, validate_response, fun route_after_llm/1),
    B7 = graph:add_conditional_edge(B6, check_before_tools, fun route_with_interrupt/1),
    graph:add_conditional_edge(B7, check_after_tools, fun route_with_interrupt/1).

%%====================================================================
%% 内部函数 - 路由
%%====================================================================

%% @private LLM 调用后的路由
-spec route_after_llm(map()) -> atom().
route_after_llm(State) ->
    case beamai_nodes:route_after_llm(State) of
        '__end__' -> '__end__';
        execute_tools -> check_before_tools
    end.

%% @private 中断检查后的路由
-spec route_with_interrupt(map()) -> atom().
route_with_interrupt(State) ->
    Interrupted = graph:get(State, interrupted, false),
    Rejected = graph:get(State, rejected, false),
    route_with_interrupt_impl(Interrupted, Rejected, State).

%% @private 路由实现（使用模式匹配减少嵌套）
-spec route_with_interrupt_impl(boolean(), boolean(), map()) -> atom().
route_with_interrupt_impl(true, _, _State) ->
    '__end__';
route_with_interrupt_impl(_, true, _State) ->
    '__end__';
route_with_interrupt_impl(false, false, State) ->
    case graph:get(State, last_interrupt_point, undefined) of
        before_tools -> execute_tools;
        after_tools -> increment_iter;
        _ -> increment_iter
    end.

%%====================================================================
%% 内部函数 - 结果处理
%%====================================================================

%% @private 处理图执行结果
%%
%% 保存 messages、full_messages 和 scratchpad 到 state。
%% - messages: 可能已被 middleware_summarization 压缩，用于 LLM 调用
%% - full_messages: 完整对话历史，用于审计、调试、回溯
-spec handle_graph_result(map(), #state{}) ->
    {ok, map(), #state{}} | {error, term(), #state{}}.
handle_graph_result(#{status := completed, final_state := FinalState}, State) ->
    Result = build_result(FinalState),
    NewState = extract_and_update_state(FinalState, State),
    {ok, Result, NewState};
handle_graph_result(#{status := max_iterations, final_state := FinalState}, State) ->
    Result = build_result(FinalState),
    NewState = extract_and_update_state(FinalState, State),
    {ok, Result#{warning => max_iterations_reached}, NewState};
handle_graph_result(#{status := error, error := Reason, final_state := FinalState}, State) ->
    NewState = extract_and_update_state(FinalState, State),
    {error, Reason, NewState}.

%% @private 从图状态提取数据并更新 Agent 状态
-spec extract_and_update_state(map(), #state{}) -> #state{}.
extract_and_update_state(FinalState, State) ->
    Messages = graph:get(FinalState, messages, []),
    FullMessages = graph:get(FinalState, full_messages, []),
    Scratchpad = graph:get(FinalState, scratchpad, []),
    Context = graph:get(FinalState, context, #{}),
    State#state{
        messages = Messages,
        full_messages = FullMessages,
        scratchpad = Scratchpad,
        context = Context
    }.

%% @private 从图状态构建结果
-spec build_result(map()) -> map().
build_result(FinalState) ->
    Messages = graph:get(FinalState, messages, []),
    FinalResponse = beamai_nodes:extract_final_response(Messages),
    FinishReason = graph:get(FinalState, finish_reason, <<"stop">>),
    ValidatedContent = graph:get(FinalState, validated_content, undefined),

    Result = #{
        status => completed,
        messages => Messages,
        final_response => FinalResponse,
        finish_reason => FinishReason,
        iterations => graph:get(FinalState, iteration, 0)
    },

    maybe_add_validated_content(Result, ValidatedContent).

%% @private 添加验证内容（如果有）
-spec maybe_add_validated_content(map(), term()) -> map().
maybe_add_validated_content(Result, undefined) -> Result;
maybe_add_validated_content(Result, V) -> Result#{validated_content => V}.

%%====================================================================
%% 内部函数 - 辅助
%%====================================================================

%% @private 转换工具定义为规格
-spec to_tool_spec(map()) -> map().
to_tool_spec(Tool) ->
    case maps:is_key(handler, Tool) of
        true -> maps:remove(handler, Tool);
        false -> Tool
    end.
