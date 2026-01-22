%%%-------------------------------------------------------------------
%%% @doc Graph Agent 通用节点模块
%%%
%%% 提供构建 Agent 的通用图节点：
%%%   - llm_node: LLM 调用节点（委托给 beamai_llm_node）
%%%   - tool_node: 工具执行节点（委托给 beamai_tool_node）
%%%   - iteration_node: 迭代计数节点
%%%   - scratchpad_node: Scratchpad 记录节点
%%%   - validate_node: 响应格式验证节点
%%%
%%% 使用 OpenAI 的 finish_reason 作为停止条件：
%%%   - "stop": 正常完成
%%%   - "tool_calls": 需要执行工具
%%%   - "length": 达到 token 限制
%%%   - "content_filter": 内容被过滤
%%%   - "end_turn": Anthropic 正常结束
%%%
%%% 模块拆分说明：
%%%   - beamai_llm_node: LLM 调用相关逻辑
%%%   - beamai_tool_node: 工具执行相关逻辑
%%%   - beamai_nodes: 简单节点、路由、工具函数（本模块）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_nodes).

-include_lib("beamai_core/include/beamai_common.hrl").

%% 节点创建 API（LLM 和工具节点委托给子模块）
-export([llm_node/1, llm_node/2]).
-export([tool_node/1]).
-export([iteration_node/0]).
-export([scratchpad_node/1]).
-export([validate_node/1]).

%% 路由函数
-export([route_after_llm/1]).

%% 工具函数
-export([build_tool_handlers/1]).
-export([extract_final_response/1]).

%% 回调辅助函数（委托给 beamai_callback_utils）
-export([invoke_callback/3, invoke_callback/4]).

%%====================================================================
%% LLM 节点（委托给 beamai_llm_node）
%%====================================================================

%% @doc 创建 LLM 调用节点（使用默认配置键）
-spec llm_node(map()) -> fun((map()) -> {ok, map()} | {error, term()}).
llm_node(LLMConfig) ->
    beamai_llm_node:create(LLMConfig).

%% @doc 创建 LLM 调用节点（带选项）
-spec llm_node(map(), map()) -> fun((map()) -> {ok, map()} | {error, term()}).
llm_node(LLMConfig, Opts) ->
    beamai_llm_node:create(LLMConfig, Opts).

%%====================================================================
%% 工具执行节点（委托给 beamai_tool_node）
%%====================================================================

%% @doc 创建工具执行节点
-spec tool_node(#{binary() => function()}) -> fun((map()) -> {ok, map()}).
tool_node(ToolHandlers) ->
    beamai_tool_node:create(ToolHandlers).

%%====================================================================
%% 迭代计数节点
%%====================================================================

%% @doc 创建迭代计数节点
-spec iteration_node() -> fun((map()) -> {ok, map()}).
iteration_node() ->
    fun(State) ->
        Iter = graph:get(State, iteration, 0),
        {ok, graph:set(State, iteration, Iter + 1)}
    end.

%%====================================================================
%% Scratchpad 节点
%%====================================================================

%% @doc 创建 Scratchpad 记录节点
%% StepType: llm_response | tool_call | tool_result
-spec scratchpad_node(atom()) -> fun((map()) -> {ok, map()}).
scratchpad_node(StepType) ->
    fun(State) ->
        Pad = graph:get(State, scratchpad, []),
        Step = #{
            iteration => length(Pad),
            type => StepType,
            timestamp => erlang:system_time(millisecond),
            data => extract_step_data(State, StepType)
        },
        {ok, graph:set(State, scratchpad, Pad ++ [Step])}
    end.

%% @private 提取步骤数据
-spec extract_step_data(map(), atom()) -> map().
extract_step_data(State, llm_response) ->
    #{
        content => graph:get(State, last_content, null),
        tool_calls => graph:get(State, tool_calls, []),
        finish_reason => graph:get(State, finish_reason, <<"stop">>)
    };
extract_step_data(State, tool_call) ->
    #{
        tool_calls => graph:get(State, tool_calls, [])
    };
extract_step_data(State, tool_result) ->
    #{
        results => graph:get(State, tool_results, [])
    };
extract_step_data(_State, _) ->
    #{}.

%%====================================================================
%% 响应验证节点
%%====================================================================

%% @doc 创建响应格式验证节点
-spec validate_node(map() | undefined) -> fun((map()) -> {ok, map()}).
validate_node(undefined) ->
    fun(State) -> {ok, State} end;
validate_node(Format) ->
    fun(State) ->
        Content = graph:get(State, last_content, <<>>),
        case validate_content(Content, Format) of
            {ok, Validated} ->
                State1 = graph:set(State, validated_content, Validated),
                {ok, State1};
            {error, _Reason} ->
                State1 = graph:set(State, format_error, true),
                {ok, State1}
        end
    end.

%% @private 验证内容格式
-spec validate_content(binary() | null, map()) -> {ok, term()} | {error, term()}.
validate_content(null, _Format) ->
    {error, empty_content};
validate_content(<<>>, _Format) ->
    {error, empty_content};
validate_content(Content, #{type := json_object}) ->
    try
        {ok, jsx:decode(Content, [return_maps])}
    catch
        _:_ -> {error, invalid_json}
    end;
validate_content(Content, #{type := json_array}) ->
    try
        Decoded = jsx:decode(Content, [return_maps]),
        case is_list(Decoded) of
            true -> {ok, Decoded};
            false -> {error, not_array}
        end
    catch
        _:_ -> {error, invalid_json}
    end;
validate_content(Content, _) ->
    {ok, Content}.

%%====================================================================
%% 路由函数
%%====================================================================

%% @doc LLM 调用后的路由函数
%% 基于 OpenAI finish_reason 判断下一步
-spec route_after_llm(map()) -> atom().
route_after_llm(State) ->
    FinishReason = graph:get(State, finish_reason, <<"stop">>),
    ToolCalls = graph:get(State, tool_calls, []),
    Iteration = graph:get(State, iteration, 0),
    MaxIters = graph:get(State, max_iterations, 10),
    HasError = graph:get(State, error, undefined) =/= undefined,
    Rejected = graph:get(State, rejected, false),

    %% 使用辅助函数进行路由决策
    route_after_llm_impl(HasError, Rejected, Iteration >= MaxIters, FinishReason, ToolCalls).

%% @private 路由实现（使用模式匹配减少嵌套）
-spec route_after_llm_impl(boolean(), boolean(), boolean(), binary(), [map()]) -> atom().
route_after_llm_impl(true, _, _, _, _) -> '__end__';
route_after_llm_impl(_, true, _, _, _) -> '__end__';
route_after_llm_impl(_, _, true, _, _) -> '__end__';
route_after_llm_impl(false, false, false, FinishReason, ToolCalls) ->
    route_by_finish_reason(FinishReason, ToolCalls).

%% @private 基于 finish_reason 路由
-spec route_by_finish_reason(binary(), [map()]) -> atom().
route_by_finish_reason(<<"tool_calls">>, _) ->
    %% OpenAI 工具调用
    execute_tools;
route_by_finish_reason(<<"tool_use">>, _) ->
    %% Anthropic 工具调用
    execute_tools;
route_by_finish_reason(<<"stop">>, _) ->
    '__end__';
route_by_finish_reason(<<"end_turn">>, _) ->
    %% Anthropic 正常结束
    '__end__';
route_by_finish_reason(<<"length">>, _) ->
    %% 达到 token 限制
    '__end__';
route_by_finish_reason(<<"content_filter">>, _) ->
    %% 内容被过滤
    '__end__';
route_by_finish_reason(_Other, ToolCalls) ->
    %% 未知 finish_reason，使用 tool_calls 存在性判断
    case ToolCalls of
        [] -> '__end__';
        [_|_] -> execute_tools
    end.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 从工具定义列表构建处理器映射（委托给 beamai_tool_node）
-spec build_tool_handlers([map()]) -> #{binary() => function()}.
build_tool_handlers(Tools) ->
    beamai_tool_node:build_handlers(Tools).

%% @doc 从消息列表提取最终响应
%% 委托给 beamai_message:extract_last_content/1
-spec extract_final_response([map()]) -> binary().
extract_final_response(Messages) ->
    beamai_message:extract_last_content(Messages).

%%====================================================================
%% 回调辅助函数
%%====================================================================

%% @doc 调用回调函数（从图状态获取回调和元数据）
%% 委托给 beamai_callback_utils
-spec invoke_callback(atom(), list(), map()) -> ok.
invoke_callback(CallbackName, Args, State) ->
    ?INVOKE_CALLBACK_FROM_STATE(CallbackName, Args, State).

%% @doc 调用回调函数（直接传入回调映射和元数据）
%% 委托给 beamai_callback_utils
-spec invoke_callback(atom(), list(), map(), map()) -> ok.
invoke_callback(CallbackName, Args, Callbacks, Meta) ->
    ?INVOKE_CALLBACK(CallbackName, Args, Callbacks, Meta).
