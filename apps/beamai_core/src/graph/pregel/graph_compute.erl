%%%-------------------------------------------------------------------
%%% @doc Graph Pregel 计算函数模块
%%%
%%% 提供全局的 Pregel 计算函数，用于 Graph 执行。
%%%
%%% 全局状态模式:
%%% - 计算函数从 global_state 读取数据
%%% - 计算函数返回 delta（增量更新）
%%% - 消息用于协调执行流程（activate 消息），不携带状态
%%% - Master 负责合并 delta 并广播新的 global_state
%%%
%%% 核心功能:
%%% - compute_fn/0: 返回全局 Pregel 计算函数
%%% - from_pregel_result/1: 从 Pregel 结果提取最终状态
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_compute).

%% API 导出
-export([compute_fn/0]).
-export([from_pregel_result/1]).

%% 类型定义
-type delta() :: #{atom() | binary() => term()}.

-export_type([delta/0]).

%% 特殊节点常量
-define(START_NODE, '__start__').
-define(END_NODE, '__end__').

%%====================================================================
%% API
%%====================================================================

%% @doc 返回全局 Pregel 计算函数
%%
%% 全局状态模式：
%% - 从 context 中的 global_state 读取当前状态
%% - 返回 delta（增量更新）而不是更新 vertex value
%% - 发送 activate 消息协调下一步执行
%%
%% 返回值包含 status 字段表示执行状态：
%% - status => ok: 计算成功
%% - status => {error, Reason}: 计算失败
%% - status => {interrupt, Reason}: 请求中断（human-in-the-loop）
-spec compute_fn() -> fun((pregel_worker:context()) -> pregel_worker:compute_result()).
compute_fn() ->
    fun(Ctx) ->
        try
            execute_node(Ctx)
        catch
            throw:{interrupt, Reason, Delta, Outbox} ->
                %% 中断：返回 delta 和中断状态
                #{delta => Delta, outbox => Outbox, status => {interrupt, Reason}};
            Class:Reason:_Stacktrace ->
                %% 失败时返回错误状态，空 delta
                #{delta => #{}, outbox => [], status => {error, {Class, Reason}}}
        end
    end.

%% @private 执行节点计算
-spec execute_node(pregel_worker:context()) -> pregel_worker:compute_result().
execute_node(Ctx) ->
    #{vertex_id := VertexId, global_state := GlobalState, inbox := Messages} = Ctx,

    case VertexId of
        ?START_NODE ->
            handle_start_node(Ctx, GlobalState, Messages);
        ?END_NODE ->
            handle_end_node(Ctx, Messages);
        _ ->
            handle_regular_node(Ctx, GlobalState, Messages)
    end.

%% @doc 从 Pregel 结果中提取最终状态
%%
%% 全局状态模式：直接返回 global_state
-spec from_pregel_result(pregel:result()) -> {ok, graph_state:state()} | {error, term()}.
from_pregel_result(Result) ->
    case pregel:get_result_status(Result) of
        completed ->
            GlobalState = pregel:get_result_global_state(Result),
            {ok, GlobalState};
        max_supersteps ->
            GlobalState = pregel:get_result_global_state(Result),
            {error, {partial_result, GlobalState, max_iterations_exceeded}};
        {error, Reason} ->
            GlobalState = pregel:get_result_global_state(Result),
            {error, {partial_result, GlobalState, Reason}}
    end.

%%====================================================================
%% 节点处理
%%====================================================================

%% @private 处理起始节点
%%
%% 起始节点在超步 0 被激活，执行后发送 activate 消息到下游节点
-spec handle_start_node(pregel_worker:context(), graph_state:state(), [term()]) ->
    pregel_worker:compute_result().
handle_start_node(Ctx, GlobalState, Messages) ->
    #{superstep := Superstep, config := Config} = Ctx,

    case Superstep of
        0 ->
            %% 超步 0：执行起始节点，发送 activate 到下游
            NodeConfig = get_node_config(?START_NODE, Config),
            execute_and_route(Ctx, GlobalState, NodeConfig);
        _ ->
            %% 后续超步：如果收到消息则处理，否则 halt
            case has_activation_message(Messages) of
                true ->
                    NodeConfig = get_node_config(?START_NODE, Config),
                    execute_and_route(Ctx, GlobalState, NodeConfig);
                false ->
                    #{delta => #{}, outbox => [], status => ok}
            end
    end.

%% @private 处理终止节点
%%
%% 终止节点收到 activate 消息后，标记执行完成
-spec handle_end_node(pregel_worker:context(), [term()]) ->
    pregel_worker:compute_result().
handle_end_node(_Ctx, []) ->
    #{delta => #{}, outbox => [], status => ok};
handle_end_node(_Ctx, Messages) ->
    case has_activation_message(Messages) of
        true ->
            %% 收到激活消息，标记完成
            #{delta => #{}, outbox => [], status => ok};
        false ->
            #{delta => #{}, outbox => [], status => ok}
    end.

%% @private 处理普通节点
-spec handle_regular_node(pregel_worker:context(), graph_state:state(), [term()]) ->
    pregel_worker:compute_result().
handle_regular_node(_Ctx, _GlobalState, []) ->
    %% 无消息，保持 halt
    #{delta => #{}, outbox => [], status => ok};
handle_regular_node(Ctx, GlobalState, Messages) ->
    case has_activation_message(Messages) of
        true ->
            #{vertex_id := VertexId, config := Config} = Ctx,
            NodeConfig = get_node_config(VertexId, Config),
            %% 处理 resume 消息
            NewGlobalState = apply_resume_messages(GlobalState, Messages),
            execute_and_route(Ctx, NewGlobalState, NodeConfig);
        false ->
            #{delta => #{}, outbox => [], status => ok}
    end.

%%====================================================================
%% 节点执行
%%====================================================================

%% @private 执行节点并路由到下一节点
-spec execute_and_route(pregel_worker:context(), graph_state:state(), map()) ->
    pregel_worker:compute_result().
execute_and_route(Ctx, GlobalState, NodeConfig) ->
    #{vertex_id := VertexId} = Ctx,
    Node = maps:get(node, NodeConfig, undefined),
    Edges = maps:get(edges, NodeConfig, []),

    case Node of
        undefined ->
            %% 无节点定义，直接路由
            route_to_next(GlobalState, Edges);
        _ ->
            %% 执行节点
            case graph_node:execute(Node, GlobalState) of
                {ok, NewState} ->
                    %% 成功：计算 delta，发送 activate 到下游
                    Delta = compute_delta(GlobalState, NewState),
                    Outbox = build_outbox(Edges, NewState),
                    #{delta => Delta, outbox => Outbox, status => ok};
                {interrupt, Reason, NewState} ->
                    %% 中断：保存 delta，但不发送消息
                    Delta = compute_delta(GlobalState, NewState),
                    throw({interrupt, Reason, Delta, []});
                {error, Reason} ->
                    %% 失败：抛出异常
                    throw({node_execution_error, VertexId, Reason})
            end
    end.

%% @private 直接路由（无节点执行）
-spec route_to_next(graph_state:state(), [graph_edge:edge()]) ->
    pregel_worker:compute_result().
route_to_next(State, Edges) ->
    Outbox = build_outbox(Edges, State),
    #{delta => #{}, outbox => Outbox, status => ok}.

%% @private 计算 delta（状态差异）
%%
%% 简单实现：返回 NewState 中与 OldState 不同的字段
%% 注意：这里假设状态变化是由节点显式设置的
-spec compute_delta(graph_state:state(), graph_state:state()) -> delta().
compute_delta(OldState, NewState) ->
    %% 获取新状态的所有键
    NewKeys = graph_state:keys(NewState),

    %% 找出变化的字段
    lists:foldl(
        fun(Key, Acc) ->
            OldValue = graph_state:get(OldState, Key),
            NewValue = graph_state:get(NewState, Key),
            case OldValue =:= NewValue of
                true -> Acc;
                false -> Acc#{Key => NewValue}
            end
        end,
        #{},
        NewKeys
    ).

%%====================================================================
%% 消息处理
%%====================================================================

%% @private 检查是否有激活消息
-spec has_activation_message([term()]) -> boolean().
has_activation_message([]) -> false;
has_activation_message([activate | _]) -> true;
has_activation_message([{activate, _} | _]) -> true;
has_activation_message([{resume, _} | _]) -> true;
has_activation_message([_ | Rest]) -> has_activation_message(Rest).

%% @private 应用 resume 消息到状态
-spec apply_resume_messages(graph_state:state(), [term()]) -> graph_state:state().
apply_resume_messages(State, []) ->
    State;
apply_resume_messages(State, [{resume, ResumeData} | Rest]) when is_map(ResumeData) ->
    %% 将 resume data 合并到状态
    NewState = maps:fold(
        fun(Key, Value, Acc) ->
            graph_state:set(Acc, Key, Value)
        end,
        State,
        ResumeData
    ),
    apply_resume_messages(NewState, Rest);
apply_resume_messages(State, [_ | Rest]) ->
    apply_resume_messages(State, Rest).

%% @private 构建输出消息（activate 消息）
-spec build_outbox([graph_edge:edge()], graph_state:state()) -> [{atom(), term()}].
build_outbox([], _State) ->
    %% 无边，发送到终止节点
    [{?END_NODE, activate}];
build_outbox(Edges, State) ->
    lists:foldl(
        fun(Edge, Acc) ->
            case graph_edge:resolve(Edge, State) of
                {ok, TargetNode} when is_atom(TargetNode) ->
                    [{TargetNode, activate} | Acc];
                {ok, TargetNodes} when is_list(TargetNodes) ->
                    [{T, activate} || T <- TargetNodes] ++ Acc;
                {error, _} ->
                    Acc
            end
        end,
        [],
        Edges
    ).

%%====================================================================
%% 配置访问
%%====================================================================

%% @private 从 config 中获取节点配置
%%
%% Config 结构：
%% #{
%%     nodes => #{
%%         node_id => #{
%%             node => graph_node:graph_node(),
%%             edges => [graph_edge:edge()]
%%         }
%%     }
%% }
-spec get_node_config(atom(), map()) -> map().
get_node_config(NodeId, Config) ->
    Nodes = maps:get(nodes, Config, #{}),
    maps:get(NodeId, Nodes, #{}).
