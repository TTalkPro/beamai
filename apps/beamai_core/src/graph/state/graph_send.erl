%%%-------------------------------------------------------------------
%%% @doc Send API - 动态并行分发模块
%%%
%%% 实现 LangGraph 风格的动态并行分发，允许条件边在运行时
%%% 创建任意数量的并行执行分支，每个分支有独立的输入状态。
%%%
%%% 核心概念:
%%% - Send: 待执行的节点调用指令
%%% - Fan-out: 条件边返回多个 Send 创建并行分支
%%% - Fan-in: 并行分支完成后结果自动聚合
%%%
%%% 设计原则:
%%% - 简洁性: 最小化 API 表面
%%% - 一致性: 与现有 graph 模块风格统一
%%% - 可追踪: Send 带唯一 ID 便于调试
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_send).

%% Send 创建
-export([send_to/2, send_to/3]).
-export([is_send/1, is_sends/1]).

%% Fan-out 创建
-export([fan_out/2, fan_out/3]).
-export([fan_out_indexed/3]).

%% 结果聚合
-export([aggregate/2, collect_to_key/4]).

%% 路由辅助
-export([continue_to/1, end_node/0]).

%% Send 访问器
-export([get_node/1, get_state/1, get_id/1, get_metadata/1]).

%% 验证
-export([validate/2]).

%% 类型定义
-type node_id() :: atom().
-type send_id() :: binary().
-type send() :: #{
    '__send__' := true,
    node := node_id(),
    state := graph_state:state(),
    id := send_id(),
    metadata := map()
}.
-type send_result() :: #{
    send_id := send_id(),
    node := node_id(),
    state := graph_state:state(),
    success := boolean(),
    error => term()
}.

-export_type([send/0, send_result/0, send_id/0]).

%%====================================================================
%% Send 创建
%%====================================================================

%% @doc 创建 Send 指令
-spec send_to(node_id(), map()) -> send().
send_to(Node, State) ->
    send_to(Node, State, #{}).

%% @doc 创建 Send 指令 (带选项)
%% 选项: id - 自定义标识符, metadata - 额外元数据
-spec send_to(node_id(), map(), map()) -> send().
send_to(Node, State, Opts) when is_atom(Node), is_map(State) ->
    #{
        '__send__' => true,
        node => Node,
        state => normalize_state(State),
        id => maps:get(id, Opts, make_id(<<"send">>)),
        metadata => maps:get(metadata, Opts, #{})
    }.

%% @doc 判断是否为 Send 指令
-spec is_send(term()) -> boolean().
is_send(#{'__send__' := true}) -> true;
is_send(_) -> false.

%% @doc 判断是否为 Send 列表
-spec is_sends(term()) -> boolean().
is_sends(L) when is_list(L) -> lists:all(fun is_send/1, L);
is_sends(_) -> false.

%%====================================================================
%% Fan-out 创建
%%====================================================================

%% @doc 创建 fan-out Send 列表 (直接使用 map 列表)
-spec fan_out(node_id(), [map()]) -> [send()].
fan_out(Node, Items) ->
    fan_out(Node, Items, fun(X) -> X end).

%% @doc 创建 fan-out Send 列表 (带转换函数)
-spec fan_out(node_id(), list(), fun((term()) -> map())) -> [send()].
fan_out(Node, Items, ToState) when is_atom(Node), is_list(Items) ->
    [make_fanout_send(Node, ToState(Item)) || Item <- Items].

%% @doc 创建带索引的 fan-out Send 列表
-spec fan_out_indexed(node_id(), list(), fun((non_neg_integer(), term()) -> map())) -> [send()].
fan_out_indexed(Node, Items, ToState) when is_atom(Node), is_list(Items) ->
    {Sends, _} = lists:foldl(
        fun(Item, {Acc, Idx}) ->
            Send = make_fanout_send(Node, ToState(Idx, Item), Idx),
            {[Send | Acc], Idx + 1}
        end,
        {[], 0},
        Items
    ),
    lists:reverse(Sends).

%%====================================================================
%% 结果聚合
%%====================================================================

%% @doc 聚合并行执行结果到基础状态
-spec aggregate([send_result()], graph_state:state()) -> graph_state:state().
aggregate(Results, BaseState) ->
    lists:foldl(
        fun(#{success := true, state := S}, Acc) -> graph_state:merge(Acc, S);
           (_, Acc) -> Acc
        end,
        BaseState,
        Results
    ).

%% @doc 将并行结果收集到指定键
-spec collect_to_key([send_result()], graph_state:state(), atom(), fun((map()) -> term())) ->
    graph_state:state().
collect_to_key(Results, BaseState, Key, Extract) ->
    Values = [Extract(S) || #{success := true, state := S} <- Results],
    graph_state:set(BaseState, Key, Values).

%%====================================================================
%% 路由辅助
%%====================================================================

%% @doc 返回单一节点跳转 (非并行)
-spec continue_to(node_id()) -> node_id().
continue_to(Node) when is_atom(Node) -> Node.

%% @doc 返回终止节点标记
-spec end_node() -> '__end__'.
end_node() -> '__end__'.

%%====================================================================
%% Send 访问器
%%====================================================================

%% @doc 获取目标节点
-spec get_node(send()) -> node_id().
get_node(#{node := N}) -> N.

%% @doc 获取状态
-spec get_state(send()) -> graph_state:state().
get_state(#{state := S}) -> S.

%% @doc 获取 ID
-spec get_id(send()) -> send_id().
get_id(#{id := Id}) -> Id.

%% @doc 获取元数据
-spec get_metadata(send()) -> map().
get_metadata(#{metadata := M}) -> M.

%%====================================================================
%% 验证
%%====================================================================

%% @doc 验证 Send 列表中的节点是否都存在
-spec validate([send()], [node_id()]) -> ok | {error, [term()]}.
validate(Sends, AvailableNodes) ->
    NodeSet = sets:from_list(AvailableNodes),
    Errors = lists:filtermap(
        fun(#{node := N, id := Id}) ->
            case sets:is_element(N, NodeSet) of
                true -> false;
                false -> {true, {unknown_node, Id, N}}
            end
        end,
        Sends
    ),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 生成唯一 ID
-spec make_id(binary()) -> binary().
make_id(Prefix) ->
    Ts = integer_to_binary(erlang:system_time(microsecond)),
    Uniq = integer_to_binary(erlang:unique_integer([positive])),
    <<Prefix/binary, "-", Ts/binary, "-", Uniq/binary>>.

%% @doc 创建 fan-out Send
-spec make_fanout_send(node_id(), map()) -> send().
make_fanout_send(Node, State) ->
    Id = maps:get(id, State, make_id(<<"fanout">>)),
    send_to(Node, State, #{id => Id}).

-spec make_fanout_send(node_id(), map(), non_neg_integer()) -> send().
make_fanout_send(Node, State, Idx) ->
    Id = maps:get(id, State, make_id(<<"fanout-", (integer_to_binary(Idx))/binary>>)),
    send_to(Node, State, #{id => Id}).

%% @doc 标准化状态
-spec normalize_state(map()) -> graph_state:state().
normalize_state(State) when is_map(State) ->
    graph_state:new(State).
