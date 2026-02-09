%%%-------------------------------------------------------------------
%%% @doc Dispatch API - 动态并行分发模块
%%%
%%% 实现 LangGraph 风格的动态并行分发，允许条件边在运行时
%%% 创建任意数量的并行执行分支，每个分支有独立的输入参数。
%%%
%%% 全局状态模式说明：
%%% - 全局状态由 Master 管理，所有分支共享同一个 global_state
%%% - Dispatch 中的 "input" 字段是分支的输入参数（不是独立状态）
%%% - 分支执行产生的 delta 会合并到 global_state
%%% - Fan-in 时所有分支的 delta 按 field_reducers 合并
%%%
%%% 核心概念:
%%% - Dispatch: 待执行的节点调用指令（包含目标节点和输入参数）
%%% - Fan-out: 条件边返回多个 Dispatch 创建并行分支
%%% - Fan-in: 并行分支完成后 delta 自动聚合到 global_state
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_dispatch).

%% Dispatch 创建
-export([dispatch/1, dispatch/2, dispatch/3]).
-export([is_dispatch/1, is_dispatches/1]).

%% Fan-out 创建
-export([fan_out/2, fan_out/3]).
-export([fan_out_indexed/3]).

%% 路由辅助
-export([continue_to/1, end_node/0]).

%% Dispatch 访问器
-export([get_node/1, get_input/1, get_id/1, get_metadata/1]).

%% 验证
-export([validate/2]).

%% 类型定义
-type node_id() :: atom().
-type dispatch_id() :: binary().

%% Dispatch 指令
%% - node: 目标节点
%% - input: 分支输入参数（会作为 VertexInput 传递给节点）
%% - id: 唯一标识符
%% - metadata: 额外元数据
-type dispatch() :: #{
    '__dispatch__' := true,
    node := node_id(),
    input := map(),
    id := dispatch_id(),
    metadata := map()
}.

-export_type([dispatch/0, dispatch_id/0]).

%%====================================================================
%% Dispatch 创建
%%====================================================================

%% @doc 创建 Dispatch 指令（仅目标节点，无额外输入）
-spec dispatch(node_id()) -> dispatch().
dispatch(Node) ->
    dispatch(Node, #{}, #{}).

%% @doc 创建 Dispatch 指令
%% Input: 分支的输入参数，会作为 VertexInput 传递给节点
-spec dispatch(node_id(), map()) -> dispatch().
dispatch(Node, Input) ->
    dispatch(Node, Input, #{}).

%% @doc 创建 Dispatch 指令 (带选项)
%% 选项: id - 自定义标识符, metadata - 额外元数据
-spec dispatch(node_id(), map(), map()) -> dispatch().
dispatch(Node, Input, Opts) when is_atom(Node), is_map(Input) ->
    #{
        '__dispatch__' => true,
        node => Node,
        input => Input,
        id => maps:get(id, Opts, make_id(<<"dispatch">>)),
        metadata => maps:get(metadata, Opts, #{})
    }.

%% @doc 判断是否为 Dispatch 指令
-spec is_dispatch(term()) -> boolean().
is_dispatch(#{'__dispatch__' := true}) -> true;
is_dispatch(_) -> false.

%% @doc 判断是否为 Dispatch 列表
-spec is_dispatches(term()) -> boolean().
is_dispatches(L) when is_list(L) -> lists:all(fun is_dispatch/1, L);
is_dispatches(_) -> false.

%%====================================================================
%% Fan-out 创建
%%====================================================================

%% @doc 创建 fan-out Dispatch 列表 (直接使用 map 列表作为输入参数)
-spec fan_out(node_id(), [map()]) -> [dispatch()].
fan_out(Node, Items) ->
    fan_out(Node, Items, fun(X) -> X end).

%% @doc 创建 fan-out Dispatch 列表 (带转换函数)
%% ToInput: 将列表项转换为输入参数的函数
-spec fan_out(node_id(), list(), fun((term()) -> map())) -> [dispatch()].
fan_out(Node, Items, ToInput) when is_atom(Node), is_list(Items) ->
    [make_fanout_dispatch(Node, ToInput(Item)) || Item <- Items].

%% @doc 创建带索引的 fan-out Dispatch 列表
-spec fan_out_indexed(node_id(), list(), fun((non_neg_integer(), term()) -> map())) -> [dispatch()].
fan_out_indexed(Node, Items, ToInput) when is_atom(Node), is_list(Items) ->
    {Dispatches, _} = lists:foldl(
        fun(Item, {Acc, Idx}) ->
            D = make_fanout_dispatch(Node, ToInput(Idx, Item), Idx),
            {[D | Acc], Idx + 1}
        end,
        {[], 0},
        Items
    ),
    lists:reverse(Dispatches).

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
%% Dispatch 访问器
%%====================================================================

%% @doc 获取目标节点
-spec get_node(dispatch()) -> node_id().
get_node(#{node := N}) -> N.

%% @doc 获取输入参数
-spec get_input(dispatch()) -> map().
get_input(#{input := I}) -> I.

%% @doc 获取 ID
-spec get_id(dispatch()) -> dispatch_id().
get_id(#{id := Id}) -> Id.

%% @doc 获取元数据
-spec get_metadata(dispatch()) -> map().
get_metadata(#{metadata := M}) -> M.

%%====================================================================
%% 验证
%%====================================================================

%% @doc 验证 Dispatch 列表中的节点是否都存在
-spec validate([dispatch()], [node_id()]) -> ok | {error, [term()]}.
validate(Dispatches, AvailableNodes) ->
    NodeSet = sets:from_list(AvailableNodes),
    Errors = lists:filtermap(
        fun(#{node := N, id := Id}) ->
            case sets:is_element(N, NodeSet) of
                true -> false;
                false -> {true, {unknown_node, Id, N}}
            end
        end,
        Dispatches
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

%% @doc 创建 fan-out Dispatch
-spec make_fanout_dispatch(node_id(), map()) -> dispatch().
make_fanout_dispatch(Node, Input) ->
    Id = maps:get(id, Input, make_id(<<"fanout">>)),
    InputWithoutId = maps:remove(id, Input),
    dispatch(Node, InputWithoutId, #{id => Id}).

-spec make_fanout_dispatch(node_id(), map(), non_neg_integer()) -> dispatch().
make_fanout_dispatch(Node, Input, Idx) ->
    Id = maps:get(id, Input, make_id(<<"fanout-", (integer_to_binary(Idx))/binary>>)),
    InputWithoutId = maps:remove(id, Input),
    dispatch(Node, InputWithoutId, #{id => Id}).
