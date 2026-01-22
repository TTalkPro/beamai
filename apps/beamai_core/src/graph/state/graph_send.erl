%%%-------------------------------------------------------------------
%%% @doc Send API - 动态并行分发模块
%%%
%%% 实现 LangGraph 风格的动态并行分发，允许条件边在运行时
%%% 创建任意数量的并行执行分支，每个分支有独立的输入参数。
%%%
%%% 全局状态模式说明：
%%% - 全局状态由 Master 管理，所有分支共享同一个 global_state
%%% - Send 中的 "input" 字段是分支的输入参数（不是独立状态）
%%% - 分支执行产生的 delta 会合并到 global_state
%%% - Fan-in 时所有分支的 delta 按 field_reducers 合并
%%%
%%% 核心概念:
%%% - Send: 待执行的节点调用指令（包含目标节点和输入参数）
%%% - Fan-out: 条件边返回多个 Send 创建并行分支
%%% - Fan-in: 并行分支完成后 delta 自动聚合到 global_state
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_send).

%% Send 创建
-export([send_to/1, send_to/2, send_to/3]).
-export([is_send/1, is_sends/1]).

%% Fan-out 创建
-export([fan_out/2, fan_out/3]).
-export([fan_out_indexed/3]).

%% 路由辅助
-export([continue_to/1, end_node/0]).

%% Send 访问器
-export([get_node/1, get_input/1, get_id/1, get_metadata/1]).

%% 验证
-export([validate/2]).

%% 类型定义
-type node_id() :: atom().
-type send_id() :: binary().

%% Send 指令
%% - node: 目标节点
%% - input: 分支输入参数（会临时合并到 global_state 供节点使用）
%% - id: 唯一标识符
%% - metadata: 额外元数据
-type send() :: #{
    '__send__' := true,
    node := node_id(),
    input := map(),
    id := send_id(),
    metadata := map()
}.

-export_type([send/0, send_id/0]).

%%====================================================================
%% Send 创建
%%====================================================================

%% @doc 创建 Send 指令（仅目标节点，无额外输入）
-spec send_to(node_id()) -> send().
send_to(Node) ->
    send_to(Node, #{}, #{}).

%% @doc 创建 Send 指令
%% Input: 分支的输入参数，会临时合并到 global_state 供节点使用
-spec send_to(node_id(), map()) -> send().
send_to(Node, Input) ->
    send_to(Node, Input, #{}).

%% @doc 创建 Send 指令 (带选项)
%% 选项: id - 自定义标识符, metadata - 额外元数据
-spec send_to(node_id(), map(), map()) -> send().
send_to(Node, Input, Opts) when is_atom(Node), is_map(Input) ->
    #{
        '__send__' => true,
        node => Node,
        input => Input,
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

%% @doc 创建 fan-out Send 列表 (直接使用 map 列表作为输入参数)
-spec fan_out(node_id(), [map()]) -> [send()].
fan_out(Node, Items) ->
    fan_out(Node, Items, fun(X) -> X end).

%% @doc 创建 fan-out Send 列表 (带转换函数)
%% ToInput: 将列表项转换为输入参数的函数
-spec fan_out(node_id(), list(), fun((term()) -> map())) -> [send()].
fan_out(Node, Items, ToInput) when is_atom(Node), is_list(Items) ->
    [make_fanout_send(Node, ToInput(Item)) || Item <- Items].

%% @doc 创建带索引的 fan-out Send 列表
-spec fan_out_indexed(node_id(), list(), fun((non_neg_integer(), term()) -> map())) -> [send()].
fan_out_indexed(Node, Items, ToInput) when is_atom(Node), is_list(Items) ->
    {Sends, _} = lists:foldl(
        fun(Item, {Acc, Idx}) ->
            Send = make_fanout_send(Node, ToInput(Idx, Item), Idx),
            {[Send | Acc], Idx + 1}
        end,
        {[], 0},
        Items
    ),
    lists:reverse(Sends).

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

%% @doc 获取输入参数
-spec get_input(send()) -> map().
get_input(#{input := I}) -> I.

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
make_fanout_send(Node, Input) ->
    Id = maps:get(id, Input, make_id(<<"fanout">>)),
    InputWithoutId = maps:remove(id, Input),
    send_to(Node, InputWithoutId, #{id => Id}).

-spec make_fanout_send(node_id(), map(), non_neg_integer()) -> send().
make_fanout_send(Node, Input, Idx) ->
    Id = maps:get(id, Input, make_id(<<"fanout-", (integer_to_binary(Idx))/binary>>)),
    InputWithoutId = maps:remove(id, Input),
    send_to(Node, InputWithoutId, #{id => Id}).
