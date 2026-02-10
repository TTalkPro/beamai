%%%-------------------------------------------------------------------
%%% @doc 图节点定义模块
%%%
%%% 节点是图中的计算单元。每个节点是一个函数，负责:
%%% - 接收当前状态
%%% - 执行计算逻辑
%%% - 返回更新后的状态、错误或中断请求
%%%
%%% 特殊节点:
%%% - '__start__': 图的入口点
%%% - '__end__': 图的终止点
%%%
%%% 设计原则:
%%% - 纯函数: 节点函数无副作用
%%% - 错误隔离: 异常被捕获并包装
%%% - 类型安全: 严格的返回值检查
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_graph_node).

%% API 导出
-export([new/2, new/3]).
-export([id/1, fun_/1, metadata/1]).
-export([execute/2]).
-export([start_node/0, end_node/0]).
-export([is_start/1, is_end/1]).
-export([is_valid/1]).

%% 类型定义
-type node_id() :: atom().
-type node_fun() :: fun((beamai_context:t(), map() | undefined) -> node_result())
                  | fun((beamai_context:t(), map() | undefined, term() | undefined) -> node_result()).
%% 节点返回值类型：
%% - {ok, State}: 执行成功
%% - {error, Reason}: 执行失败
%% - {interrupt, Reason, State}: 请求中断（human-in-the-loop）
%% - {command, Command}: Command 模式（同时指定 delta 和路由）
-type node_result() :: {ok, beamai_context:t()}
                     | {error, term()}
                     | {interrupt, term(), beamai_context:t()}
                     | {command, beamai_graph_command:command()}.
-type metadata() :: #{
    description => binary(),
    timeout => pos_integer(),
    retry => non_neg_integer()
}.
-type graph_node() :: #{
    id := node_id(),
    fun_ := node_fun(),
    metadata := metadata()
}.

-export_type([node_id/0, node_fun/0, node_result/0, graph_node/0, metadata/0]).

%% 特殊节点 ID 常量
-define(START_NODE, '__start__').
-define(END_NODE, '__end__').

%%====================================================================
%% 节点创建
%%====================================================================

%% @doc 创建新节点
%% Id: 节点唯一标识符
%% Fun: 节点执行函数
-spec new(node_id(), node_fun()) -> graph_node().
new(Id, Fun) ->
    new(Id, Fun, #{}).

%% @doc 创建带元数据的新节点
-spec new(node_id(), node_fun(), metadata()) -> graph_node().
new(Id, Fun, Metadata) when is_atom(Id), (is_function(Fun, 2) orelse is_function(Fun, 3)) ->
    #{
        id => Id,
        fun_ => Fun,
        metadata => Metadata
    };
new(Id, Fun, _Metadata) ->
    error({invalid_node, Id, Fun}).

%%====================================================================
%% 节点访问器
%%====================================================================

%% @doc 获取节点 ID
-spec id(graph_node()) -> node_id().
id(#{id := Id}) -> Id.

%% @doc 获取节点函数
-spec fun_(graph_node()) -> node_fun().
fun_(#{fun_ := Fun}) -> Fun.

%% @doc 获取节点元数据
-spec metadata(graph_node()) -> metadata().
metadata(#{metadata := Meta}) -> Meta.

%%====================================================================
%% 节点执行
%%====================================================================

%% @doc 执行节点，返回新状态或错误
-spec execute(graph_node(), beamai_context:t()) -> node_result().
execute(#{id := ?END_NODE}, State) ->
    %% 终止节点直接传递状态
    {ok, State};
execute(#{id := ?START_NODE}, State) ->
    %% 起始节点直接传递状态
    {ok, State};
execute(#{fun_ := Fun} = Node, State) ->
    try_execute(Node, Fun, State).

%% @doc 安全执行节点函数
%% 捕获所有异常并包装为错误
-spec try_execute(graph_node(), node_fun(), beamai_context:t()) -> node_result().
try_execute(Node, Fun, State) ->
    try call_node_fun(Fun, State, undefined, undefined) of
        {ok, NewState} when is_map(NewState) ->
            {ok, NewState};
        {command, Cmd} when is_map(Cmd) ->
            case beamai_graph_command:is_command(Cmd) of
                true -> {command, Cmd};
                false -> {error, {invalid_command, id(Node), Cmd}}
            end;
        {interrupt, Reason, NewState} when is_map(NewState) ->
            {interrupt, Reason, NewState};
        {error, Reason} ->
            {error, {node_error, id(Node), Reason}};
        Other ->
            {error, {invalid_return, id(Node), Other}}
    catch
        Class:Reason:Stacktrace ->
            {error, {node_exception, id(Node), Class, Reason, Stacktrace}}
    end.

%%====================================================================
%% 特殊节点
%%====================================================================

%% @doc 创建起始节点 (图入口)
-spec start_node() -> graph_node().
start_node() ->
    new(?START_NODE, fun(S, _) -> {ok, S} end, #{description => <<"图入口点"/utf8>>}).

%% @doc 创建终止节点 (图出口)
-spec end_node() -> graph_node().
end_node() ->
    new(?END_NODE, fun(S, _) -> {ok, S} end, #{description => <<"图出口点"/utf8>>}).

%% @doc 检查是否为起始节点
-spec is_start(graph_node() | node_id()) -> boolean().
is_start(#{id := Id}) -> is_start(Id);
is_start(?START_NODE) -> true;
is_start(_) -> false.

%% @doc 检查是否为终止节点
-spec is_end(graph_node() | node_id()) -> boolean().
is_end(#{id := Id}) -> is_end(Id);
is_end(?END_NODE) -> true;
is_end(_) -> false.

%%====================================================================
%% 节点验证
%%====================================================================

%% @doc 验证节点结构是否有效
-spec is_valid(term()) -> boolean().
is_valid(#{id := Id, fun_ := Fun}) when is_atom(Id), (is_function(Fun, 2) orelse is_function(Fun, 3)) ->
    true;
is_valid(_) ->
    false.

%%====================================================================
%% 内部辅助函数
%%====================================================================

%% @private 根据 arity 调用节点函数：fun/2 或 fun/3
call_node_fun(Fun, State, Input, ResumeData) ->
    case erlang:fun_info(Fun, arity) of
        {arity, 3} -> Fun(State, Input, ResumeData);
        {arity, 2} -> Fun(State, Input)
    end.
