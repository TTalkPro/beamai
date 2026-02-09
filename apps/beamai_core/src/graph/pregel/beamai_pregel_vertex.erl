%%%-------------------------------------------------------------------
%%% @doc Pregel 顶点模块
%%%
%%% 顶点是 Pregel 图计算的基本单元。
%%%
%%% 扁平化顶点数据结构:
%%% - id: 唯一标识符（任意 Erlang term）
%%% - fun_: 节点计算函数
%%% - metadata: 节点元数据
%%% - routing_edges: 路由边列表（条件边）
%%% - halted: 布尔值，表示顶点是否已投票停止
%%%
%%% 设计模式: 不可变数据结构
%%% - 所有修改操作返回新顶点，原顶点不变
%%% - 适合并发环境，无需加锁
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_pregel_vertex).

%% === 构造函数 ===
-export([new_flat/4]).

%% === 读取操作 ===
-export([id/1, is_halted/1, is_active/1]).
-export([fun_/1, metadata/1, routing_edges/1]).

%% === 修改操作 ===
-export([halt/1, activate/1]).

%% === 类型导出 ===
-export_type([vertex/0, vertex_id/0]).

%%====================================================================
%% 类型定义
%%====================================================================

%% 顶点唯一标识符，可以是任意 Erlang term（通常是 atom 或 binary）。
-type vertex_id() :: term().

%% 扁平化顶点结构:
%% - id: 顶点唯一标识符
%% - fun_: 节点计算函数
%% - metadata: 节点元数据
%% - routing_edges: 路由边列表（条件边）
%% - halted: 是否已投票停止
-type vertex() :: #{
    id := vertex_id(),
    fun_ => term(),
    metadata => map(),
    routing_edges => list(),
    halted := boolean()
}.

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 创建扁平化顶点
%%
%% 直接在顶点上存储计算函数、元数据和路由边。
%%
%% 参数:
%% - Id: 顶点唯一标识符（通常与节点ID相同）
%% - Fun: 节点计算函数（来自 graph_node）
%% - Metadata: 节点元数据 map
%% - RoutingEdges: 路由边列表（条件边，用于决定下一步激活哪些顶点）
-spec new_flat(vertex_id(), term(), map(), list()) -> vertex().
new_flat(Id, Fun, Metadata, RoutingEdges) ->
    #{
        id => Id,
        fun_ => Fun,
        metadata => Metadata,
        routing_edges => RoutingEdges,
        halted => false
    }.

%%====================================================================
%% 读取操作
%%====================================================================

%% @doc 获取顶点ID
-spec id(vertex()) -> vertex_id().
id(#{id := Id}) -> Id.

%% @doc 获取节点计算函数
-spec fun_(vertex()) -> term().
fun_(#{fun_ := F}) -> F;
fun_(_) -> undefined.

%% @doc 获取节点元数据
-spec metadata(vertex()) -> map().
metadata(#{metadata := M}) -> M;
metadata(_) -> #{}.

%% @doc 获取路由边列表
-spec routing_edges(vertex()) -> list().
routing_edges(#{routing_edges := E}) -> E;
routing_edges(_) -> [].

%% @doc 检查顶点是否已停止
-spec is_halted(vertex()) -> boolean().
is_halted(#{halted := H}) -> H.

%% @doc 检查顶点是否活跃
-spec is_active(vertex()) -> boolean().
is_active(V) -> not is_halted(V).

%%====================================================================
%% 修改操作
%%====================================================================

%% @doc 投票停止（将顶点标记为已停止）
-spec halt(vertex()) -> vertex().
halt(V) -> V#{halted => true}.

%% @doc 激活顶点（将已停止的顶点重新激活）
-spec activate(vertex()) -> vertex().
activate(V) -> V#{halted => false}.
