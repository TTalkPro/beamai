%%%-------------------------------------------------------------------
%%% @doc Graph Command 模块
%%%
%%% 实现 LangGraph 风格的 Command 返回类型，允许节点函数在单个返回值中
%%% 同时指定状态增量更新和路由目标。
%%%
%%% Command 语义：
%%% - update: 直接作为 delta（跳过 compute_delta 全状态 diff）
%%% - goto: 覆盖边路由（undefined 时回退到正常边路由）
%%% - graph: 目标图（current/parent/undefined）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_command).

%% 构造器
-export([new/1, goto/1, goto/2, update/1, update/2]).
%% 访问器
-export([get_goto/1, get_update/1, get_graph/1]).
%% 类型判断
-export([is_command/1]).

%% 类型定义
-type goto_target() :: atom()                        %% 单节点
                     | [atom()]                      %% 多节点并行
                     | graph_dispatch:dispatch()     %% 单 dispatch
                     | [graph_dispatch:dispatch()].  %% 多 dispatch

-type command() :: #{
    '__command__' := true,
    update := map(),                    %% 增量状态更新 (delta)
    goto := goto_target() | undefined,  %% 路由目标
    graph := current | parent | undefined  %% 目标图
}.

-export_type([command/0, goto_target/0]).

%%====================================================================
%% 构造器
%%====================================================================

%% @doc 创建 Command
%%
%% 接受选项 map，支持以下字段：
%% - update: 增量状态更新（默认 #{}）
%% - goto: 路由目标（默认 undefined）
%% - graph: 目标图（默认 undefined）
-spec new(map()) -> command().
new(Opts) when is_map(Opts) ->
    Update = maps:get(update, Opts, #{}),
    Goto = maps:get(goto, Opts, undefined),
    Graph = maps:get(graph, Opts, undefined),
    validate_and_build(Update, Goto, Graph).

%% @doc 快捷构造器：仅 goto
-spec goto(goto_target()) -> command().
goto(Target) ->
    validate_and_build(#{}, Target, undefined).

%% @doc 快捷构造器：goto + update
-spec goto(goto_target(), map()) -> command().
goto(Target, Update) when is_map(Update) ->
    validate_and_build(Update, Target, undefined).

%% @doc 快捷构造器：仅 update
-spec update(map()) -> command().
update(Update) when is_map(Update) ->
    validate_and_build(Update, undefined, undefined).

%% @doc 快捷构造器：update + goto
-spec update(map(), goto_target()) -> command().
update(Update, Target) when is_map(Update) ->
    validate_and_build(Update, Target, undefined).

%%====================================================================
%% 访问器
%%====================================================================

%% @doc 获取路由目标
-spec get_goto(command()) -> goto_target() | undefined.
get_goto(#{'__command__' := true, goto := Goto}) -> Goto.

%% @doc 获取增量更新
-spec get_update(command()) -> map().
get_update(#{'__command__' := true, update := Update}) -> Update.

%% @doc 获取目标图
-spec get_graph(command()) -> current | parent | undefined.
get_graph(#{'__command__' := true, graph := Graph}) -> Graph.

%%====================================================================
%% 类型判断
%%====================================================================

%% @doc 判断是否为有效的 Command
-spec is_command(term()) -> boolean().
is_command(#{'__command__' := true, update := U, goto := G, graph := Gr})
  when is_map(U) ->
    valid_goto(G) andalso valid_graph(Gr);
is_command(_) ->
    false.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 验证参数并构建 Command
-spec validate_and_build(map(), goto_target() | undefined, current | parent | undefined) -> command().
validate_and_build(Update, Goto, Graph)
  when is_map(Update) ->
    case valid_goto(Goto) andalso valid_graph(Graph) of
        true ->
            #{
                '__command__' => true,
                update => Update,
                goto => Goto,
                graph => Graph
            };
        false ->
            error({invalid_command_options, #{update => Update, goto => Goto, graph => Graph}})
    end.

%% @private 验证 goto 目标
-spec valid_goto(term()) -> boolean().
valid_goto(undefined) -> true;
valid_goto(Target) when is_atom(Target) -> true;
valid_goto([]) -> true;
valid_goto([First | _] = List) when is_atom(First) ->
    lists:all(fun is_atom/1, List);
valid_goto(Dispatch) when is_map(Dispatch) ->
    graph_dispatch:is_dispatch(Dispatch);
valid_goto([First | _] = List) when is_map(First) ->
    graph_dispatch:is_dispatches(List);
valid_goto(_) -> false.

%% @private 验证 graph 目标
-spec valid_graph(term()) -> boolean().
valid_graph(undefined) -> true;
valid_graph(current) -> true;
valid_graph(parent) -> true;
valid_graph(_) -> false.
