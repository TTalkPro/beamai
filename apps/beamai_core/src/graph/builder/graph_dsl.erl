%%%-------------------------------------------------------------------
%%% @doc 声明式图构建 DSL
%%%
%%% 提供 LangGraph 风格的声明式 API 来构建图。
%%% 相比命令式 Builder，DSL 更简洁直观。
%%%
%%% 使用示例:
%%% <pre>
%%% Graph = graph_dsl:build([
%%%     {node, agent, AgentFun},
%%%     {node, tools, ToolsFun},
%%%     {conditional_edge, agent, RouterFun},
%%%     {edge, tools, agent},
%%%     {entry, agent}
%%% ]).
%%% </pre>
%%%
%%% 支持的 DSL 元素:
%%% - {node, Name, Fun}              节点定义
%%% - {node, Name, Fun, Metadata}    带元数据的节点
%%% - {edge, From, To}               直接边
%%% - {fanout, From, Targets}        扇出边 (静态并行)
%%% - {conditional_edge, From, Fun}  条件边
%%% - {entry, Node}                  入口节点
%%% - {config, Key, Value}           配置项
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_dsl).

%% 主 API
-export([build/1, build/2]).

%% DSL 元素构造器 (可选，用于更清晰的语法)
-export([node/2, node/3]).
-export([edge/2, fanout/2, conditional_edge/2]).
-export([entry/1, config/2]).

%% 类型定义
-type node_id() :: atom().
-type node_fun() :: fun((map()) -> {ok, map()} | {error, term()}).
-type router_fun() :: fun((map()) -> node_id() | [node_id()] | [graph_send:send()]).

-type dsl_element() ::
    {node, node_id(), node_fun()} |
    {node, node_id(), node_fun(), map()} |
    {edge, node_id(), node_id()} |
    {fanout, node_id(), [node_id()]} |
    {conditional_edge, node_id(), router_fun()} |
    {entry, node_id()} |
    {config, atom(), term()}.

-export_type([dsl_element/0]).

%%====================================================================
%% 主 API
%%====================================================================

%% @doc 从 DSL 元素列表构建图
-spec build([dsl_element()]) -> {ok, graph_builder:graph()} | {error, term()}.
build(Specs) ->
    build(Specs, #{}).

%% @doc 从 DSL 元素列表构建图 (带默认配置)
-spec build([dsl_element()], map()) -> {ok, graph_builder:graph()} | {error, term()}.
build(Specs, DefaultConfig) ->
    Builder = graph_builder:new(DefaultConfig),
    case apply_specs(Specs, Builder) of
        {ok, FinalBuilder} ->
            graph_builder:compile(FinalBuilder);
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% DSL 元素构造器
%%====================================================================

%% @doc 创建节点元素
-spec node(node_id(), node_fun()) -> dsl_element().
node(Name, Fun) ->
    {node, Name, Fun}.

%% @doc 创建带元数据的节点元素
-spec node(node_id(), node_fun(), map()) -> dsl_element().
node(Name, Fun, Metadata) ->
    {node, Name, Fun, Metadata}.

%% @doc 创建边元素
-spec edge(node_id(), node_id()) -> dsl_element().
edge(From, To) ->
    {edge, From, To}.

%% @doc 创建扇出边元素
-spec fanout(node_id(), [node_id()]) -> dsl_element().
fanout(From, Targets) ->
    {fanout, From, Targets}.

%% @doc 创建条件边元素
-spec conditional_edge(node_id(), router_fun()) -> dsl_element().
conditional_edge(From, RouterFun) ->
    {conditional_edge, From, RouterFun}.

%% @doc 创建入口元素
-spec entry(node_id()) -> dsl_element().
entry(Node) ->
    {entry, Node}.

%% @doc 创建配置元素
-spec config(atom(), term()) -> dsl_element().
config(Key, Value) ->
    {config, Key, Value}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 应用所有 DSL 元素到 Builder
-spec apply_specs([dsl_element()], graph_builder:builder()) ->
    {ok, graph_builder:builder()} | {error, term()}.
apply_specs([], Builder) ->
    {ok, Builder};
apply_specs([Spec | Rest], Builder) ->
    case apply_spec(Spec, Builder) of
        {ok, NewBuilder} ->
            apply_specs(Rest, NewBuilder);
        {error, _} = Error ->
            Error
    end.

%% @doc 应用单个 DSL 元素
-spec apply_spec(dsl_element(), graph_builder:builder()) ->
    {ok, graph_builder:builder()} | {error, term()}.
apply_spec({node, Name, Fun}, Builder) ->
    {ok, graph_builder:add_node(Builder, Name, Fun)};

apply_spec({node, Name, Fun, Metadata}, Builder) ->
    {ok, graph_builder:add_node(Builder, Name, Fun, Metadata)};

apply_spec({edge, From, To}, Builder) ->
    {ok, graph_builder:add_edge(Builder, From, To)};

apply_spec({fanout, From, Targets}, Builder) ->
    {ok, graph_builder:add_fanout_edge(Builder, From, Targets)};

apply_spec({conditional_edge, From, RouterFun}, Builder) ->
    {ok, graph_builder:add_conditional_edge(Builder, From, RouterFun)};

apply_spec({entry, Node}, Builder) ->
    {ok, graph_builder:set_entry(Builder, Node)};

apply_spec({config, _Key, _Value}, Builder) ->
    %% 配置在 build/2 的 DefaultConfig 中处理
    %% 这里忽略，保持向前兼容
    {ok, Builder};

apply_spec(Unknown, _Builder) ->
    {error, {unknown_dsl_element, Unknown}}.
