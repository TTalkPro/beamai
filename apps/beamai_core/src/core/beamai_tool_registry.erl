%%%-------------------------------------------------------------------
%%% @doc 工具注册表（声明侧）
%%%
%%% 工具的**注册、按名解析、给模型看的定义、框架元数据**都在这里；ChatClient
%%% 只负责**持有**这张表并把它交出来，不再自己回答"有哪些工具 / 这个工具能不能
%%% 并发 / 它的 schema 长什么样"。
%%%
%%% 对照 Spring AI 的工具体系（docs/api/tools.html）：
%%% - `resolve/2`           ← ToolCallbackResolver（按名找工具，**不执行**）
%%% - `specs/1`、`schemas/2` ← ToolDefinition（name/description/inputSchema，给模型看）
%%% - `serial/2`、`return_direct/2` ← ToolMetadata（框架行为，**不**给模型看）
%%% - 执行不在这里：见 beamai_tool_executor（对应 ToolCallingManager）
%%%
%%% 表本身就是一个 `#{Name => tool_spec()}` map（重名覆盖），没有额外包装。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_registry).

%% 注册
-export([new/0, add/2, add_many/2, from_module/2]).
%% 解析与查询
-export([resolve/2, list/1, by_tag/2]).
%% 给模型看的定义
-export([specs/1, schemas/1, schemas/2]).
%% 框架元数据
-export([serial/2, return_direct/2]).

-export_type([t/0]).

-type t() :: #{binary() => beamai_tool:tool_spec()}.

%%====================================================================
%% 注册
%%====================================================================

%% @doc 空注册表
-spec new() -> t().
new() -> #{}.

%% @doc 注册一个工具（以名称为键，重名覆盖）
-spec add(t(), beamai_tool:tool_spec()) -> t().
add(Registry, #{name := Name} = Tool) ->
    Registry#{Name => Tool}.

%% @doc 批量注册
-spec add_many(t(), [beamai_tool:tool_spec()]) -> t().
add_many(Registry, Tools) ->
    lists:foldl(fun(T, Acc) -> add(Acc, T) end, Registry, Tools).

%% @doc 从模块加载并注册（模块需实现 beamai_tool_behaviour 的 tools/0）
%%
%% 加载失败抛出 {tool_module_load_failed, Module, Reason}。
-spec from_module(t(), module()) -> t().
from_module(Registry, Module) ->
    case beamai_tool:from_module(Module) of
        {ok, Tools} -> add_many(Registry, Tools);
        {error, Reason} -> erlang:error({tool_module_load_failed, Module, Reason})
    end.

%%====================================================================
%% 解析与查询
%%====================================================================

%% @doc 按名称解析工具（对应 ToolCallbackResolver：只找，不执行）
-spec resolve(t(), binary()) -> {ok, beamai_tool:tool_spec()} | error.
resolve(Registry, ToolName) ->
    maps:find(ToolName, Registry).

%% @doc 列出全部工具
-spec list(t()) -> [beamai_tool:tool_spec()].
list(Registry) ->
    maps:values(Registry).

%% @doc 按标签筛选工具
-spec by_tag(t(), binary()) -> [beamai_tool:tool_spec()].
by_tag(Registry, Tag) ->
    [T || T <- maps:values(Registry), beamai_tool:has_tag(T, Tag)].

%%====================================================================
%% 给模型看的定义（ToolDefinition）
%%====================================================================

%% @doc 全部工具的统一 tool spec（name/description/parameters 中间格式）
-spec specs(t()) -> [map()].
specs(Registry) ->
    [beamai_tool:to_tool_spec(T) || T <- list(Registry)].

%% @doc 全部工具的 tool schema（默认 OpenAI 格式）
-spec schemas(t()) -> [map()].
schemas(Registry) ->
    schemas(Registry, openai).

%% @doc 全部工具的 tool schema（指定 provider 格式）
-spec schemas(t(), openai | anthropic | atom()) -> [map()].
schemas(Registry, Provider) ->
    [beamai_tool:to_tool_schema(T, Provider) || T <- list(Registry)].

%%====================================================================
%% 框架元数据（ToolMetadata：只影响框架行为，不进模型）
%%====================================================================

%% @doc 该工具是否串行（有副作用、需顺序执行）
%%
%% 未注册的名字返回 false：不因一个未知工具把整批退化成串行。
-spec serial(t(), binary()) -> boolean().
serial(Registry, ToolName) ->
    case resolve(Registry, ToolName) of
        {ok, ToolSpec} -> beamai_tool:is_serial(ToolSpec);
        error -> false
    end.

%% @doc 该工具结果是否直接作为最终答案（不回灌模型）
%%
%% 未注册的名字返回 false：直返会终止循环并丢弃同批其余结果，未知名字取保守值。
-spec return_direct(t(), binary()) -> boolean().
return_direct(Registry, ToolName) ->
    case resolve(Registry, ToolName) of
        {ok, ToolSpec} -> beamai_tool:is_return_direct(ToolSpec);
        error -> false
    end.
