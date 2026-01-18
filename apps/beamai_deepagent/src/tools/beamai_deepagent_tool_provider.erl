%%%-------------------------------------------------------------------
%%% @doc DeepAgent 工具提供者
%%%
%%% 实现 beamai_tool_provider 行为，将 DeepAgent 的工具纳入统一的工具管理体系。
%%% 根据配置上下文动态决定可用的工具集合。
%%%
%%% == 工具类型 ==
%%%
%%% - 基础工具：checkpoint, get_trace（始终可用）
%%% - 计划工具：create_plan, update_plan（depth=0 且 planning_mode=full 时可用）
%%% - 子任务工具：spawn_subtask（depth < max_depth 时可用）
%%% - 反思工具：reflect（reflection_enabled 时可用）
%%% - 文件系统工具：ls, read_file 等（filesystem_enabled 时可用）
%%% - TodoList 工具：write_todos, read_todos（planning_mode=simple 时可用）
%%% - Human 工具：ask_human, confirm_action（human_in_loop.enabled 时可用）
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 通过 beamai_tool_registry 使用
%%% Config = #{depth => 0, planning_mode => full},
%%% Tools = beamai_tool_registry:from_config(#{
%%%     providers => [{beamai_deepagent_tool_provider, Config}]
%%% }).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_tool_provider).
-behaviour(beamai_tool_provider).

-include_lib("beamai_tools/include/beamai_tools.hrl").

%%====================================================================
%% 行为回调
%%====================================================================

-export([
    list_tools/1,
    find_tool/2,
    info/0,
    available/0
]).

%%====================================================================
%% 工具集合 API（供直接访问）
%%====================================================================

-export([
    base_tools/0,
    plan_tools/0,
    subtask_tools/0,
    reflect_tools/0,
    filesystem_tools/0,
    todo_tools/0,
    human_tools/0
]).

%%====================================================================
%% beamai_tool_provider 回调实现
%%====================================================================

%% @doc 返回 Provider 元信息
-spec info() -> map().
info() ->
    #{
        name => <<"deepagent">>,
        version => <<"0.1.0">>,
        description => <<"DeepAgent 工具提供者，支持计划、子任务、反思等工具">>,
        categories => [plan, todo, file, human, custom]
    }.

%% @doc 检查 Provider 是否可用
-spec available() -> boolean().
available() ->
    true.

%% @doc 根据配置返回可用工具列表
%%
%% Opts 参数：
%% - context: 包含 DeepAgent 配置的 map，可包含：
%%   - depth: 当前递归深度（默认 0）
%%   - max_depth: 最大递归深度（默认 3）
%%   - planning_mode: 计划模式（full | simple，默认 full）
%%   - planning_enabled: 是否启用计划（默认 true）
%%   - reflection_enabled: 是否启用反思（默认 true）
%%   - filesystem_enabled: 是否启用文件系统工具
%%   - filesystem: 文件系统配置
%%   - human_in_loop: Human-in-the-loop 配置
-spec list_tools(map()) -> {ok, [tool_def()]}.
list_tools(Opts) ->
    Config = maps:get(context, Opts, #{}),
    ToolSets = [
        {base_tools(), true},
        {plan_tools(), should_include_plan_tools(Config)},
        {todo_tools(), should_include_todo_tools(Config)},
        {subtask_tools(), should_include_subtask_tools(Config)},
        {reflect_tools(), should_include_reflect_tools(Config)},
        {filesystem_tools(), should_include_filesystem_tools(Config)},
        {human_tools(), should_include_human_tools(Config)}
    ],
    Tools = lists:append([Tools || {Tools, true} <- ToolSets]),
    {ok, Tools}.

%% @doc 按名称查找工具
%%
%% @param Name 工具名称
%% @param Opts 选项（包含 context 配置）
%% @returns {ok, ToolDef} | {error, not_found}
-spec find_tool(binary(), map()) -> {ok, tool_def()} | {error, not_found}.
find_tool(Name, Opts) ->
    case list_tools(Opts) of
        {ok, Tools} ->
            case lists:search(fun(#{name := N}) -> N =:= Name end, Tools) of
                {value, Tool} -> {ok, Tool};
                false -> {error, not_found}
            end;
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 工具集合 API
%%====================================================================

%% @doc 获取基础工具集合（始终可用）
-spec base_tools() -> [tool_def()].
base_tools() ->
    beamai_deepagent_base_tools:all().

%% @doc 获取计划工具集合
-spec plan_tools() -> [tool_def()].
plan_tools() ->
    beamai_deepagent_plan_tools:plan_tools().

%% @doc 获取子任务工具集合
-spec subtask_tools() -> [tool_def()].
subtask_tools() ->
    beamai_deepagent_plan_tools:subtask_tools().

%% @doc 获取反思工具集合
-spec reflect_tools() -> [tool_def()].
reflect_tools() ->
    beamai_deepagent_plan_tools:reflect_tools().

%% @doc 获取文件系统工具集合
-spec filesystem_tools() -> [tool_def()].
filesystem_tools() ->
    beamai_deepagent_fs_tools:all().

%% @doc 获取 TodoList 工具集合
-spec todo_tools() -> [tool_def()].
todo_tools() ->
    beamai_deepagent_todo_tools:all().

%% @doc 获取 Human 交互工具集合
-spec human_tools() -> [tool_def()].
human_tools() ->
    beamai_deepagent_human_tools:all().

%%====================================================================
%% 私有函数 - 工具条件判断
%%====================================================================

%% @private 判断是否应包含计划工具
%%
%% 条件：planning_mode=full（默认）且 depth=0
-spec should_include_plan_tools(map()) -> boolean().
should_include_plan_tools(Config) ->
    PlanningMode = maps:get(planning_mode, Config, full),
    PlanningEnabled = maps:get(planning_enabled, Config, true),
    Depth = maps:get(depth, Config, 0),
    PlanningEnabled andalso PlanningMode =:= full andalso Depth =:= 0.

%% @private 判断是否应包含子任务工具
%%
%% 条件：depth < max_depth
-spec should_include_subtask_tools(map()) -> boolean().
should_include_subtask_tools(Config) ->
    Depth = maps:get(depth, Config, 0),
    MaxDepth = maps:get(max_depth, Config, 3),
    Depth < MaxDepth.

%% @private 判断是否应包含反思工具
%%
%% 条件：reflection_enabled=true
-spec should_include_reflect_tools(map()) -> boolean().
should_include_reflect_tools(Config) ->
    maps:get(reflection_enabled, Config, true).

%% @private 判断是否应包含 TodoList 工具
%%
%% 条件：planning_mode=simple
-spec should_include_todo_tools(map()) -> boolean().
should_include_todo_tools(Config) ->
    PlanningMode = maps:get(planning_mode, Config, full),
    PlanningMode =:= simple.

%% @private 判断是否应包含文件系统工具
%%
%% 条件：filesystem_enabled=true 或有 filesystem 配置
-spec should_include_filesystem_tools(map()) -> boolean().
should_include_filesystem_tools(Config) ->
    case maps:get(filesystem_enabled, Config, undefined) of
        undefined -> maps:is_key(filesystem, Config);
        Enabled -> Enabled
    end.

%% @private 判断是否应包含 Human 交互工具
%%
%% 条件：human_in_loop.enabled=true（默认启用）
-spec should_include_human_tools(map()) -> boolean().
should_include_human_tools(Config) ->
    HumanConfig = maps:get(human_in_loop, Config, #{}),
    maps:get(enabled, HumanConfig, true).
