%%%-------------------------------------------------------------------
%%% @doc 工具注册表模块
%%%
%%% 统一管理所有工具的注册和查询：
%%% - 工具集合 API：获取各类工具集合
%%% - 条件判断：根据配置决定可用工具
%%% - 工具发现：按名称查找工具处理器
%%%
%%% 工具类型：
%%% - 基础工具：checkpoint, get_trace（始终可用）
%%% - 计划工具：create_plan, update_plan（depth=0 时可用）
%%% - 子任务工具：spawn_subtask（depth < max_depth 时可用）
%%% - 反思工具：reflect（reflection_enabled 时可用）
%%% - 文件系统工具：ls, read_file 等（filesystem_enabled 时可用）
%%% - TodoList 工具：write_todos, read_todos（planning_mode=simple 时可用）
%%% - Human 工具：ask_human, confirm_action（human_in_loop.enabled 时可用）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_tool_registry).

%%====================================================================
%% 导出 API
%%====================================================================

%% 主要 API
-export([
    all_tools/1,       %% 根据配置获取所有可用工具
    find_handler/2     %% 按名称查找处理器
]).

%% 工具集合（供内部和测试使用）
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
%% 主要 API
%%====================================================================

%% @doc 根据配置获取所有可用工具
%%
%% 根据以下条件组合工具集：
%% - 基础工具：始终包含
%% - 计划工具：planning_mode=full 且 depth=0
%% - TodoList 工具：planning_mode=simple
%% - 子任务工具：depth < max_depth
%% - 反思工具：reflection_enabled=true
%% - 文件系统工具：filesystem_enabled=true 或有 filesystem 配置
-spec all_tools(map()) -> [map()].
all_tools(Config) ->
    ToolSets = [
        {base_tools(), true},
        {plan_tools(), should_include_plan_tools(Config)},
        {todo_tools(), should_include_todo_tools(Config)},
        {subtask_tools(), should_include_subtask_tools(Config)},
        {reflect_tools(), should_include_reflect_tools(Config)},
        {filesystem_tools(), should_include_filesystem_tools(Config)},
        {human_tools(), should_include_human_tools(Config)}
    ],
    lists:append([Tools || {Tools, true} <- ToolSets]).

%% @doc 按名称查找工具处理器
%%
%% @param ToolName 工具名称
%% @param Tools 工具列表
%% @returns {ok, Handler} | not_found
-spec find_handler(binary(), [map()]) -> {ok, function()} | not_found.
find_handler(ToolName, Tools) ->
    case lists:search(fun(#{name := N}) -> N =:= ToolName end, Tools) of
        {value, #{handler := Handler}} -> {ok, Handler};
        false -> not_found
    end.

%%====================================================================
%% 工具集合 API
%%====================================================================

%% @doc 获取基础工具集合（始终可用）
-spec base_tools() -> [map()].
base_tools() ->
    beamai_deepagent_base_tools:all().

%% @doc 获取计划工具集合
-spec plan_tools() -> [map()].
plan_tools() ->
    beamai_deepagent_plan_tools:plan_tools().

%% @doc 获取子任务工具集合
-spec subtask_tools() -> [map()].
subtask_tools() ->
    beamai_deepagent_plan_tools:subtask_tools().

%% @doc 获取反思工具集合
-spec reflect_tools() -> [map()].
reflect_tools() ->
    beamai_deepagent_plan_tools:reflect_tools().

%% @doc 获取文件系统工具集合
-spec filesystem_tools() -> [map()].
filesystem_tools() ->
    beamai_deepagent_fs_tools:all().

%% @doc 获取 TodoList 工具集合
-spec todo_tools() -> [map()].
todo_tools() ->
    beamai_deepagent_todo_tools:all().

%% @doc 获取 Human 交互工具集合
-spec human_tools() -> [map()].
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
