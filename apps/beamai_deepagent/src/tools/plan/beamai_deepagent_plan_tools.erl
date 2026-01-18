%%%-------------------------------------------------------------------
%%% @doc 计划工具定义模块
%%%
%%% 定义计划管理相关工具：
%%% - create_plan: 创建执行计划
%%% - update_plan: 更新计划步骤状态
%%% - spawn_subtask: 派生并行子任务
%%% - reflect: 记录反思和分析
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_plan_tools).

-import(beamai_deepagent_tool_defs, [
    string_param/1,
    int_param/1,
    bool_param/1,
    array_param/2,
    enum_param/2
]).

%%====================================================================
%% 导出 API
%%====================================================================

-export([plan_tools/0, subtask_tools/0, reflect_tools/0]).
-export([
    create_plan_tool/0,
    update_plan_tool/0,
    spawn_subtask_tool/0,
    reflect_tool/0
]).

%%====================================================================
%% 工具集合
%%====================================================================

%% @doc 获取计划工具（create_plan, update_plan）
-spec plan_tools() -> [map()].
plan_tools() ->
    [create_plan_tool(), update_plan_tool()].

%% @doc 获取子任务工具
-spec subtask_tools() -> [map()].
subtask_tools() ->
    [spawn_subtask_tool()].

%% @doc 获取反思工具
-spec reflect_tools() -> [map()].
reflect_tools() ->
    [reflect_tool()].

%%====================================================================
%% 计划工具定义
%%====================================================================

%% @doc create_plan 工具 - 创建执行计划
-spec create_plan_tool() -> map().
create_plan_tool() ->
    #{
        name => <<"create_plan">>,
        description => <<"创建包含目标和步骤的执行计划。在复杂任务开始时使用，以结构化你的方法。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"goal">> => string_param(<<"计划的总体目标"/utf8>>),
                <<"steps">> => make_plan_steps_param()
            },
            required => [<<"goal">>, <<"steps">>]
        },
        handler => fun beamai_deepagent_plan_handlers:handle_create_plan/2
    }.

%% @doc update_plan 工具 - 更新计划步骤
-spec update_plan_tool() -> map().
update_plan_tool() ->
    #{
        name => <<"update_plan">>,
        description => <<"更新计划步骤的状态。用于标记步骤为进行中、已完成或失败。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"step_id">> => int_param(<<"要更新的步骤 ID（从 1 开始）"/utf8>>),
                <<"status">> => make_status_param(),
                <<"result">> => string_param(<<"此步骤的结果或备注"/utf8>>)
            },
            required => [<<"step_id">>, <<"status">>]
        },
        handler => fun beamai_deepagent_plan_handlers:handle_update_plan/2
    }.

%%====================================================================
%% 子任务工具定义
%%====================================================================

%% @doc spawn_subtask 工具 - 创建并行子任务
-spec spawn_subtask_tool() -> map().
spawn_subtask_tool() ->
    #{
        name => <<"spawn_subtask">>,
        description => <<"创建要并行执行的子任务。用于可以并行化的步骤。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"task_id">> => string_param(<<"此子任务的唯一标识符"/utf8>>),
                <<"description">> => string_param(<<"子任务应完成的内容"/utf8>>),
                <<"input">> => string_param(<<"子任务的输入/指令"/utf8>>)
            },
            required => [<<"task_id">>, <<"description">>, <<"input">>]
        },
        handler => fun beamai_deepagent_plan_handlers:handle_spawn_subtask/2
    }.

%%====================================================================
%% 反思工具定义
%%====================================================================

%% @doc reflect 工具 - 记录反思分析
-spec reflect_tool() -> map().
reflect_tool() ->
    #{
        name => <<"reflect">>,
        description => <<"反思当前进展并调整策略。用于分析已完成的内容并决定下一步。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"observation">> => string_param(<<"你从最近行动中观察到的内容"/utf8>>),
                <<"analysis">> => string_param(<<"你对情况的分析"/utf8>>),
                <<"next_action">> => string_param(<<"你计划下一步做什么"/utf8>>)
            },
            required => [<<"observation">>, <<"analysis">>, <<"next_action">>]
        },
        handler => fun beamai_deepagent_plan_handlers:handle_reflect/2
    }.

%%====================================================================
%% 私有函数 - 复杂参数定义
%%====================================================================

%% @private 创建计划步骤数组参数
-spec make_plan_steps_param() -> map().
make_plan_steps_param() ->
    #{
        type => array,
        items => make_step_item_schema(),
        description => <<"要执行的步骤列表"/utf8>>
    }.

%% @private 创建单个步骤的 JSON Schema
-spec make_step_item_schema() -> map().
make_step_item_schema() ->
    #{
        type => object,
        properties => #{
            <<"description">> => string_param(<<"此步骤的描述"/utf8>>),
            <<"dependencies">> => array_param(
                #{type => integer},
                <<"此步骤依赖的步骤 ID 列表（从 1 开始）"/utf8>>),
            <<"requires_subtask">> => bool_param(
                <<"此步骤是否应作为并行子任务执行"/utf8>>)
        },
        required => [<<"description">>]
    }.

%% @private 创建状态枚举参数
-spec make_status_param() -> map().
make_status_param() ->
    enum_param(
        [<<"pending">>, <<"in_progress">>, <<"completed">>, <<"failed">>, <<"skipped">>],
        <<"步骤的新状态"/utf8>>).
