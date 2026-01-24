%%%-------------------------------------------------------------------
%%% @doc DeepAgent 计划工具插件
%%%
%%% 实现 beamai_plugin_behaviour，为 Planner 子代理提供
%%% create_plan 工具，使 LLM 能够输出结构化计划。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_plan_plugin).
-behaviour(beamai_plugin_behaviour).

-export([plugin_info/0, functions/0]).

%%====================================================================
%% Plugin Behaviour 回调
%%====================================================================

%% @doc 返回插件元信息
%%
%% name 用作工具名称前缀，最终工具名为 "deepagent_plan.create_plan"。
-spec plugin_info() -> map().
plugin_info() ->
    #{
        name => <<"deepagent_plan">>,
        description => <<"Plan creation tool for DeepAgent planner">>
    }.

%% @doc 返回插件提供的工具函数列表
-spec functions() -> [beamai_function:t()].
functions() ->
    [create_plan_function()].

%%====================================================================
%% 内部函数 - 工具定义
%%====================================================================

%% @doc 创建 create_plan 工具函数定义
%%
%% 参数:
%%   goal - 计划的总体目标（必填）
%%   steps - 步骤列表，每个步骤包含：
%%     description - 步骤描述（必填）
%%     dependencies - 依赖的步骤 ID 列表（1-indexed）
%%     requires_deep - 是否需要深度处理
-spec create_plan_function() -> beamai_function:t().
create_plan_function() ->
    beamai_function:new(<<"create_plan">>, fun handle_create_plan/1, #{
        description => <<"Create a structured execution plan with steps and dependencies. "
                         "Each step should be an atomic unit of work. Steps can declare "
                         "dependencies on other steps by ID. Steps without mutual dependencies "
                         "can be executed in parallel.">>,
        parameters => #{
            <<"goal">> => #{
                type => string,
                description => <<"The overall goal of the plan">>,
                required => true
            },
            <<"steps">> => #{
                type => array,
                description => <<"List of plan steps">>,
                required => true,
                items => #{
                    type => object,
                    properties => #{
                        <<"description">> => #{
                            type => string,
                            description => <<"What this step should accomplish">>,
                            required => true
                        },
                        <<"dependencies">> => #{
                            type => array,
                            description => <<"IDs (1-indexed) of steps this step depends on">>,
                            items => #{type => integer}
                        },
                        <<"requires_deep">> => #{
                            type => boolean,
                            description => <<"Whether this step requires deep/complex processing">>
                        }
                    }
                }
            }
        }
    }).

%%====================================================================
%% 内部函数 - 工具处理器
%%====================================================================

%% @doc 处理 create_plan 工具调用
%%
%% 使用 beamai_deepagent_plan:new/2 创建计划数据结构，
%% 返回 JSON 格式的确认信息供 LLM 知晓计划已创建。
-spec handle_create_plan(map()) -> {ok, binary()} | {error, binary()}.
handle_create_plan(#{<<"goal">> := Goal, <<"steps">> := Steps}) ->
    Plan = beamai_deepagent_plan:new(Goal, Steps),
    PlanMap = beamai_deepagent_plan:to_map(Plan),
    {ok, jsx:encode(#{
        status => <<"plan_created">>,
        goal => Goal,
        step_count => length(Steps),
        plan => PlanMap
    })};
handle_create_plan(_Args) ->
    {error, <<"Missing required parameters: goal and steps">>}.
