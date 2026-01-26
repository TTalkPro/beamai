%%%-------------------------------------------------------------------
%%% @doc DeepAgent 计划工具模块
%%%
%%% 实现 beamai_tool_behaviour，为 Planner 子代理提供
%%% create_plan 工具，使 LLM 能够输出结构化计划。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_plan_plugin).
-behaviour(beamai_tool_behaviour).

-export([tool_info/0, tools/0]).

%%====================================================================
%% Tool Behaviour 回调
%%====================================================================

%% @doc 返回工具模块元信息
-spec tool_info() -> map().
tool_info() ->
    #{
        description => <<"Plan creation tool for DeepAgent planner">>,
        tags => [<<"deepagent">>, <<"plan">>]
    }.

%% @doc 返回工具定义列表
-spec tools() -> [beamai_tool:tool_spec()].
tools() ->
    [create_plan_tool()].

%%====================================================================
%% 内部函数 - 工具定义
%%====================================================================

%% @doc 创建 create_plan 工具定义
%%
%% 参数:
%%   goal - 计划的总体目标（必填）
%%   steps - 步骤列表，每个步骤包含：
%%     description - 步骤描述（必填）
%%     dependencies - 依赖的步骤 ID 列表（1-indexed）
%%     requires_deep - 是否需要深度处理
-spec create_plan_tool() -> beamai_tool:tool_spec().
create_plan_tool() ->
    #{
        name => <<"create_plan">>,
        handler => fun handle_create_plan/1,
        description => <<"Create a structured execution plan with steps and dependencies. "
                         "Each step should be an atomic unit of work. Steps can declare "
                         "dependencies on other steps by ID. Steps without mutual dependencies "
                         "can be executed in parallel.">>,
        tag => <<"plan">>,
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
    }.

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
