%%%-------------------------------------------------------------------
%%% @doc 协调器公共工具模块
%%%
%%% 提供 Pipeline 和 Orchestrator 协调器的公共函数。
%%% 所有函数都是纯函数或简单封装，无复杂状态管理。
%%%
%%% 功能分类：
%%%   - Worker 管理：启动、初始化
%%%   - 工具构建：委托、路由、并行、综合
%%%   - 提示词：协调器系统提示词
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_coordinator_common).

%% 导出公共 API
-export([start_workers/3]).
-export([build_delegate_tools/2, build_router_tool/2]).
-export([build_parallel_tool/2, build_synthesize_tool/0]).
-export([build_pipeline_prompt/0, build_orchestrator_prompt/0]).

%%====================================================================
%% Worker 管理函数
%%====================================================================

%% @doc 启动所有 workers
%%
%% 为协调器启动多个子 Agent，每个 Agent 负责特定角色。
%%
%% @param ParentId 父协调器 ID
%% @param Agents Agent 定义列表
%% @param LLMConfig LLM 配置
%% @returns {成功启动数量, 名称到 PID 的映射}
-spec start_workers(binary(), [map()], map()) -> {pos_integer(), #{binary() => pid()}}.
start_workers(ParentId, Agents, LLMConfig) ->
    lists:foldl(fun(AgentDef, {Count, PidsAcc}) ->
        do_start_worker(ParentId, AgentDef, LLMConfig, Count, PidsAcc)
    end, {0, #{}}, Agents).

%% @private 启动单个 worker
-spec do_start_worker(binary(), map(), map(), pos_integer(), #{binary() => pid()}) ->
    {pos_integer(), #{binary() => pid()}}.
do_start_worker(ParentId, AgentDef, LLMConfig, Count, PidsAcc) ->
    Name = maps:get(name, AgentDef),
    Role = maps:get(role, AgentDef, Name),
    SystemPrompt = maps:get(system_prompt, AgentDef,
        <<"你是 ", Role/binary, "，负责完成分配给你的任务。"/utf8>>),

    WorkerId = <<ParentId/binary, "_worker_", Name/binary>>,
    WorkerOpts = #{
        system_prompt => SystemPrompt,
        llm => LLMConfig
    },

    case beamai_agent:start_link(WorkerId, WorkerOpts) of
        {ok, Pid} ->
            {Count + 1, PidsAcc#{Name => Pid}};
        {error, Reason} ->
            error_logger:warning_msg("Failed to start worker ~s: ~p~n", [Name, Reason]),
            {Count, PidsAcc}
    end.

%%====================================================================
%% 工具构建函数
%%====================================================================

%% @doc 构建委托工具列表
%%
%% 为每个 Agent 创建一个委托工具，允许协调器将任务委托给特定 worker。
%%
%% @param Agents Agent 定义列表
%% @param Pids Worker PID 映射
%% @returns 工具列表
-spec build_delegate_tools([map()], #{binary() => pid()}) -> [map()].
build_delegate_tools(Agents, Pids) ->
    [build_delegate_tool(Agent, Pids) || Agent <- Agents].

%% @doc 构建单个委托工具
%%
%% 创建一个工具，用于将任务委托给特定的 worker。
%%
%% @param Agent Agent 定义
%% @param Pids Worker PID 映射
%% @returns 工具定义
-spec build_delegate_tool(map(), #{binary() => pid()}) -> map().
build_delegate_tool(#{name := Name} = Agent, Pids) ->
    Role = maps:get(role, Agent, Name),
    #{
        name => <<"delegate_to_", Name/binary>>,
        description => <<"委托任务给 ", Role/binary, "（", Name/binary, "）"/utf8>>,
        input_schema => #{
            type => object,
            properties => #{
                <<"task">> => #{
                    type => string,
                    description => <<"要执行的任务描述"/utf8>>
                }
            },
            required => [<<"task">>]
        },
        handler => fun(#{<<"task">> := Task}, _Context) ->
            delegate_to_worker(Name, Pids, Task)
        end
    }.

%% @private 委托任务给 worker
-spec delegate_to_worker(binary(), #{binary() => pid()}, binary()) ->
    {ok, binary()} | {error, term()}.
delegate_to_worker(Name, Pids, Task) ->
    case maps:find(Name, Pids) of
        {ok, Pid} ->
            case beamai_agent:run(Pid, Task, #{timeout => 120000}) of
                {ok, Result} ->
                    {ok, maps:get(final_response, Result, <<>>)};
                {error, Reason} ->
                    {error, Reason}
            end;
        error ->
            {error, {worker_not_found, Name}}
    end.

%% @doc 构建路由工具
%%
%% 创建一个工具，用于根据任务特点选择最合适的 worker。
%%
%% @param Agents Agent 定义列表
%% @param Pids Worker PID 映射
%% @returns 路由工具定义
-spec build_router_tool([map()], #{binary() => pid()}) -> map().
build_router_tool(_Agents, _Pids) ->
    #{
        name => <<"route_to_workers">>,
        description => <<"根据任务特点自动选择最合适的 worker"/utf8>>,
        input_schema => #{
            type => object,
            properties => #{
                <<"task">> => #{
                    type => string,
                    description => <<"任务描述"/utf8>>
                },
                <<"candidates">> => #{
                    type => array,
                    description => <<"候选 worker 列表"/utf8>>,
                    items => #{type => string}
                }
            },
            required => [<<"task">>]
        },
        handler => fun(#{<<"task">> := _Task, <<"candidates">> := Candidates}, _Context) ->
            CandidatesList = format_candidates(Candidates),
            {ok, <<"可用的 workers: ", CandidatesList/binary>>}
        end
    }.

%% @private 格式化候选列表
-spec format_candidates([binary()]) -> binary().
format_candidates(Candidates) ->
    iolist_to_binary([[<<", ">>, C] || C <- Candidates]).

%% @doc 构建并行执行工具
%%
%% 创建一个工具，用于并行调用多个 workers 并综合结果。
%%
%% @param Agents Agent 定义列表
%% @param Pids Worker PID 映射
%% @returns 并行执行工具定义
-spec build_parallel_tool([map()], #{binary() => pid()}) -> map().
build_parallel_tool(_Agents, _Pids) ->
    #{
        name => <<"execute_parallel">>,
        description => <<"并行调用多个 workers 并综合结果"/utf8>>,
        input_schema => #{
            type => object,
            properties => #{
                <<"task">> => #{
                    type => string,
                    description => <<"要执行的并行任务"/utf8>>
                },
                <<"workers">> => #{
                    type => array,
                    description => <<"要调用的 worker 名称列表"/utf8>>,
                    items => #{type => string}
                }
            },
            required => [<<"task">>, <<"workers">>]
        },
        handler => fun(#{<<"task">> := _Task, <<"workers">> := WorkerNames}, _Context) ->
            WorkersList = format_workers_list(WorkerNames),
            {ok, <<"并行执行已安排: ", WorkersList/binary>>}
        end
    }.

%% @private 格式化 worker 列表
-spec format_workers_list([binary()]) -> binary().
format_workers_list(WorkerNames) ->
    iolist_to_binary([[<<", ">>, W] || W <- WorkerNames]).

%% @doc 构建综合工具
%%
%% 创建一个工具，用于综合多个 worker 的结果。
%%
%% @returns 综合工具定义
-spec build_synthesize_tool() -> map().
build_synthesize_tool() ->
    #{
        name => <<"synthesize_results">>,
        description => <<"综合多个 worker 的结果"/utf8>>,
        input_schema => #{
            type => object,
            properties => #{
                <<"results">> => #{
                    type => object,
                    description => <<"各个 worker 的结果"/utf8>>
                }
            },
            required => [<<"results">>]
        },
        handler => fun(#{<<"results">> := Results}, _Context) ->
            {ok, synthesize_results(Results)}
        end
    }.

%% @private 综合多个结果
-spec synthesize_results(map()) -> binary().
synthesize_results(Results) ->
    FormattedResults = [[Name, <<": ">>, Result, <<"\n\n">>] || {Name, Result} <- maps:to_list(Results)],
    iolist_to_binary([
        <<"以下是综合结果：\n\n"/utf8>>,
        FormattedResults
    ]).

%%====================================================================
%% 提示词构建函数
%%====================================================================

%% @doc 构建 Pipeline 模式的系统提示词
%%
%% Pipeline 模式用于顺序协调，任务在 workers 间依次传递。
%%
%% @returns 系统提示词
-spec build_pipeline_prompt() -> binary().
build_pipeline_prompt() ->
    <<"你是一个流水线协调器，负责将任务按顺序委托给团队成员，并将结果传递给下一个成员。"/utf8>>.

%% @doc 构建 Orchestrator 模式的系统提示词
%%
%% Orchestrator 模式用于编排协调，可以委托、路由、并行调用多个 workers。
%%
%% @returns 系统提示词
-spec build_orchestrator_prompt() -> binary().
build_orchestrator_prompt() ->
    <<"你是一个任务编排器，负责分析任务、选择合适的执行者、协调执行并综合结果。"/utf8>>.
