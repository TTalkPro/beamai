%%%-------------------------------------------------------------------
%%% @doc 基础工具定义模块
%%%
%%% 定义始终可用的基础工具：
%%% - snapshot: 创建状态快照，标记重要里程碑
%%% - get_trace: 获取执行轨迹，用于调试和分析
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_base_tools).

-import(beamai_deepagent_tool_defs, [string_param/1, int_param/1]).

%%====================================================================
%% 导出 API
%%====================================================================

-export([all/0]).
-export([snapshot_tool/0, get_trace_tool/0]).

%%====================================================================
%% 工具集合
%%====================================================================

%% @doc 获取所有基础工具
-spec all() -> [map()].
all() ->
    [snapshot_tool(), get_trace_tool()].

%%====================================================================
%% 工具定义
%%====================================================================

%% @doc snapshot 工具 - 创建状态快照
-spec snapshot_tool() -> map().
snapshot_tool() ->
    #{
        name => <<"snapshot">>,
        description => <<"创建快照以标记进度。在重要里程碑处使用。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"label">> => string_param(<<"快照标签"/utf8>>),
                <<"notes">> => string_param(<<"关于当前状态的备注（可选）"/utf8>>)
            },
            required => [<<"label">>]
        },
        handler => fun beamai_deepagent_base_handlers:handle_snapshot/2
    }.

%% @doc get_trace 工具 - 获取执行轨迹
-spec get_trace_tool() -> map().
get_trace_tool() ->
    #{
        name => <<"get_trace">>,
        description => <<"获取最近的执行轨迹，用于调试或分析。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"limit">> => int_param(<<"返回的最大轨迹条目数（默认：10）"/utf8>>)
            }
        },
        handler => fun beamai_deepagent_base_handlers:handle_get_trace/2
    }.
