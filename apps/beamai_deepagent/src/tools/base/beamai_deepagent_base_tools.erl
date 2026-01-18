%%%-------------------------------------------------------------------
%%% @doc 基础工具定义模块
%%%
%%% 定义始终可用的基础工具：
%%% - checkpoint: 创建执行检查点，标记重要里程碑
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
-export([checkpoint_tool/0, get_trace_tool/0]).

%%====================================================================
%% 工具集合
%%====================================================================

%% @doc 获取所有基础工具
-spec all() -> [map()].
all() ->
    [checkpoint_tool(), get_trace_tool()].

%%====================================================================
%% 工具定义
%%====================================================================

%% @doc checkpoint 工具 - 创建执行检查点
-spec checkpoint_tool() -> map().
checkpoint_tool() ->
    #{
        name => <<"checkpoint">>,
        description => <<"创建检查点以标记进度。在重要里程碑处使用。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"label">> => string_param(<<"检查点标签"/utf8>>),
                <<"notes">> => string_param(<<"关于当前状态的备注（可选）"/utf8>>)
            },
            required => [<<"label">>]
        },
        handler => fun beamai_deepagent_base_handlers:handle_checkpoint/2
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
