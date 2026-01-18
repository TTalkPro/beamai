%%%-------------------------------------------------------------------
%%% @doc Human-in-the-Loop 工具定义
%%%
%%% 使用 beamai_tools 公共工具库的定义，结合 beamai_deepagent 特有的处理器。
%%%
%%% 与直接使用 beamai_tools 的区别：
%%% - 使用 beamai_deepagent_interaction 模块处理用户交互
%%% - 支持异步等待用户响应
%%% - 返回格式适配 beamai_deepagent 的工具执行流程
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_human_tools).

-include_lib("beamai_tools/include/beamai_tools.hrl").

%% API
-export([all/0]).
-export([ask_human_tool/0, confirm_action_tool/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 获取所有 Human 交互工具
%%
%% 从 beamai_tools 获取工具定义，替换为 beamai_deepagent 特有的处理器。
-spec all() -> [map()].
all() ->
    [ask_human_tool(), confirm_action_tool()].

%% @doc ask_human 工具 - Agent 主动向用户提问
%%
%% 使用 beamai_tools 的定义，覆盖 handler 为 beamai_deepagent 实现。
-spec ask_human_tool() -> map().
ask_human_tool() ->
    case beamai_tools:find_tool(<<"ask_human">>) of
        {ok, ToolDef} ->
            %% 工具定义已经是 map 格式，直接替换 handler
            ToolDef#{handler => fun beamai_deepagent_human_handlers:handle_ask_human/2};
        {error, not_found} ->
            fallback_ask_human_tool()
    end.

%% @doc confirm_action 工具 - 请求操作确认
%%
%% 使用 beamai_tools 的定义，覆盖 handler 为 beamai_deepagent 实现。
-spec confirm_action_tool() -> map().
confirm_action_tool() ->
    case beamai_tools:find_tool(<<"confirm_action">>) of
        {ok, ToolDef} ->
            %% 工具定义已经是 map 格式，直接替换 handler
            ToolDef#{handler => fun beamai_deepagent_human_handlers:handle_confirm_action/2};
        {error, not_found} ->
            fallback_confirm_action_tool()
    end.

%%====================================================================
%% 降级定义（当 beamai_tools 不可用时）
%%====================================================================

%% @private 降级 ask_human 定义
fallback_ask_human_tool() ->
    #{
        name => <<"ask_human">>,
        description => <<"向用户提问以获取更多信息、澄清需求或请求帮助。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"question">> => #{
                    type => string,
                    description => <<"要向用户提出的问题"/utf8>>
                },
                <<"context">> => #{
                    type => string,
                    description => <<"问题的上下文说明"/utf8>>
                },
                <<"options">> => #{
                    type => array,
                    items => #{type => string},
                    description => <<"可选的预设回答选项"/utf8>>
                }
            },
            required => [<<"question">>]
        },
        handler => fun beamai_deepagent_human_handlers:handle_ask_human/2
    }.

%% @private 降级 confirm_action 定义
fallback_confirm_action_tool() ->
    #{
        name => <<"confirm_action">>,
        description => <<"在执行重要或不可逆的操作前请求用户确认。"/utf8>>,
        parameters => #{
            type => object,
            properties => #{
                <<"action">> => #{
                    type => string,
                    description => <<"要执行的操作描述"/utf8>>
                },
                <<"reason">> => #{
                    type => string,
                    description => <<"为什么需要执行此操作"/utf8>>
                },
                <<"consequences">> => #{
                    type => string,
                    description => <<"操作可能的后果说明"/utf8>>
                }
            },
            required => [<<"action">>, <<"reason">>]
        },
        handler => fun beamai_deepagent_human_handlers:handle_confirm_action/2
    }.
