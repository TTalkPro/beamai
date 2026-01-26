%%%-------------------------------------------------------------------
%%% @doc 人机交互工具模块
%%%
%%% 提供人机交互（human-in-the-loop）工具：
%%% - ask_human: 向用户提问，获取更多信息或澄清
%%% - confirm_action: 请求用户确认重要操作
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_human).

-behaviour(beamai_tool_behaviour).

-export([tool_info/0, tools/0]).
-export([handle_ask_human/2, handle_confirm_action/2]).

%%====================================================================
%% 工具行为回调
%%====================================================================

tool_info() ->
    #{description => <<"Human interaction tools">>,
      tags => [<<"human">>, <<"interaction">>]}.

tools() ->
    [ask_human_tool(), confirm_action_tool()].

%%====================================================================
%% 工具定义
%%====================================================================

ask_human_tool() ->
    #{
        name => <<"ask_human">>,
        handler => fun ?MODULE:handle_ask_human/2,
        description => <<"Ask user a question for more information or clarification.">>,
        tag => <<"human">>,
        parameters => #{
            <<"question">> => #{type => string, description => <<"Question to ask">>, required => true},
            <<"context">> => #{type => string, description => <<"Context for the question">>},
            <<"options">> => #{type => array, description => <<"Preset answer options">>,
                              items => #{type => string}}
        },
        metadata => #{requires_human_input => true}
    }.

confirm_action_tool() ->
    #{
        name => <<"confirm_action">>,
        handler => fun ?MODULE:handle_confirm_action/2,
        description => <<"Request user confirmation before important operations.">>,
        tag => <<"human">>,
        parameters => #{
            <<"action">> => #{type => string, description => <<"Action description">>, required => true},
            <<"reason">> => #{type => string, description => <<"Why this action is needed">>, required => true},
            <<"consequences">> => #{type => string, description => <<"Potential consequences">>}
        },
        metadata => #{requires_human_input => true, blocking => true}
    }.

%%====================================================================
%% 处理器函数
%%====================================================================

handle_ask_human(Args, Context) ->
    Question = maps:get(<<"question">>, Args),
    QuestionContext = maps:get(<<"context">>, Args, undefined),
    Options = maps:get(<<"options">>, Args, []),
    RequestId = generate_request_id(),

    Request = #{
        type => ask_human,
        request_id => RequestId,
        question => Question,
        context => QuestionContext,
        options => Options,
        timestamp => erlang:system_time(millisecond)
    },

    case get_interaction_handler(Context) of
        undefined ->
            {ok, #{
                action => ask_human,
                pending => true,
                request_id => RequestId,
                question => Question,
                message => <<"Waiting for user input">>
            }};
        Handler when is_function(Handler, 1) ->
            case Handler(Request) of
                {ok, Response} ->
                    {ok, #{action => ask_human, success => true,
                           request_id => RequestId, response => Response}};
                {error, Reason} ->
                    {error, {human_interaction_error, Reason}}
            end
    end.

handle_confirm_action(Args, Context) ->
    Action = maps:get(<<"action">>, Args),
    Reason = maps:get(<<"reason">>, Args),
    Consequences = maps:get(<<"consequences">>, Args, undefined),
    RequestId = generate_request_id(),

    Request = #{
        type => confirm_action,
        request_id => RequestId,
        action => Action,
        reason => Reason,
        consequences => Consequences,
        timestamp => erlang:system_time(millisecond)
    },

    case get_interaction_handler(Context) of
        undefined ->
            {ok, #{
                action => confirm_action,
                pending => true,
                request_id => RequestId,
                action_description => Action,
                reason => Reason,
                message => <<"Waiting for user confirmation">>
            }};
        Handler when is_function(Handler, 1) ->
            case Handler(Request) of
                {ok, confirmed} ->
                    {ok, #{action => confirm_action, success => true,
                           confirmed => true, request_id => RequestId}};
                {ok, denied} ->
                    {ok, #{action => confirm_action, success => false,
                           confirmed => false, denied => true, request_id => RequestId}};
                {error, Reason} ->
                    {error, {human_interaction_error, Reason}}
            end
    end.

%%====================================================================
%% 内部函数
%%====================================================================

generate_request_id() ->
    Rand = rand:uniform(16#FFFFFFFF),
    Timestamp = erlang:system_time(millisecond),
    iolist_to_binary(io_lib:format("req_~p_~8.16.0b", [Timestamp, Rand])).

get_interaction_handler(Context) ->
    maps:get(human_interaction_handler, Context, undefined).
