%%%-------------------------------------------------------------------
%%% @doc 人工交互工具
%%%
%%% 提供与用户交互的工具：
%%% - ask_human: 向用户提问
%%% - confirm_action: 请求用户确认操作
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tools_human).

-include("beamai_tools.hrl").

%% 导入 DSL 函数
-import(beamai_tool, [define/5]).

%% API
-export([all/0]).

%% 工具处理器
-export([
    handle_ask_human/1,
    handle_ask_human/2,
    handle_confirm_action/1,
    handle_confirm_action/2
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 获取所有人工交互工具定义
-spec all() -> [tool_def()].
all() ->
    [ask_human_tool(), confirm_action_tool()].

%%====================================================================
%% 工具定义（使用 DSL）
%%====================================================================

%% @private ask_human 工具
ask_human_tool() ->
    define(<<"ask_human">>,
           <<"向用户提问以获取更多信息、澄清需求或请求帮助。"/utf8>>,
           #{category => human, metadata => #{requires_human_input => true}},
           [
               {<<"question">>, string, <<"要向用户提出的问题"/utf8>>, required},
               {<<"context">>, string, <<"问题的上下文说明（可选）"/utf8>>},
               {<<"options">>, array, <<"可选的预设回答选项（可选）"/utf8>>}
           ],
           fun ?MODULE:handle_ask_human/2).

%% @private confirm_action 工具
confirm_action_tool() ->
    define(<<"confirm_action">>,
           <<"在执行重要或不可逆的操作前请求用户确认。"/utf8>>,
           #{category => human, metadata => #{requires_human_input => true, blocking => true}},
           [
               {<<"action">>, string, <<"要执行的操作描述"/utf8>>, required},
               {<<"reason">>, string, <<"为什么需要执行此操作"/utf8>>, required},
               {<<"consequences">>, string, <<"操作可能的后果说明（可选）"/utf8>>}
           ],
           fun ?MODULE:handle_confirm_action/2).

%%====================================================================
%% 工具处理器
%%====================================================================

%% @doc 向用户提问
%%
%% 返回一个待处理的请求，需要外部系统收集用户输入后继续。
handle_ask_human(Args) ->
    handle_ask_human(Args, #{}).

handle_ask_human(Args, Context) ->
    Question = maps:get(<<"question">>, Args),
    QuestionContext = maps:get(<<"context">>, Args, undefined),
    Options = maps:get(<<"options">>, Args, []),

    %% 生成请求 ID
    RequestId = generate_request_id(),

    %% 构建请求
    Request = #{
        type => ask_human,
        request_id => RequestId,
        question => Question,
        context => QuestionContext,
        options => Options,
        timestamp => erlang:system_time(millisecond)
    },

    %% 检查是否有交互处理器
    case get_interaction_handler(Context) of
        undefined ->
            %% 没有处理器，返回待处理状态
            {ok, #{
                action => ask_human,
                pending => true,
                request_id => RequestId,
                question => Question,
                message => <<"等待用户输入"/utf8>>
            }};
        Handler when is_function(Handler, 1) ->
            %% 有处理器，调用它
            case Handler(Request) of
                {ok, Response} ->
                    {ok, #{
                        action => ask_human,
                        success => true,
                        request_id => RequestId,
                        response => Response
                    }};
                {error, Reason} ->
                    {error, {human_interaction_error, Reason}}
            end
    end.

%% @doc 请求用户确认
handle_confirm_action(Args) ->
    handle_confirm_action(Args, #{}).

handle_confirm_action(Args, Context) ->
    Action = maps:get(<<"action">>, Args),
    Reason = maps:get(<<"reason">>, Args),
    Consequences = maps:get(<<"consequences">>, Args, undefined),

    %% 生成请求 ID
    RequestId = generate_request_id(),

    %% 构建确认请求
    Request = #{
        type => confirm_action,
        request_id => RequestId,
        action => Action,
        reason => Reason,
        consequences => Consequences,
        timestamp => erlang:system_time(millisecond)
    },

    %% 检查是否有交互处理器
    case get_interaction_handler(Context) of
        undefined ->
            %% 没有处理器，返回待处理状态
            {ok, #{
                action => confirm_action,
                pending => true,
                request_id => RequestId,
                action_description => Action,
                reason => Reason,
                message => <<"等待用户确认"/utf8>>
            }};
        Handler when is_function(Handler, 1) ->
            %% 有处理器，调用它
            case Handler(Request) of
                {ok, confirmed} ->
                    {ok, #{
                        action => confirm_action,
                        success => true,
                        confirmed => true,
                        request_id => RequestId
                    }};
                {ok, denied} ->
                    {ok, #{
                        action => confirm_action,
                        success => false,
                        confirmed => false,
                        denied => true,
                        request_id => RequestId
                    }};
                {error, Reason} ->
                    {error, {human_interaction_error, Reason}}
            end
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 生成请求 ID
generate_request_id() ->
    Rand = rand:uniform(16#FFFFFFFF),
    Timestamp = erlang:system_time(millisecond),
    iolist_to_binary(io_lib:format("req_~p_~8.16.0b", [Timestamp, Rand])).

%% @private 获取交互处理器
%%
%% 从 context 中获取用户定义的交互处理器。
get_interaction_handler(Context) ->
    maps:get(human_interaction_handler, Context, undefined).
