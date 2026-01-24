%%%-------------------------------------------------------------------
%%% @doc 人机交互插件
%%%
%%% 提供人机交互（human-in-the-loop）工具：
%%% - ask_human: 向用户提问，获取更多信息或澄清
%%% - confirm_action: 请求用户确认重要操作
%%%
%%% 支持同步交互处理器和异步挂起模式。
%%% 当未配置交互处理器时，返回待处理状态等待外部确认。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_plugin_human).

-behaviour(beamai_plugin_behaviour).

-export([plugin_info/0, functions/0]).
-export([handle_ask_human/2, handle_confirm_action/2]).

%%====================================================================
%% 插件行为回调
%%====================================================================

%% @doc 返回插件元数据
%%
%% 提供插件的名称和描述信息，用于插件注册和发现。
%%
%% @returns 插件信息映射，包含 name 和 description 字段
plugin_info() ->
    #{name => <<"human">>,
      description => <<"Human interaction tools">>}.

%% @doc 返回函数定义列表
%%
%% 返回本插件提供的所有可调用函数定义，
%% 包含提问和确认两个人机交互函数。
%%
%% @returns 函数定义列表
functions() ->
    [ask_human_func(), confirm_action_func()].

%%====================================================================
%% 函数定义
%%====================================================================

%% @doc 构建"向用户提问"函数定义
%% 标记为需要人工输入（requires_human_input）
ask_human_func() ->
    beamai_function:new(<<"ask_human">>, fun ?MODULE:handle_ask_human/2, #{
        description => <<"Ask user a question for more information or clarification.">>,
        parameters => #{
            <<"question">> => #{type => string, description => <<"Question to ask">>, required => true},
            <<"context">> => #{type => string, description => <<"Context for the question">>},
            <<"options">> => #{type => array, description => <<"Preset answer options">>,
                              items => #{type => string}}
        },
        metadata => #{requires_human_input => true}
    }).

%% @doc 构建"请求用户确认"函数定义
%% 标记为需要人工输入且为阻塞操作（blocking）
confirm_action_func() ->
    beamai_function:new(<<"confirm_action">>, fun ?MODULE:handle_confirm_action/2, #{
        description => <<"Request user confirmation before important operations.">>,
        parameters => #{
            <<"action">> => #{type => string, description => <<"Action description">>, required => true},
            <<"reason">> => #{type => string, description => <<"Why this action is needed">>, required => true},
            <<"consequences">> => #{type => string, description => <<"Potential consequences">>}
        },
        metadata => #{requires_human_input => true, blocking => true}
    }).

%%====================================================================
%% 处理器函数
%%====================================================================

%% @doc 向用户提问
%%
%% 构建提问请求并通过交互处理器发送给用户。
%% 如果未配置交互处理器，返回待处理状态（pending），
%% 等待外部系统获取用户输入后继续。
%%
%% @param Args 参数映射，包含：
%%   - question: 要问的问题（必填）
%%   - context: 问题的背景说明（可选）
%%   - options: 预设的选项列表（可选）
%% @param Context 调用上下文，可包含 human_interaction_handler
%% @returns {ok, 结果映射} | {error, 错误原因}
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

%% @doc 请求用户确认操作
%%
%% 在执行重要操作前，向用户展示操作描述、原因和可能的后果，
%% 等待用户确认或拒绝。
%% 如果未配置交互处理器，返回待处理状态。
%%
%% @param Args 参数映射，包含：
%%   - action: 操作描述（必填）
%%   - reason: 为什么需要执行此操作（必填）
%%   - consequences: 可能产生的后果（可选）
%% @param Context 调用上下文，可包含 human_interaction_handler
%% @returns {ok, 结果映射} | {error, 错误原因}
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

%% @doc 生成唯一的请求 ID
%% 格式为 "req_" 加时间戳和 8 位十六进制随机数，确保全局唯一性
generate_request_id() ->
    Rand = rand:uniform(16#FFFFFFFF),
    Timestamp = erlang:system_time(millisecond),
    iolist_to_binary(io_lib:format("req_~p_~8.16.0b", [Timestamp, Rand])).

%% @doc 从上下文中获取人机交互处理器
%% 处理器是一个接受请求映射参数的单参函数
%% 如果未配置，返回 undefined
get_interaction_handler(Context) ->
    maps:get(human_interaction_handler, Context, undefined).
