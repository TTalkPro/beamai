%%%-------------------------------------------------------------------
%%% @doc Human-in-the-Loop 工具处理器
%%%
%%% 实现 Human 交互工具的具体逻辑。
%%% 通过 beamai_deepagent_interaction 模块处理实际的用户交互。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_human_handlers).

%% API
-export([handle_ask_human/2, handle_confirm_action/2]).

%%====================================================================
%% 处理器实现
%%====================================================================

%% @doc 处理 ask_human 请求
%%
%% 向用户提问并等待回答。根据交互模式可能同步阻塞或异步返回。
-spec handle_ask_human(map(), map()) -> map().
handle_ask_human(Args, State) ->
    Question = maps:get(<<"question">>, Args),
    Context = maps:get(<<"context">>, Args, <<>>),
    Options = maps:get(<<"options">>, Args, []),

    %% 从状态获取配置
    Config = get_human_config(State),

    %% 构建交互请求
    Request = #{
        type => question,
        question => Question,
        context => Context,
        options => Options
    },

    %% 请求人工输入
    case beamai_deepagent_interaction:request_human_input(Request, Config) of
        {ok, Response} ->
            #{
                action => ask_human,
                success => true,
                question => Question,
                response => maps:get(response, Response, <<>>),
                message => <<"用户已回答问题"/utf8>>
            };
        {pending, RequestId} ->
            #{
                action => ask_human,
                success => true,
                pending => true,
                request_id => RequestId,
                question => Question,
                message => <<"等待用户回答"/utf8>>
            };
        {error, Reason} ->
            #{
                action => ask_human,
                success => false,
                question => Question,
                error => format_error(Reason)
            }
    end.

%% @doc 处理 confirm_action 请求
%%
%% 请求用户确认操作。用户可以批准或拒绝。
-spec handle_confirm_action(map(), map()) -> map().
handle_confirm_action(Args, State) ->
    Action = maps:get(<<"action">>, Args),
    Reason = maps:get(<<"reason">>, Args),
    Consequences = maps:get(<<"consequences">>, Args, <<>>),

    %% 从状态获取配置
    Config = get_human_config(State),

    %% 构建确认请求
    Request = #{
        type => confirmation,
        action => Action,
        reason => Reason,
        consequences => Consequences
    },

    %% 请求人工确认
    case beamai_deepagent_interaction:request_human_input(Request, Config) of
        {ok, #{confirmed := true}} ->
            #{
                action => confirm_action,
                success => true,
                confirmed => true,
                action_description => Action,
                message => <<"用户已确认操作"/utf8>>
            };
        {ok, #{confirmed := false}} ->
            #{
                action => confirm_action,
                success => true,
                confirmed => false,
                action_description => Action,
                message => <<"用户已拒绝操作"/utf8>>
            };
        {pending, RequestId} ->
            #{
                action => confirm_action,
                success => true,
                pending => true,
                request_id => RequestId,
                action_description => Action,
                message => <<"等待用户确认"/utf8>>
            };
        {error, Reason} ->
            #{
                action => confirm_action,
                success => false,
                action_description => Action,
                error => format_error(Reason)
            }
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 从状态获取 Human-in-loop 配置
get_human_config(State) ->
    Config = graph_state:get(State, config, #{}),
    maps:get(human_in_loop, Config, #{}).

%% @private 格式化错误信息
format_error(timeout) ->
    <<"请求超时，用户未在规定时间内响应"/utf8>>;
format_error(cancelled) ->
    <<"请求被取消"/utf8>>;
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason);
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).
