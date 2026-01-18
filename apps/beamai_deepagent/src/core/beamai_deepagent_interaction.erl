%%%-------------------------------------------------------------------
%%% @doc Human 交互模式管理
%%%
%%% 处理 Agent 与用户的交互：
%%% - 同步模式 (sync): 阻塞等待用户输入，适合 CLI
%%% - 异步模式 (async): 返回 pending 状态，适合 API/Web
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_interaction).

%% API
-export([request_human_input/2]).
-export([request_approval/3]).
-export([default_cli_callback/0]).

%% 类型导出
-export_type([interaction_mode/0, interaction_callback/0]).
-export_type([interaction_request/0, interaction_response/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type interaction_mode() :: sync | async.

-type interaction_callback() :: fun((interaction_request()) -> interaction_response()).

-type interaction_request() :: #{
    type := question | confirmation | approval,
    atom() => term()
}.

-type interaction_response() :: #{
    response => term(),
    confirmed => boolean(),
    approved => boolean()
}.

%%====================================================================
%% API
%%====================================================================

%% @doc 请求人工输入
%%
%% 根据交互模式处理请求：
%% - sync: 调用回调并阻塞等待
%% - async: 返回 pending 状态
-spec request_human_input(interaction_request(), map()) ->
    {ok, interaction_response()} | {pending, binary()} | {error, term()}.
request_human_input(Request, Config) ->
    Mode = maps:get(mode, Config, sync),
    Timeout = maps:get(timeout, Config, 300000),

    case Mode of
        sync ->
            handle_sync_request(Request, Config, Timeout);
        async ->
            handle_async_request(Request, Config)
    end.

%% @doc 请求工具审批
%%
%% 专门用于工具执行前的审批请求
-spec request_approval(binary(), map(), map()) ->
    approved | {denied, binary()} | {pending, binary()}.
request_approval(ToolName, ToolArgs, Config) ->
    Request = #{
        type => approval,
        tool => ToolName,
        args => ToolArgs,
        message => <<"请确认是否执行此工具"/utf8>>
    },

    case request_human_input(Request, Config) of
        {ok, #{approved := true}} ->
            approved;
        {ok, #{approved := false}} ->
            {denied, <<"用户拒绝了此操作"/utf8>>};
        {pending, RequestId} ->
            {pending, RequestId};
        {error, Reason} ->
            {denied, format_error(Reason)}
    end.

%% @doc 默认 CLI 回调
%%
%% 在终端中显示提示并等待用户输入
-spec default_cli_callback() -> interaction_callback().
default_cli_callback() ->
    fun(Request) ->
        Type = maps:get(type, Request),
        case Type of
            question -> handle_cli_question(Request);
            confirmation -> handle_cli_confirmation(Request);
            approval -> handle_cli_approval(Request)
        end
    end.

%%====================================================================
%% 同步模式处理
%%====================================================================

%% @private 处理同步请求
handle_sync_request(Request, Config, Timeout) ->
    Callback = maps:get(callback, Config, default_cli_callback()),

    %% 使用超时包装回调调用
    Parent = self(),
    Ref = make_ref(),

    Pid = spawn(fun() ->
        try
            Result = Callback(Request),
            Parent ! {Ref, {ok, Result}}
        catch
            Class:Reason ->
                Parent ! {Ref, {error, {Class, Reason}}}
        end
    end),

    receive
        {Ref, Result} ->
            Result
    after Timeout ->
        exit(Pid, kill),
        {error, timeout}
    end.

%%====================================================================
%% 异步模式处理
%%====================================================================

%% @private 处理异步请求
handle_async_request(Request, _Config) ->
    %% 生成请求 ID
    RequestId = generate_request_id(),

    %% 在异步模式下，我们只返回 pending 状态
    %% 实际的响应处理由外部系统（如 HTTP API）完成
    %% 这里可以将请求存储到 ETS 或其他存储中

    %% 简单实现：直接返回 pending
    %% 完整实现需要：
    %% 1. 将请求存储到持久化存储
    %% 2. 提供 resume/2 API 用于恢复执行
    store_pending_request(RequestId, Request),
    {pending, RequestId}.

%% @private 存储待处理请求
store_pending_request(RequestId, Request) ->
    %% 使用进程字典临时存储（生产环境应使用 ETS 或数据库）
    PendingRequests = get(pending_human_requests) orelse #{},
    put(pending_human_requests, maps:put(RequestId, Request, PendingRequests)),
    ok.

%% @private 生成请求 ID
generate_request_id() ->
    Timestamp = erlang:system_time(millisecond),
    Random = rand:uniform(999999),
    iolist_to_binary(io_lib:format("hil_~p_~p", [Timestamp, Random])).

%%====================================================================
%% CLI 处理函数
%%====================================================================

%% @private 处理 CLI 问题
handle_cli_question(Request) ->
    %% 使用公共 UI 模块显示界面
    beamai_deepagent_cli_ui:display_question(Request),
    io:format("请输入回答: "),
    case beamai_deepagent_cli_ui:read_input() of
        {ok, Answer} -> #{response => Answer};
        {error, _} -> #{response => <<>>}
    end.

%% @private 处理 CLI 确认
handle_cli_confirmation(Request) ->
    %% 使用公共 UI 模块显示界面
    beamai_deepagent_cli_ui:display_confirmation(Request),
    case beamai_deepagent_cli_ui:read_yes_no() of
        {ok, Confirmed} -> #{confirmed => Confirmed};
        {error, _} -> #{confirmed => false}
    end.

%% @private 处理 CLI 审批
handle_cli_approval(Request) ->
    %% 使用公共 UI 模块显示界面
    beamai_deepagent_cli_ui:display_approval(Request),
    case beamai_deepagent_cli_ui:read_approval() of
        {ok, Approved} -> #{approved => Approved};
        {error, _} -> #{approved => false}
    end.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 格式化错误信息
-spec format_error(term()) -> binary().
format_error(timeout) ->
    <<"请求超时"/utf8>>;
format_error({Class, Reason}) ->
    iolist_to_binary(io_lib:format("~p:~p", [Class, Reason]));
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).
