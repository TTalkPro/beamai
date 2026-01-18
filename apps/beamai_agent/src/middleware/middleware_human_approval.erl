%%%-------------------------------------------------------------------
%%% @doc Human-in-the-Loop Middleware - 人工审批
%%%
%%% 在工具执行前暂停，等待人工确认：
%%% - 支持工具调用审批
%%% - 支持自定义审批条件
%%% - 支持超时处理
%%% - 支持动作修改
%%%
%%% == 配置选项 ==
%%%
%%% ```erlang
%%% {middleware_human_approval, #{
%%%     %% 审批模式
%%%     mode => all | selective | custom,
%%%
%%%     %% selective 模式下需要审批的工具列表
%%%     tools_requiring_approval => [<<"dangerous_tool">>, <<"delete_file">>],
%%%
%%%     %% custom 模式下的判断函数
%%%     approval_fn => fun(ToolCalls, State) -> boolean(),
%%%
%%%     %% 审批处理器（用于获取用户输入）
%%%     approval_handler => fun(Action, State) -> confirm | reject | {modify, NewAction},
%%%
%%%     %% 审批超时（毫秒，0 表示无限等待）
%%%     timeout => 60000,
%%%
%%%     %% 超时后的默认动作
%%%     timeout_action => reject | confirm
%%% }}
%%% ```
%%%
%%% == 审批流程 ==
%%%
%%% 1. before_tools 钩子触发
%%% 2. 检查是否需要审批（根据 mode）
%%% 3. 如果需要审批：
%%%    a. 返回 {interrupt, Action}
%%%    b. Agent 暂停执行
%%%    c. 等待用户通过 resume/2 继续
%%% 4. 用户响应：
%%%    - confirm: 继续执行工具
%%%    - reject: 跳过工具，结束执行
%%%    - {modify, NewToolCalls}: 使用修改后的工具调用
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_human_approval).

-behaviour(beamai_middleware).

%% Middleware 回调
-export([init/1, before_tools/2]).

%% 工具函数
-export([
    requires_approval/3,
    format_approval_request/2
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type approval_mode() :: all | selective | custom | none.
-type approval_action() :: confirm | reject | {modify, term()}.

%%====================================================================
%% Middleware 回调
%%====================================================================

%% @doc 初始化 Middleware 状态
%%
%% 配置选项：
%%   - mode: 审批模式（all | selective | custom | none）
%%   - tools_requiring_approval: selective 模式下需要审批的工具列表
%%   - approval_fn: custom 模式下的自定义判断函数
%%   - approval_handler: 同步审批处理器（可选）
%%   - timeout: 审批超时时间（毫秒）
%%   - timeout_action: 超时后的默认动作
%%   - track_history: 是否记录审批历史
%%   - debug: 调试模式
-spec init(map()) -> map().
init(Opts) ->
    #{
        mode => maps:get(mode, Opts, none),
        tools_requiring_approval => maps:get(tools_requiring_approval, Opts, []),
        approval_fn => maps:get(approval_fn, Opts, undefined),
        approval_handler => maps:get(approval_handler, Opts, undefined),
        timeout => maps:get(timeout, Opts, 60000),
        timeout_action => maps:get(timeout_action, Opts, reject),
        track_history => maps:get(track_history, Opts, true),
        debug => maps:get(debug, Opts, false)
    }.

%% @doc 工具执行前检查是否需要人工审批
%%
%% 流程：
%%   1. 检查审批模式
%%   2. 如果 mode=none，直接通过
%%   3. 否则检查是否需要审批
-spec before_tools(map(), map()) -> beamai_middleware:middleware_result().
before_tools(State, MwState) ->
    #{mode := Mode} = MwState,
    case Mode of
        none -> ok;
        _ -> check_and_request_approval(State, MwState)
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 检查并请求审批
-spec check_and_request_approval(map(), map()) -> beamai_middleware:middleware_result().
check_and_request_approval(State, MwState) ->
    ToolCalls = graph:get(State, tool_calls, []),
    case ToolCalls of
        [] ->
            %% 没有工具调用
            ok;
        _ ->
            case requires_approval(ToolCalls, State, MwState) of
                false -> ok;
                true -> handle_approval_required(ToolCalls, State, MwState)
            end
    end.

%% @doc 判断是否需要审批
%%
%% 根据不同模式进行判断：
%%   - none: 永不需要
%%   - all: 总是需要
%%   - selective: 检查工具是否在审批列表中
%%   - custom: 使用自定义函数判断
-spec requires_approval([map()], map(), map()) -> boolean().
requires_approval(_ToolCalls, _State, #{mode := none}) ->
    false;
requires_approval(_ToolCalls, _State, #{mode := all}) ->
    true;
requires_approval(ToolCalls, _State, #{mode := selective,
                                        tools_requiring_approval := RequiredTools}) ->
    ToolNames = extract_tool_names(ToolCalls),
    lists:any(fun(Name) -> lists:member(Name, RequiredTools) end, ToolNames);
requires_approval(ToolCalls, State, #{mode := custom, approval_fn := ApprovalFn})
  when is_function(ApprovalFn, 2) ->
    try
        ApprovalFn(ToolCalls, State)
    catch
        _:_ -> false
    end;
requires_approval(_, _, _) ->
    false.

%% @private 处理需要审批的情况
-spec handle_approval_required([map()], map(), map()) -> beamai_middleware:middleware_result().
handle_approval_required(ToolCalls, State, MwState) ->
    #{approval_handler := Handler,
      timeout := Timeout,
      timeout_action := TimeoutAction} = MwState,

    ApprovalRequest = format_approval_request(ToolCalls, State),

    case Handler of
        undefined ->
            %% 没有同步处理器，返回中断让外部处理
            {interrupt, #{
                type => tool_approval,
                tool_calls => ToolCalls,
                request => ApprovalRequest,
                timeout => Timeout,
                timeout_action => TimeoutAction
            }};
        Handler when is_function(Handler, 2) ->
            %% 有同步处理器，直接调用
            handle_sync_approval(Handler, ApprovalRequest, State, MwState)
    end.

%% @private 同步处理审批
-spec handle_sync_approval(function(), map(), map(), map()) ->
    beamai_middleware:middleware_result().
handle_sync_approval(Handler, ApprovalRequest, State, MwState) ->
    #{timeout := Timeout, timeout_action := TimeoutAction} = MwState,

    Result = try
        case Timeout of
            0 ->
                Handler(ApprovalRequest, State);
            _ ->
                call_with_timeout(Handler, ApprovalRequest, State, Timeout, TimeoutAction)
        end
    catch
        _:Reason ->
            logger:warning("审批处理器异常: ~p，使用超时动作: ~p", [Reason, TimeoutAction]),
            TimeoutAction
    end,

    process_approval_result(Result, ApprovalRequest, State, MwState).

%% @private 带超时的函数调用
-spec call_with_timeout(function(), map(), map(), pos_integer(), approval_action()) ->
    approval_action().
call_with_timeout(Handler, Request, State, Timeout, TimeoutAction) ->
    Parent = self(),
    Ref = make_ref(),

    Pid = spawn(fun() ->
        Result = Handler(Request, State),
        Parent ! {Ref, Result}
    end),

    receive
        {Ref, Result} -> Result
    after Timeout ->
        exit(Pid, kill),
        logger:info("审批超时 (~p ms)，使用默认动作: ~p", [Timeout, TimeoutAction]),
        TimeoutAction
    end.

%% @private 处理审批结果
%%
%% 根据结果决定下一步：
%%   - confirm: 继续执行
%%   - reject: 跳过工具，结束
%%   - {modify, NewToolCalls}: 使用修改后的工具调用
-spec process_approval_result(approval_action(), map(), map(), map()) ->
    beamai_middleware:middleware_result().
process_approval_result(confirm, _Request, _State, MwState) ->
    maybe_log("审批通过，继续执行工具", MwState),
    ok;
process_approval_result(reject, _Request, _State, MwState) ->
    maybe_log("审批拒绝，跳过工具执行", MwState),
    {update_goto, #{
        tool_calls => [],
        approval_rejected => true,
        approval_reason => rejected_by_user
    }, '__end__'};
process_approval_result({modify, NewToolCalls}, _Request, _State, MwState) ->
    maybe_log("审批修改，使用新的工具调用", MwState),
    {update, #{
        tool_calls => NewToolCalls,
        approval_modified => true
    }};
process_approval_result(Other, _Request, _State, MwState) ->
    logger:warning("未知的审批结果: ~p，默认继续执行", [Other]),
    maybe_log("未知审批结果，继续执行", MwState),
    ok.

%% @doc 格式化审批请求
%%
%% 构建审批请求信息，包含工具调用详情和上下文。
-spec format_approval_request([map()], map()) -> map().
format_approval_request(ToolCalls, State) ->
    #{
        tool_calls => ToolCalls,
        tool_names => extract_tool_names(ToolCalls),
        tool_count => length(ToolCalls),
        iteration => graph:get(State, iteration, 0),
        timestamp => erlang:system_time(millisecond),
        summary => format_tool_summary(ToolCalls)
    }.

%% @private 提取工具名称列表
-spec extract_tool_names([map()]) -> [binary()].
extract_tool_names(ToolCalls) ->
    %% 使用公共工具函数
    [beamai_agent_utils:extract_tool_name(TC) || TC <- ToolCalls].

%% @private 格式化工具调用摘要
-spec format_tool_summary([map()]) -> binary().
format_tool_summary(ToolCalls) ->
    Summaries = lists:map(fun(TC) ->
        Name = beamai_agent_utils:extract_tool_name(TC),
        Args = beamai_agent_utils:extract_tool_args(TC),
        ArgsPreview = case jsx:encode(Args) of
            Encoded when byte_size(Encoded) > 100 ->
                <<(binary:part(Encoded, 0, 100))/binary, "...">>;
            Encoded ->
                Encoded
        end,
        <<Name/binary, "(", ArgsPreview/binary, ")">>
    end, ToolCalls),
    iolist_to_binary(lists:join(<<", ">>, Summaries)).

%% @private 调试日志
-spec maybe_log(string(), map()) -> ok.
maybe_log(Msg, #{debug := true}) ->
    logger:info("[HumanApproval] ~s", [Msg]);
maybe_log(_, _) ->
    ok.
