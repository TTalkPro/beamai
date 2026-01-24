%%%-------------------------------------------------------------------
%%% @doc 人工审批中间件（4-hook 版本）
%%%
%%% 在工具执行前暂停并等待人工确认：
%%% - 支持 all（全部需审批）/ selective（选择性审批）/ custom（自定义）/ none（无需审批）模式
%%% - 可配置审批超时时间和超时后的默认动作
%%% - 支持同步审批处理器
%%%
%%% 使用的钩子：
%%% - pre_invocation: 在函数调用前检查是否需要人工审批
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_human_approval).

-behaviour(beamai_middleware).

-export([init/1, pre_invocation/2]).
-export([requires_approval/3]).

%%====================================================================
%% 类型定义
%%====================================================================

%% 审批模式类型：
%% - all: 所有函数调用都需要审批
%% - selective: 仅指定列表中的函数需要审批
%% - custom: 通过自定义函数判断是否需要审批
%% - none: 不需要任何审批
-type approval_mode() :: all | selective | custom | none.
-export_type([approval_mode/0]).

%%====================================================================
%% 中间件回调函数
%%====================================================================

%% @doc 初始化审批模式和处理器
%%
%% 从配置选项中提取审批相关参数，构建中间件初始状态。
%%
%% @param Opts 配置选项映射，支持以下字段：
%%   - mode: 审批模式（all | selective | custom | none）
%%   - tools_requiring_approval: 需要审批的工具名称列表（selective 模式下使用）
%%   - approval_fn: 自定义审批判断函数（custom 模式下使用）
%%   - approval_handler: 同步审批处理器函数
%%   - timeout: 审批超时时间（毫秒，默认60000）
%%   - timeout_action: 超时后的默认动作（reject | confirm）
%% @returns 中间件状态映射
-spec init(map()) -> map().
init(Opts) ->
    #{
        mode => maps:get(mode, Opts, none),
        tools_requiring_approval => maps:get(tools_requiring_approval, Opts, []),
        approval_fn => maps:get(approval_fn, Opts, undefined),
        approval_handler => maps:get(approval_handler, Opts, undefined),
        timeout => maps:get(timeout, Opts, 60000),
        timeout_action => maps:get(timeout_action, Opts, reject)
    }.

%% @doc 检查是否需要人工审批
%%
%% 在函数调用执行前检查该函数是否需要人工审批。
%% 如果审批模式为 none，直接放行；否则根据模式判断是否需要审批。
%%
%% @param FilterCtx 过滤上下文，包含被调用函数的定义和参数
%% @param MwState 中间件状态，包含审批模式和配置
%% @returns ok（放行）| {skip, 待审批信息} | {error, 审批拒绝原因}
-spec pre_invocation(map(), map()) -> beamai_middleware:middleware_result().
pre_invocation(_FilterCtx, #{mode := none}) ->
    ok;
pre_invocation(FilterCtx, MwState) ->
    FuncDef = maps:get(function, FilterCtx, #{}),
    FuncName = maps:get(name, FuncDef, <<"unknown">>),
    Args = maps:get(args, FilterCtx, #{}),

    case requires_approval([FuncName], FilterCtx, MwState) of
        false -> ok;
        true -> handle_approval_required(FuncName, Args, FilterCtx, MwState)
    end.

%%====================================================================
%% 公共辅助函数
%%====================================================================

%% @doc 判断指定函数是否需要审批
%%
%% 根据当前审批模式判断给定的函数名列表中是否有需要审批的函数。
%% 支持四种模式的判断逻辑：
%% - none: 始终不需要审批
%% - all: 始终需要审批
%% - selective: 检查函数名是否在审批列表中
%% - custom: 调用自定义判断函数
%%
%% @param Names 函数名列表（二进制字符串）
%% @param Ctx 调用上下文
%% @param MwState 中间件状态，包含审批模式和配置
%% @returns true（需要审批）| false（无需审批）
-spec requires_approval([binary()], map(), map()) -> boolean().
requires_approval(_Names, _Ctx, #{mode := none}) -> false;
requires_approval(_Names, _Ctx, #{mode := all}) -> true;
requires_approval(Names, _Ctx, #{mode := selective,
                                  tools_requiring_approval := RequiredTools}) ->
    lists:any(fun(Name) -> lists:member(Name, RequiredTools) end, Names);
requires_approval(Names, Ctx, #{mode := custom, approval_fn := ApprovalFn})
  when is_function(ApprovalFn, 2) ->
    try ApprovalFn(Names, Ctx) catch _:_ -> false end;
requires_approval(_, _, _) -> false.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 处理需要审批的函数调用
%% 构建审批请求，根据是否配置了处理器选择异步挂起或同步等待审批结果
handle_approval_required(FuncName, Args, FilterCtx, MwState) ->
    #{approval_handler := Handler,
      timeout := Timeout,
      timeout_action := _TimeoutAction} = MwState,

    ApprovalRequest = #{
        type => tool_approval,
        function => FuncName,
        args => Args,
        timestamp => erlang:system_time(millisecond)
    },

    case Handler of
        undefined ->
            %% 未配置处理器 - 返回 skip 并附带待审批信息
            {skip, #{pending => approval,
                     request => ApprovalRequest,
                     timeout => Timeout}};
        Handler when is_function(Handler, 2) ->
            handle_sync_approval(Handler, ApprovalRequest, FilterCtx, MwState)
    end.

%% @doc 同步处理审批请求
%% 调用审批处理器等待用户响应，支持超时机制
%% 处理器异常时使用超时动作作为默认结果
handle_sync_approval(Handler, Request, _FilterCtx, MwState) ->
    #{timeout := Timeout, timeout_action := TimeoutAction} = MwState,

    Result = try
        case Timeout of
            0 -> Handler(Request, #{});
            _ -> call_with_timeout(Handler, Request, Timeout, TimeoutAction)
        end
    catch
        _:Reason ->
            logger:warning("审批处理器执行异常: ~p, 使用超时动作: ~p",
                          [Reason, TimeoutAction]),
            TimeoutAction
    end,

    process_result(Result).

%% @doc 带超时的审批处理器调用
%% 在独立进程中执行处理器，超时后强制终止并返回超时动作
call_with_timeout(Handler, Request, Timeout, TimeoutAction) ->
    Parent = self(),
    Ref = make_ref(),
    Pid = spawn(fun() -> Parent ! {Ref, Handler(Request, #{})} end),
    receive
        {Ref, Result} -> Result
    after Timeout ->
        exit(Pid, kill),
        TimeoutAction
    end.

%% @doc 处理审批结果
%% 将审批响应转换为中间件返回值：
%% - confirm: 放行，继续执行
%% - reject: 拒绝，返回错误
%% - {modify, NewArgs}: 修改参数后继续（当前仅放行）
process_result(confirm) -> ok;
process_result(reject) ->
    {error, {approval_rejected, rejected_by_user}};
process_result({modify, NewArgs}) when is_map(NewArgs) ->
    %% 可以在过滤上下文中更新参数，当前仅放行继续执行
    ok;
process_result(_) -> ok.
