%%%-------------------------------------------------------------------
%%% @doc 模型降级中间件（4-hook 版本）
%%%
%%% 当主模型调用失败时，自动切换到备选降级模型。
%%% 支持配置多个降级模型列表，按优先级依次尝试。
%%% 当所有降级模型耗尽时，返回原始错误。
%%%
%%% 使用的钩子：
%%% - post_chat: 检测 LLM 错误并触发模型降级
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_model_fallback).

-behaviour(beamai_middleware).

-export([init/1, post_chat/2]).

%%====================================================================
%% 中间件回调函数
%%====================================================================

%% @doc 初始化降级模型列表和触发条件
%%
%% 从配置选项中提取降级相关参数，构建中间件初始状态。
%%
%% @param Opts 配置选项映射，支持以下字段：
%%   - fallback_models: 降级模型列表，按优先级排序
%%   - trigger_errors: 触发降级的错误类型列表
%%   - on_fallback: 降级时触发的回调函数（可选）
%% @returns 中间件状态映射，包含降级配置和当前索引
-spec init(map()) -> map().
init(Opts) ->
    #{
        fallback_models => maps:get(fallback_models, Opts, []),
        trigger_errors => maps:get(trigger_errors, Opts, default_trigger_errors()),
        on_fallback => maps:get(on_fallback, Opts, undefined),
        fallback_index => 0,
        fallback_active => false
    }.

%% @doc 检测 LLM 错误并触发模型降级
%%
%% 在 LLM 调用完成后检查返回结果。如果检测到错误，
%% 判断是否需要触发降级，并切换到下一个可用的备选模型。
%%
%% @param FilterCtx 过滤上下文，包含 result 字段表示 LLM 调用结果
%% @param MwState 中间件状态，包含降级模型列表和当前索引
%% @returns ok | {skip, 降级信息映射}
-spec post_chat(map(), map()) -> beamai_middleware:middleware_result().
post_chat(FilterCtx, MwState) ->
    case maps:get(result, FilterCtx, undefined) of
        {error, Error} ->
            handle_error(Error, FilterCtx, MwState);
        _ ->
            ok
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 获取默认的触发降级错误类型列表
%% 包含常见的网络错误、超时、限流、服务不可用等错误类型
default_trigger_errors() ->
    [timeout, connection_error, connection_closed, rate_limit,
     rate_limited, server_error, service_unavailable,
     internal_server_error, bad_gateway, gateway_timeout,
     model_not_found, invalid_api_key, quota_exceeded].

%% @doc 处理 LLM 调用错误
%% 判断错误是否应触发降级，如果是则尝试切换到下一个备选模型
%% 当所有降级模型耗尽时返回 ok（不再尝试降级）
handle_error(Error, _FilterCtx, MwState) ->
    #{fallback_models := FallbackModels,
      trigger_errors := TriggerErrors,
      on_fallback := OnFallback,
      fallback_index := CurrentIndex} = MwState,

    case should_trigger_fallback(Error, TriggerErrors) of
        false -> ok;
        true ->
            NextIndex = CurrentIndex + 1,
            case NextIndex =< length(FallbackModels) of
                true ->
                    NextModel = lists:nth(NextIndex, FallbackModels),
                    maybe_call_on_fallback(OnFallback, #{}, NextModel, Error),
                    %% 通过 skip 信号通知框架执行模型降级
                    {skip, #{pending => model_fallback,
                             fallback_model => NextModel,
                             fallback_index => NextIndex,
                             error => Error}};
                false ->
                    %% 所有降级模型已耗尽，无法继续降级
                    ok
            end
    end.

%% @doc 判断错误是否应触发降级
%% 支持三种错误格式：
%% - {ErrorType, _} 元组形式：提取错误类型进行匹配
%% - 原子形式：直接与触发列表比较
%% - 二进制字符串形式：通过关键词模糊匹配
should_trigger_fallback({ErrorType, _}, TriggerErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, TriggerErrors);
should_trigger_fallback(ErrorType, TriggerErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, TriggerErrors);
should_trigger_fallback(ErrorBin, _) when is_binary(ErrorBin) ->
    LowerError = string:lowercase(binary_to_list(ErrorBin)),
    lists:any(fun(Pattern) ->
        string:find(LowerError, Pattern) =/= nomatch
    end, ["timeout", "rate limit", "connection", "server error",
          "503", "502", "504", "quota", "invalid key", "model not found"]);
should_trigger_fallback(_, _) -> false.

%% @doc 调用降级回调函数（如果已配置）
%% 当发生模型降级时通知调用方，便于记录日志或执行自定义逻辑
%% 回调函数异常不会影响降级流程
maybe_call_on_fallback(undefined, _, _, _) -> ok;
maybe_call_on_fallback(OnFallback, FromModel, ToModel, Error) when is_function(OnFallback, 3) ->
    try OnFallback(FromModel, ToModel, Error)
    catch _:R -> logger:warning("降级回调函数执行异常: ~p", [R])
    end,
    ok.
