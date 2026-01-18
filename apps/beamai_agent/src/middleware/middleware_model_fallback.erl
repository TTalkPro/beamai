%%%-------------------------------------------------------------------
%%% @doc Model Fallback Middleware - 模型故障转移
%%%
%%% 当主模型调用失败时，自动切换到备用模型。
%%%
%%% == 功能特性 ==
%%%
%%% - 支持多个备用模型按顺序尝试
%%% - 可配置触发故障转移的错误类型
%%% - 支持不同 provider 之间的切换
%%% - 支持故障转移回调通知
%%% - 成功后可选择是否恢复主模型
%%%
%%% == 配置示例 ==
%%%
%%% ```erlang
%%% {middleware_model_fallback, #{
%%%     %% 备用模型列表（按优先级顺序）
%%%     fallback_models => [
%%%         #{provider => openai, model => <<"gpt-4">>, api_key => <<"...">>},
%%%         #{provider => anthropic, model => <<"claude-3-sonnet">>, api_key => <<"...">>}
%%%     ],
%%%
%%%     %% 触发故障转移的错误类型
%%%     trigger_errors => [timeout, rate_limit, server_error],
%%%
%%%     %% 故障转移回调
%%%     on_fallback => fun(FromModel, ToModel, Error) -> ok,
%%%
%%%     %% 成功后恢复主模型的等待时间（毫秒，0 表示不恢复）
%%%     recovery_delay => 60000,
%%%
%%%     %% 调试模式
%%%     debug => false
%%% }}
%%% ```
%%%
%%% == 工作流程 ==
%%%
%%% 1. before_model: 检查是否需要使用备用模型
%%% 2. after_model: 检测错误，触发故障转移
%%% 3. 保存原始 LLM 配置，切换到备用模型
%%% 4. 返回 {goto, model} 重新调用
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_model_fallback).

-behaviour(beamai_middleware).

%% Middleware 回调
-export([init/1, before_model/2, after_model/2]).

%% 工具函数
-export([
    get_current_model/1,
    get_fallback_status/1,
    reset_to_primary/1
]).

%%====================================================================
%% Middleware 回调
%%====================================================================

%% @doc 初始化 Middleware 状态
%%
%% 配置选项：
%%   - fallback_models: 备用模型列表
%%   - trigger_errors: 触发故障转移的错误类型
%%   - on_fallback: 故障转移回调
%%   - recovery_delay: 恢复主模型的延迟（0 表示不恢复）
%%   - debug: 调试模式
-spec init(map()) -> map().
init(Opts) ->
    #{
        fallback_models => maps:get(fallback_models, Opts, []),
        trigger_errors => maps:get(trigger_errors, Opts, default_trigger_errors()),
        on_fallback => maps:get(on_fallback, Opts, undefined),
        recovery_delay => maps:get(recovery_delay, Opts, 0),
        debug => maps:get(debug, Opts, false)
    }.

%% @doc 模型调用前检查是否需要使用备用模型
%%
%% 如果已经处于故障转移状态，使用当前备用模型。
-spec before_model(map(), map()) -> beamai_middleware:middleware_result().
before_model(State, MwState) ->
    case graph:get(State, mw_fallback_active, false) of
        false ->
            %% 正常状态，保存原始 LLM 配置
            case graph:get(State, mw_original_llm_config, undefined) of
                undefined ->
                    LLMConfig = graph:get(State, llm_config, #{}),
                    {update, #{mw_original_llm_config => LLMConfig}};
                _ ->
                    ok
            end;
        true ->
            %% 已处于故障转移状态，检查是否需要恢复
            maybe_check_recovery(State, MwState)
    end.

%% @doc 模型调用后检查是否需要故障转移
-spec after_model(map(), map()) -> beamai_middleware:middleware_result().
after_model(State, MwState) ->
    case graph:get(State, llm_error, undefined) of
        undefined ->
            %% 成功，如果处于故障转移状态，记录成功时间
            case graph:get(State, mw_fallback_active, false) of
                true ->
                    {update, #{mw_fallback_success_time => erlang:system_time(millisecond)}};
                false ->
                    ok
            end;
        Error ->
            handle_error(Error, State, MwState)
    end.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 获取当前使用的模型信息
-spec get_current_model(map()) -> map().
get_current_model(State) ->
    case graph:get(State, mw_fallback_active, false) of
        true ->
            #{
                is_fallback => true,
                fallback_index => graph:get(State, mw_fallback_index, 0),
                model => graph:get(State, llm_config, #{})
            };
        false ->
            #{
                is_fallback => false,
                model => graph:get(State, llm_config, #{})
            }
    end.

%% @doc 获取故障转移状态
-spec get_fallback_status(map()) -> map().
get_fallback_status(State) ->
    #{
        active => graph:get(State, mw_fallback_active, false),
        fallback_index => graph:get(State, mw_fallback_index, 0),
        fallback_count => graph:get(State, mw_fallback_count, 0),
        last_error => graph:get(State, mw_last_fallback_error, undefined),
        success_time => graph:get(State, mw_fallback_success_time, undefined)
    }.

%% @doc 重置到主模型
-spec reset_to_primary(map()) -> map().
reset_to_primary(State) ->
    OriginalConfig = graph:get(State, mw_original_llm_config, #{}),
    graph:set(State, #{
        llm_config => OriginalConfig,
        mw_fallback_active => false,
        mw_fallback_index => 0
    }).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 默认触发故障转移的错误类型
-spec default_trigger_errors() -> [atom()].
default_trigger_errors() ->
    [
        timeout,
        connection_error,
        connection_closed,
        rate_limit,
        rate_limited,
        server_error,
        service_unavailable,
        internal_server_error,
        bad_gateway,
        gateway_timeout,
        model_not_found,
        invalid_api_key,
        quota_exceeded
    ].

%% @private 处理错误，尝试故障转移
-spec handle_error(term(), map(), map()) -> beamai_middleware:middleware_result().
handle_error(Error, State, MwState) ->
    #{fallback_models := FallbackModels,
      trigger_errors := TriggerErrors,
      on_fallback := OnFallback,
      debug := Debug} = MwState,

    %% 检查是否应该触发故障转移
    ShouldFallback = should_trigger_fallback(Error, TriggerErrors),

    case ShouldFallback of
        false ->
            %% 不触发故障转移
            ok;
        true ->
            %% 获取当前故障转移索引
            CurrentIndex = graph:get(State, mw_fallback_index, 0),
            NextIndex = CurrentIndex + 1,

            case NextIndex =< length(FallbackModels) of
                true ->
                    %% 切换到下一个备用模型
                    NextModel = lists:nth(NextIndex, FallbackModels),
                    CurrentConfig = graph:get(State, llm_config, #{}),

                    %% 调用故障转移回调
                    maybe_call_on_fallback(OnFallback, CurrentConfig, NextModel, Error),

                    %% 调试日志
                    case Debug of
                        true ->
                            logger:info("[ModelFallback] 切换到备用模型 #~p: ~p，错误: ~p",
                                       [NextIndex, maps:get(model, NextModel, unknown), Error]);
                        false ->
                            ok
                    end,

                    %% 更新状态并重试
                    {update_goto, #{
                        llm_config => NextModel,
                        llm_error => undefined,
                        mw_fallback_active => true,
                        mw_fallback_index => NextIndex,
                        mw_fallback_count => graph:get(State, mw_fallback_count, 0) + 1,
                        mw_last_fallback_error => Error
                    }, model};
                false ->
                    %% 所有备用模型都已尝试
                    case Debug of
                        true ->
                            logger:warning("[ModelFallback] 所有备用模型都已尝试，放弃");
                        false ->
                            ok
                    end,
                    ok
            end
    end.

%% @private 检查是否应该触发故障转移
-spec should_trigger_fallback(term(), [atom()]) -> boolean().
should_trigger_fallback({ErrorType, _}, TriggerErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, TriggerErrors);
should_trigger_fallback(ErrorType, TriggerErrors) when is_atom(ErrorType) ->
    lists:member(ErrorType, TriggerErrors);
should_trigger_fallback(ErrorBin, _TriggerErrors) when is_binary(ErrorBin) ->
    %% 检查常见的错误消息
    LowerError = string:lowercase(binary_to_list(ErrorBin)),
    lists:any(fun(Pattern) ->
        string:find(LowerError, Pattern) =/= nomatch
    end, ["timeout", "rate limit", "connection", "server error", "503", "502", "504",
          "quota", "invalid key", "model not found"]);
should_trigger_fallback(_, _) ->
    false.

%% @private 检查是否需要恢复主模型
-spec maybe_check_recovery(map(), map()) -> beamai_middleware:middleware_result().
maybe_check_recovery(State, MwState) ->
    #{recovery_delay := RecoveryDelay, debug := Debug} = MwState,

    case RecoveryDelay > 0 of
        false ->
            ok;
        true ->
            SuccessTime = graph:get(State, mw_fallback_success_time, 0),
            Now = erlang:system_time(millisecond),

            case SuccessTime > 0 andalso (Now - SuccessTime) >= RecoveryDelay of
                true ->
                    %% 恢复主模型
                    OriginalConfig = graph:get(State, mw_original_llm_config, #{}),
                    case Debug of
                        true ->
                            logger:info("[ModelFallback] 恢复主模型: ~p",
                                       [maps:get(model, OriginalConfig, unknown)]);
                        false ->
                            ok
                    end,
                    {update, #{
                        llm_config => OriginalConfig,
                        mw_fallback_active => false,
                        mw_fallback_index => 0,
                        mw_fallback_success_time => 0
                    }};
                false ->
                    ok
            end
    end.

%% @private 调用故障转移回调
-spec maybe_call_on_fallback(function() | undefined, map(), map(), term()) -> ok.
maybe_call_on_fallback(undefined, _, _, _) ->
    ok;
maybe_call_on_fallback(OnFallback, FromModel, ToModel, Error) when is_function(OnFallback, 3) ->
    try
        OnFallback(FromModel, ToModel, Error)
    catch
        _:Reason ->
            logger:warning("[ModelFallback] 故障转移回调异常: ~p", [Reason])
    end,
    ok.
