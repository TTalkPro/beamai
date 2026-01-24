%%%-------------------------------------------------------------------
%%% @doc 中间件预设配置
%%%
%%% 提供常用的中间件组合方案，适用于不同使用场景：
%%% - default/0,1: 默认预设（调用限制 + 模型重试）
%%% - minimal/0,1: 最小预设（仅调用限制）
%%% - production/0,1: 生产环境预设（严格限制 + 重试 + 降级）
%%% - development/0,1: 开发环境预设（宽松限制）
%%% - human_in_loop/0,1: 人机协作预设（含人工审批）
%%%
%%% 每个预设函数都支持无参和带参两种调用方式，
%%% 带参版本允许覆盖预设中的默认配置。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_middleware_presets).

-export([
    default/0, default/1,
    minimal/0, minimal/1,
    production/0, production/1,
    development/0, development/1,
    human_in_loop/0, human_in_loop/1
]).

-export([
    call_limit/0, call_limit/1,
    human_approval/0, human_approval/1,
    tool_retry/0, tool_retry/1,
    model_retry/0, model_retry/1,
    model_fallback/0, model_fallback/1
]).

%%====================================================================
%% 预设配置组合
%%====================================================================

%% @doc 默认预设（无参版本）。
%% 包含调用限制中间件和模型重试中间件，适用于大多数通用场景。
%% @returns 中间件规格列表
-spec default() -> [term()].
default() -> default(#{}).

%% @doc 默认预设（带参版本）。
%% 包含调用限制 + 模型重试，可通过 Opts 覆盖各中间件的默认参数。
%% @param Opts 配置选项映射，支持 call_limit 和 model_retry 键
%% @returns 中间件规格列表
-spec default(map()) -> [term()].
default(Opts) ->
    [
        call_limit(maps:get(call_limit, Opts, #{})),
        model_retry(maps:get(model_retry, Opts, #{}))
    ].

%% @doc 最小预设（无参版本）。
%% 仅包含调用限制中间件，适用于简单场景或测试环境。
%% @returns 中间件规格列表
-spec minimal() -> [term()].
minimal() -> minimal(#{}).

%% @doc 最小预设（带参版本）。
%% 仅包含调用限制中间件，可通过 Opts 覆盖默认参数。
%% @param Opts 配置选项映射，支持 call_limit 键
%% @returns 中间件规格列表
-spec minimal(map()) -> [term()].
minimal(Opts) ->
    [call_limit(maps:get(call_limit, Opts, #{}))].

%% @doc 生产环境预设（无参版本）。
%% 包含严格的调用限制、工具重试、模型重试和模型降级，适用于生产部署。
%% @returns 中间件规格列表
-spec production() -> [term()].
production() -> production(#{}).

%% @doc 生产环境预设（带参版本）。
%% 采用更严格的限制参数：
%% - 模型调用上限 15 次
%% - 工具调用上限 30 次
%% - 最大迭代 10 次
%% - 超限时直接中止（halt）
%% - 工具重试最多 2 次，指数退避（初始 500ms，最大 10s）
%% - 模型重试最多 2 次
%% - 支持模型降级列表
%%
%% @param Opts 配置选项映射，支持 call_limit、tool_retry、model_retry、
%%             model_fallback 和 fallback_models 键
%% @returns 中间件规格列表
-spec production(map()) -> [term()].
production(Opts) ->
    [
        {middleware_call_limit, maps:merge(#{
            max_model_calls => 15,
            max_tool_calls => 30,
            max_iterations => 10,
            on_limit_exceeded => halt
        }, maps:get(call_limit, Opts, #{})), 10},

        {middleware_tool_retry, maps:merge(#{
            max_retries => 2,
            backoff => #{
                type => exponential,
                initial_delay => 500,
                max_delay => 10000
            }
        }, maps:get(tool_retry, Opts, #{})), 30},

        {middleware_model_retry, maps:merge(#{
            max_retries => 2
        }, maps:get(model_retry, Opts, #{})), 40},

        {middleware_model_fallback, maps:merge(#{
            fallback_models => maps:get(fallback_models, Opts, [])
        }, maps:get(model_fallback, Opts, #{})), 50}
    ].

%% @doc 开发环境预设（无参版本）。
%% 采用宽松的限制参数，方便开发调试时进行多轮对话和工具调用。
%% @returns 中间件规格列表
-spec development() -> [term()].
development() -> development(#{}).

%% @doc 开发环境预设（带参版本）。
%% 采用宽松的限制参数：
%% - 模型调用上限 50 次
%% - 工具调用上限 100 次
%% - 最大迭代 30 次
%% - 超限时仅警告并继续（warn_and_continue）
%% - 工具重试最多 5 次
%%
%% @param Opts 配置选项映射，支持 call_limit 和 tool_retry 键
%% @returns 中间件规格列表
-spec development(map()) -> [term()].
development(Opts) ->
    [
        {middleware_call_limit, maps:merge(#{
            max_model_calls => 50,
            max_tool_calls => 100,
            max_iterations => 30,
            on_limit_exceeded => warn_and_continue
        }, maps:get(call_limit, Opts, #{})), 10},

        {middleware_tool_retry, maps:merge(#{
            max_retries => 5
        }, maps:get(tool_retry, Opts, #{})), 30}
    ].

%% @doc 人机协作预设（无参版本）。
%% 包含调用限制和人工审批中间件，适用于需要人工确认的关键操作场景。
%% @returns 中间件规格列表
-spec human_in_loop() -> [term()].
human_in_loop() -> human_in_loop(#{}).

%% @doc 人机协作预设（带参版本）。
%% 包含调用限制 + 人工审批，在工具执行前会请求人工确认。
%% @param Opts 配置选项映射，支持 call_limit 和 human_approval 键
%% @returns 中间件规格列表
-spec human_in_loop(map()) -> [term()].
human_in_loop(Opts) ->
    [
        call_limit(maps:get(call_limit, Opts, #{})),
        human_approval(maps:get(human_approval, Opts, #{}))
    ].

%%====================================================================
%% 单个中间件配置
%%====================================================================

%% @doc 调用限制中间件配置（无参版本）。
%% 使用默认限制参数创建调用限制中间件规格。
%% @returns 中间件规格元组 {Module, Opts, Priority}
-spec call_limit() -> term().
call_limit() -> call_limit(#{}).

%% @doc 调用限制中间件配置（带参版本）。
%% 默认参数：
%% - max_model_calls: 20（模型最大调用次数）
%% - max_tool_calls: 50（工具最大调用次数）
%% - max_tool_calls_per_turn: 10（每轮最大工具调用次数）
%% - max_iterations: 15（最大迭代次数）
%% - on_limit_exceeded: halt（超限时的处理方式）
%%
%% @param Opts 自定义配置选项，会与默认值合并
%% @returns 中间件规格元组 {Module, Opts, Priority}，优先级为 10
-spec call_limit(map()) -> term().
call_limit(Opts) ->
    DefaultOpts = #{
        max_model_calls => 20,
        max_tool_calls => 50,
        max_tool_calls_per_turn => 10,
        max_iterations => 15,
        on_limit_exceeded => halt
    },
    {middleware_call_limit, maps:merge(DefaultOpts, Opts), 10}.

%% @doc 人工审批中间件配置（无参版本）。
%% 使用默认参数创建人工审批中间件规格。
%% @returns 中间件规格元组 {Module, Opts, Priority}
-spec human_approval() -> term().
human_approval() -> human_approval(#{}).

%% @doc 人工审批中间件配置（带参版本）。
%% 默认参数：
%% - mode: all（所有工具调用都需要审批）
%% - timeout: 60000（审批超时时间，毫秒）
%% - timeout_action: reject（超时时拒绝执行）
%%
%% @param Opts 自定义配置选项，会与默认值合并
%% @returns 中间件规格元组 {Module, Opts, Priority}，优先级为 50
-spec human_approval(map()) -> term().
human_approval(Opts) ->
    DefaultOpts = #{
        mode => all,
        timeout => 60000,
        timeout_action => reject
    },
    {middleware_human_approval, maps:merge(DefaultOpts, Opts), 50}.

%% @doc 工具重试中间件配置（无参版本）。
%% 使用默认参数创建工具重试中间件规格。
%% @returns 中间件规格元组 {Module, Opts, Priority}
-spec tool_retry() -> term().
tool_retry() -> tool_retry(#{}).

%% @doc 工具重试中间件配置（带参版本）。
%% 默认参数：
%% - max_retries: 3（最大重试次数）
%% - backoff: 指数退避策略
%%   - type: exponential（退避类型）
%%   - initial_delay: 1000ms（初始延迟）
%%   - max_delay: 30000ms（最大延迟）
%%   - multiplier: 2（退避倍数）
%% - retryable_errors: all（所有错误均可重试）
%%
%% @param Opts 自定义配置选项，会与默认值合并
%% @returns 中间件规格元组 {Module, Opts, Priority}，优先级为 80
-spec tool_retry(map()) -> term().
tool_retry(Opts) ->
    DefaultOpts = #{
        max_retries => 3,
        backoff => #{
            type => exponential,
            initial_delay => 1000,
            max_delay => 30000,
            multiplier => 2
        },
        retryable_errors => all
    },
    {middleware_tool_retry, maps:merge(DefaultOpts, Opts), 80}.

%% @doc 模型重试中间件配置（无参版本）。
%% 使用默认参数创建模型重试中间件规格。
%% @returns 中间件规格元组 {Module, Opts, Priority}
-spec model_retry() -> term().
model_retry() -> model_retry(#{}).

%% @doc 模型重试中间件配置（带参版本）。
%% 默认参数：
%% - max_retries: 3（最大重试次数）
%% - backoff: 指数退避策略（含抖动）
%%   - type: exponential（退避类型）
%%   - initial_delay: 1000ms（初始延迟）
%%   - max_delay: 30000ms（最大延迟）
%%   - multiplier: 2（退避倍数）
%%   - jitter: true（启用随机抖动，避免雷群效应）
%%
%% @param Opts 自定义配置选项，会与默认值合并
%% @returns 中间件规格元组 {Module, Opts, Priority}，优先级为 90
-spec model_retry(map()) -> term().
model_retry(Opts) ->
    DefaultOpts = #{
        max_retries => 3,
        backoff => #{
            type => exponential,
            initial_delay => 1000,
            max_delay => 30000,
            multiplier => 2,
            jitter => true
        }
    },
    {middleware_model_retry, maps:merge(DefaultOpts, Opts), 90}.

%% @doc 模型降级中间件配置（无参版本）。
%% 使用默认参数创建模型降级中间件规格。
%% @returns 中间件规格元组 {Module, Opts, Priority}
-spec model_fallback() -> term().
model_fallback() -> model_fallback(#{}).

%% @doc 模型降级中间件配置（带参版本）。
%% 默认参数：
%% - fallback_models: []（降级模型列表，为空则不启用降级）
%%
%% 当主模型调用失败且重试耗尽时，会依次尝试降级模型列表中的模型。
%%
%% @param Opts 自定义配置选项，会与默认值合并
%% @returns 中间件规格元组 {Module, Opts, Priority}，优先级为 95
-spec model_fallback(map()) -> term().
model_fallback(Opts) ->
    DefaultOpts = #{
        fallback_models => []
    },
    {middleware_model_fallback, maps:merge(DefaultOpts, Opts), 95}.
