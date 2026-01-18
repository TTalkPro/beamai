%%%-------------------------------------------------------------------
%%% @doc Middleware 预设配置
%%%
%%% 提供常用的 Middleware 组合，简化配置。
%%%
%%% == 预设组合 ==
%%%
%%% - `default()` - 推荐的默认配置
%%% - `minimal()` - 最小配置（仅调用限制）
%%% - `production()` - 生产环境配置
%%% - `development()` - 开发调试配置
%%% - `human_in_loop()` - 人工审批配置
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 使用预设配置创建 Agent
%%% {ok, State} = beamai_agent:create_state(#{
%%%     llm => #{provider => openai, model => <<"gpt-4">>},
%%%     tools => [...],
%%%     middlewares => beamai_middleware_presets:default()
%%% }).
%%%
%%% %% 自定义预设
%%% Middlewares = beamai_middleware_presets:default() ++ [
%%%     {my_custom_middleware, #{option => value}}
%%% ].
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_middleware_presets).

%% 预设配置
-export([
    default/0,
    default/1,
    minimal/0,
    minimal/1,
    production/0,
    production/1,
    development/0,
    development/1,
    human_in_loop/0,
    human_in_loop/1
]).

%% 单独 Middleware 配置
-export([
    call_limit/0,
    call_limit/1,
    summarization/0,
    summarization/1,
    human_approval/0,
    human_approval/1,
    tool_retry/0,
    tool_retry/1
]).

%%====================================================================
%% 预设配置
%%====================================================================

%% @doc 默认配置
%%
%% 包含：调用限制、摘要压缩
%% 适用于大多数场景
-spec default() -> [term()].
default() ->
    default(#{}).

-spec default(map()) -> [term()].
default(Opts) ->
    [
        call_limit(maps:get(call_limit, Opts, #{})),
        summarization(maps:get(summarization, Opts, #{}))
    ].

%% @doc 最小配置
%%
%% 仅包含调用限制，适用于简单场景
-spec minimal() -> [term()].
minimal() ->
    minimal(#{}).

-spec minimal(map()) -> [term()].
minimal(Opts) ->
    [
        call_limit(maps:get(call_limit, Opts, #{}))
    ].

%% @doc 生产环境配置
%%
%% 包含：调用限制、摘要压缩、工具重试
%% 更严格的限制，更健壮的错误处理
-spec production() -> [term()].
production() ->
    production(#{}).

-spec production(map()) -> [term()].
production(Opts) ->
    [
        {middleware_call_limit, maps:merge(#{
            max_model_calls => 15,
            max_tool_calls => 30,
            max_iterations => 10,
            on_limit_exceeded => halt
        }, maps:get(call_limit, Opts, #{})), 10},  %% 高优先级

        {middleware_summarization, maps:merge(#{
            window_size => 15,
            max_tokens => 3000,
            summarize => true,
            compress_threshold => 20
        }, maps:get(summarization, Opts, #{})), 20},

        {middleware_tool_retry, maps:merge(#{
            max_retries => 2,
            backoff => #{
                type => exponential,
                initial_delay => 500,
                max_delay => 10000
            }
        }, maps:get(tool_retry, Opts, #{})), 30}
    ].

%% @doc 开发调试配置
%%
%% 更宽松的限制，详细的日志
-spec development() -> [term()].
development() ->
    development(#{}).

-spec development(map()) -> [term()].
development(Opts) ->
    [
        {middleware_call_limit, maps:merge(#{
            max_model_calls => 50,
            max_tool_calls => 100,
            max_iterations => 30,
            on_limit_exceeded => warn_and_continue,
            debug => true
        }, maps:get(call_limit, Opts, #{})), 10},

        {middleware_summarization, maps:merge(#{
            window_size => 30,
            max_tokens => 8000,
            summarize => false,
            debug => true
        }, maps:get(summarization, Opts, #{})), 20},

        {middleware_tool_retry, maps:merge(#{
            max_retries => 5,
            debug => true
        }, maps:get(tool_retry, Opts, #{})), 30}
    ].

%% @doc 人工审批配置
%%
%% 包含人工审批，适用于需要人工确认的场景
-spec human_in_loop() -> [term()].
human_in_loop() ->
    human_in_loop(#{}).

-spec human_in_loop(map()) -> [term()].
human_in_loop(Opts) ->
    [
        call_limit(maps:get(call_limit, Opts, #{})),
        summarization(maps:get(summarization, Opts, #{})),
        human_approval(maps:get(human_approval, Opts, #{}))
    ].

%%====================================================================
%% 单独 Middleware 配置
%%====================================================================

%% @doc 调用限制 Middleware 配置
-spec call_limit() -> term().
call_limit() ->
    call_limit(#{}).

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

%% @doc 摘要压缩 Middleware 配置
-spec summarization() -> term().
summarization() ->
    summarization(#{}).

-spec summarization(map()) -> term().
summarization(Opts) ->
    DefaultOpts = #{
        window_size => 20,
        max_tokens => 4000,
        summarize => false,
        compress_threshold => 30,
        token_threshold => 3000
    },
    {middleware_summarization, maps:merge(DefaultOpts, Opts), 20}.

%% @doc 人工审批 Middleware 配置
-spec human_approval() -> term().
human_approval() ->
    human_approval(#{}).

-spec human_approval(map()) -> term().
human_approval(Opts) ->
    DefaultOpts = #{
        mode => all,  %% all | selective | custom | none
        timeout => 60000,
        timeout_action => reject
    },
    {middleware_human_approval, maps:merge(DefaultOpts, Opts), 50}.

%% @doc 工具重试 Middleware 配置
-spec tool_retry() -> term().
tool_retry() ->
    tool_retry(#{}).

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
