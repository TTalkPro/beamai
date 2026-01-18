%%%-------------------------------------------------------------------
%%% @doc Example 公共工具模块
%%%
%%% 提供 Examples 共用的 LLM 配置和辅助函数。
%%% 默认使用智谱 GLM-4.7 模型（通过 Anthropic 兼容 API）。
%%%
%%% 所有配置均通过 llm_client:create/2 创建。
%%%
%%% 使用方法:
%%% ```erlang
%%% %% 设置环境变量
%%% export ZHIPU_API_KEY=your-api-key
%%%
%%% %% 获取配置
%%% {ok, LLM} = example_utils:get_llm_config().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_utils).

%% LLM 配置 API
-export([
    get_llm_config/0,
    get_llm_config/1,
    get_llm_config/2,
    llm_config/0
]).

%% 运行辅助 API
-export([
    run_with_config/2,
    ensure_api_key/0
]).

%%====================================================================
%% 配置常量
%%====================================================================

-define(ZHIPU_ANTHROPIC_BASE_URL, <<"https://open.bigmodel.cn/api/anthropic">>).
-define(DEFAULT_MODEL, <<"glm-4.7">>).
-define(DEFAULT_MAX_TOKENS, 2048).
-define(DEFAULT_TIMEOUT, 120000).

%%====================================================================
%% LLM 配置 API
%%====================================================================

%% @doc 从环境变量获取 LLM 配置
%%
%% 使用 llm_client:create/2 创建配置。
%% 优先级：ZHIPU_API_KEY > ANTHROPIC_API_KEY > OPENAI_API_KEY
%%
%% 返回值:
%%   - {ok, Config} - 成功获取配置
%%   - {error, no_api_key} - 未找到任何 API Key
-spec get_llm_config() -> {ok, map()} | {error, no_api_key}.
get_llm_config() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            case os:getenv("ANTHROPIC_API_KEY") of
                false ->
                    case os:getenv("OPENAI_API_KEY") of
                        false ->
                            {error, no_api_key};
                        Key ->
                            {ok, llm_client:create(openai, #{
                                api_key => list_to_binary(Key)
                            })}
                    end;
                Key ->
                    {ok, llm_client:create(anthropic, #{
                        api_key => list_to_binary(Key)
                    })}
            end;
        Key ->
            {ok, llm_client:create(anthropic, #{
                api_key => list_to_binary(Key),
                base_url => ?ZHIPU_ANTHROPIC_BASE_URL,
                model => ?DEFAULT_MODEL,
                max_tokens => ?DEFAULT_MAX_TOKENS,
                timeout => ?DEFAULT_TIMEOUT
            })}
    end.

%% @doc 从 Config 获取或合并 LLM 配置
%%
%% 如果 Config 是已有效的 llm_client 配置，直接使用。
%% 否则尝试从环境变量读取并使用 llm_client:create/2 创建。
%% 支持 zhipu provider 自动转换为 anthropic 兼容 API。
-spec get_llm_config(map()) -> {ok, map()} | {error, no_api_key}.
get_llm_config(Config) ->
    case llm_client:is_valid_config(Config) of
        true ->
            {ok, Config};
        false ->
            case maps:get(provider, Config, undefined) of
                undefined ->
                    get_llm_config();
                Provider ->
                    EnvVar = provider_env_var(Provider),
                    case os:getenv(EnvVar) of
                        false -> {error, no_api_key};
                        Key ->
                            ApiKey = list_to_binary(Key),
                            FinalConfig = case Provider of
                                zhipu ->
                                    llm_client:create(anthropic, #{
                                        api_key => ApiKey,
                                        base_url => ?ZHIPU_ANTHROPIC_BASE_URL,
                                        model => maps:get(model, Config, ?DEFAULT_MODEL),
                                        max_tokens => maps:get(max_tokens, Config, ?DEFAULT_MAX_TOKENS)
                                    });
                                _ ->
                                    llm_client:create(Provider, Config#{api_key => ApiKey})
                            end,
                            {ok, FinalConfig}
                    end
            end
    end.

%% @doc 使用指定的 API Key 获取智谱配置
-spec get_llm_config(binary(), map()) -> map().
get_llm_config(ApiKey, Opts) when is_binary(ApiKey) ->
    llm_client:create(anthropic, #{
        api_key => ApiKey,
        base_url => ?ZHIPU_ANTHROPIC_BASE_URL,
        model => maps:get(model, Opts, ?DEFAULT_MODEL),
        max_tokens => maps:get(max_tokens, Opts, ?DEFAULT_MAX_TOKENS),
        timeout => maps:get(timeout, Opts, ?DEFAULT_TIMEOUT)
    }).

%% @doc 直接获取智谱 LLM 配置（用于简化调用）
%%
%% 与 get_llm_config/0 类似，但在失败时抛出异常而非返回错误。
%% 适用于示例代码中不需要处理错误的场景。
-spec llm_config() -> map().
llm_config() ->
    case get_llm_config() of
        {ok, Config} -> Config;
        {error, _} ->
            error({missing_env, "ZHIPU_API_KEY"})
    end.

%%====================================================================
%% 运行辅助 API
%%====================================================================

%% @doc 使用 LLM 配置运行函数
%%
%% 自动获取 LLM 配置并传递给回调函数。
%% 如果获取配置失败，显示错误提示。
-spec run_with_config(fun((map()) -> term()), binary()) -> term().
run_with_config(Fun, ErrorHint) when is_function(Fun, 1) ->
    case get_llm_config() of
        {ok, LLMConfig} ->
            Fun(LLMConfig);
        {error, Reason} ->
            io:format("~ts~n", [ErrorHint]),
            io:format("Please set ZHIPU_API_KEY environment variable.~n"),
            {error, Reason}
    end.

%% @doc 确保 API Key 存在
%%
%% 返回 API Key 或显示错误信息。
-spec ensure_api_key() -> {ok, binary()} | {error, no_api_key}.
ensure_api_key() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            io:format("Error: ZHIPU_API_KEY not set~n"),
            io:format("Please run: export ZHIPU_API_KEY=your-api-key~n"),
            {error, no_api_key};
        Key ->
            {ok, list_to_binary(Key)}
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 获取 provider 对应的环境变量名
-spec provider_env_var(atom()) -> string().
provider_env_var(openai) -> "OPENAI_API_KEY";
provider_env_var(anthropic) -> "ANTHROPIC_API_KEY";
provider_env_var(zhipu) -> "ZHIPU_API_KEY";
provider_env_var(_) -> "ZHIPU_API_KEY".
