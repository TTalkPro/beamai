%%%-------------------------------------------------------------------
%%% @doc 统一的 LLM 调用封装模块
%%%
%%% 提供与多种 LLM Provider 交互的统一接口。
%%% 基于 llm_client 抽象层，支持 OpenAI、Anthropic、Ollama 等。
%%%
%%% 设计模式：
%%%   - 门面模式：简化 LLM 调用接口
%%%   - 错误传播：API 调用失败时返回错误信息，不进行 mock 回退
%%%
%%% 配置要求：
%%%   - 推荐使用 llm_client:create/2 创建配置
%%%   - 必须提供有效的 API Key（OpenAI、Anthropic、Zhipu）
%%%   - 或配置 Ollama 本地服务
%%%
%%% 使用示例：
%%% <pre>
%%%   创建 LLM 配置（推荐）:
%%%     LLM = llm_client:create(anthropic, #{api_key =&gt; API_KEY, model =&gt; &lt;&lt;"glm-4.7"&gt;&gt;})
%%%   调用专家分析:
%%%     Result = llm_helper:call_expert(Expert, Question, LLM)
%%%   生成综合建议:
%%%     Result = llm_helper:synthesize(TechAnalysis, BizAnalysis, UxAnalysis, LLM)
%%% </pre>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_helper).

%% API 导出
-export([call_expert/3]).
-export([synthesize/4]).
-export([call_llm/3]).

%% 类型引用
-type expert_config() :: #{
    name := binary(),
    system_prompt := binary(),
    focus := binary()
}.
-type llm_config() :: llm_client:config() | #{
    api_key => binary(),
    model => binary(),
    max_tokens => pos_integer()
}.

-export_type([llm_config/0, expert_config/0]).

%%====================================================================
%% 公共 API
%%====================================================================

%% @doc 调用 LLM 获取专家分析
%% 使用专家的系统提示词与用户问题构建对话
-spec call_expert(expert_config(), binary(), llm_config()) -> binary() | {error, term()}.
call_expert(Expert, Question, LLMConfig) ->
    SystemPrompt = maps:get(system_prompt, Expert),
    Messages = [
        #{role => system, content => SystemPrompt},
        #{role => user, content => Question}
    ],
    call_llm(Messages, LLMConfig, 500).

%% @doc 生成综合建议
%% 基于三位专家的分析，调用 LLM 生成统一建议
-spec synthesize(binary(), binary(), binary(), llm_config()) -> binary() | {error, term()}.
synthesize(TechAnalysis, BizAnalysis, UxAnalysis, LLMConfig) ->
    SystemPrompt = <<"你是一位决策顾问。请基于三位专家的分析，提供综合建议。
请包含以下内容：
1. 总体建议（推荐/不推荐/有条件推荐）
2. 关键考量因素
3. 建议的后续步骤

请保持简洁，不超过300字。"/utf8>>,

    UserPrompt = iolist_to_binary([
        <<"## 技术分析\n"/utf8>>, TechAnalysis, <<"\n\n">>,
        <<"## 业务分析\n"/utf8>>, BizAnalysis, <<"\n\n">>,
        <<"## 用户体验分析\n"/utf8>>, UxAnalysis
    ]),

    Messages = [
        #{role => system, content => SystemPrompt},
        #{role => user, content => UserPrompt}
    ],

    call_llm(Messages, LLMConfig, 800).

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 统一的 LLM 调用接口
-spec call_llm([map()], map(), pos_integer()) -> binary() | {error, term()}.
call_llm(Messages, LLMConfig, MaxTokens) ->
    call_llm_internal(Messages, LLMConfig, MaxTokens).

%% @doc 统一的 LLM 调用接口（内部实现）
%% 自动检测 Provider 并路由到对应实现
-spec call_llm_internal([map()], map(), pos_integer()) -> binary() | {error, term()}.
call_llm_internal(Messages, LLMConfig, MaxTokens) ->
    case maps:find(api_key, LLMConfig) of
        {ok, ApiKey} when is_binary(ApiKey), byte_size(ApiKey) > 0 ->
            Config = build_config(LLMConfig, MaxTokens),
            parse_response(llm_client:chat(Config, Messages));
        error ->
            %% 检查是否配置了 Ollama（无需 API Key）
            case maps:get(provider, LLMConfig, undefined) of
                ollama ->
                    Config = build_config(LLMConfig, MaxTokens),
                    parse_response(llm_client:chat(Config, Messages));
                _ ->
                    {error, missing_api_key}
            end
    end.

%% @doc 构建 LLM 配置
%% 将简化配置转换为完整的 llm_client 配置
-spec build_config(map(), pos_integer()) -> llm_client:config().
build_config(LLMConfig, MaxTokens) ->
    Provider = detect_provider(LLMConfig),
    BaseConfig = #{
        max_tokens => maps:get(max_tokens, LLMConfig, MaxTokens)
    },
    %% 合并用户配置
    MergedConfig = maps:merge(BaseConfig, LLMConfig),
    llm_client:create(Provider, MergedConfig).

%% @doc 检测 Provider 类型
%% 根据配置或 API Key 前缀自动判断
-spec detect_provider(map()) -> llm_client:provider().
detect_provider(#{provider := P}) -> P;
detect_provider(#{api_key := Key}) when is_binary(Key) ->
    case Key of
        <<"sk-ant-", _/binary>> -> anthropic;
        <<"sk-", _/binary>> -> openai;
        _ -> openai
    end;
detect_provider(_) -> ollama.

%% @doc 解析 LLM 响应
%% 统一提取内容文本
-spec parse_response({ok, map()} | {error, term()}) -> binary() | {error, term()}.
parse_response({ok, #{content := Content}}) when is_binary(Content) ->
    Content;
parse_response({ok, #{content := null}}) ->
    <<>>;
parse_response({error, Reason}) ->
    {error, {llm_error, Reason}}.
