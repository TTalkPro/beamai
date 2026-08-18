%%%-------------------------------------------------------------------
%%% @doc Moonshot AI (Kimi) LLM Provider 实现
%%%
%%% 支持月之暗面 Kimi 系列模型的 Chat Completions API（OpenAI 兼容）。
%%% API 文档: https://platform.moonshot.cn/docs/api-reference
%%%
%%% == 站点区域 ==
%%%
%%% Kimi 开放平台分国内站与国际站，密钥不互通，通过 `region' 选择：
%%%
%%%   - `cn'（默认）: https://api.moonshot.cn
%%%   - `global'    : https://api.moonshot.ai
%%%
%%% 也可直接用 `base_url' 覆盖（如自建代理）。
%%%
%%% 支持的功能：
%%%   - 基本对话 (chat/stream_chat)
%%%   - 多模态输入（图片，经 message_adapter 转为 image_url）
%%%   - 工具调用 (tools + tool_choice)，流式分片工具调用累加
%%%   - 思考模式 (thinking: #{type => enabled | disabled, budget_tokens => N})
%%%   - 思维链历史策略 (reasoning_history: disabled | interleaved | preserved)
%%%   - 推理强度 (reasoning_effort，Kimi K3 目前仅支持 max)
%%%   - 思维链内容 (reasoning_content，同步与流式均支持)
%%%   - 结构化输出 (response_format: json_object / json_schema)
%%%   - 部分模式续写：末尾 assistant 消息标 partial => true
%%%
%%% 注意（对齐 Vercel AI SDK 的 moonshotai provider）：
%%%   - json_schema 中的顶层 `$schema' 关键字会让 Kimi 输出异常，
%%%     构建请求时自动剔除（本地校验仍用完整 schema）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_provider_moonshot).
-behaviour(beamai_llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([supports_tools/0, supports_streaming/0]).

-ifdef(TEST).
-export([build_request_body/2, build_url/1]).
-endif.

%% 默认值
-define(MOONSHOT_BASE_URL_CN, <<"https://api.moonshot.cn">>).
-define(MOONSHOT_BASE_URL_GLOBAL, <<"https://api.moonshot.ai">>).
-define(MOONSHOT_ENDPOINT, <<"/v1/chat/completions">>).
-define(MOONSHOT_MODEL, <<"kimi-k2.5">>).
-define(MOONSHOT_MAX_TOKENS, 4096).
-define(MOONSHOT_TEMPERATURE, 0.6).

%% Config 中可选参数与 Moonshot API 字段的映射
-define(OPTIONAL_PARAMS, [
    {frequency_penalty, <<"frequency_penalty">>},
    {presence_penalty, <<"presence_penalty">>},
    {n, <<"n">>},
    {stop, <<"stop">>},
    {user, <<"user">>},
    {reasoning_effort, <<"reasoning_effort">>},
    {reasoning_history, <<"reasoning_history">>}
]).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"Moonshot AI">>.

default_config() ->
    #{
        region => cn,
        model => ?MOONSHOT_MODEL,
        timeout => beamai_llm_provider_common:default_timeout(moonshot),
        max_tokens => ?MOONSHOT_MAX_TOKENS,
        temperature => ?MOONSHOT_TEMPERATURE
    }.

validate_config(#{api_key := Key}) when is_binary(Key), byte_size(Key) > 0 ->
    ok;
validate_config(_) ->
    {error, missing_api_key}.

supports_tools() -> true.
supports_streaming() -> true.

%%====================================================================
%% 聊天 API
%%====================================================================

%% @doc 发送聊天请求
chat(Config, Request) ->
    Url = build_url(Config),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request),
    Opts = beamai_llm_provider_common:with_pool_opt(#{
        timeout => beamai_llm_provider_common:request_timeout(Config, moonshot),
        on_headers => fun beamai_llm_provider_common:rate_limit_metadata/1
    }, Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_llm_response_parser:parser_moonshot()).

%% @doc 发送流式聊天请求
stream_chat(Config, Request, Callback) ->
    Url = build_url(Config),
    Headers = build_headers(Config),
    Body = build_request_body(Config, Request#{stream => true}),
    Opts = beamai_llm_provider_common:with_pool_opt(#{
        timeout => beamai_llm_provider_common:request_timeout(Config, moonshot),
        finalizer => fun(Acc) ->
            beamai_llm_provider_common:finalize_openai_stream(Acc, moonshot)
        end,
        on_headers => fun beamai_llm_provider_common:rate_limit_metadata/1
    }, Config),
    beamai_llm_http_client:stream_request(Url, Headers, Body, Opts, Callback,
                                          fun beamai_llm_provider_common:accumulate_openai_event/2).

%%====================================================================
%% 请求构建
%%====================================================================

%% @private 构建请求 URL（base_url 优先，其次按 region 选择站点）
build_url(Config) ->
    Default = region_base_url(maps:get(region, Config, cn)),
    beamai_llm_provider_common:build_url(Config, ?MOONSHOT_ENDPOINT, Default).

%% @private 区域对应的站点
region_base_url(global) -> ?MOONSHOT_BASE_URL_GLOBAL;
region_base_url(_) -> ?MOONSHOT_BASE_URL_CN.

%% @private 构建请求头
build_headers(Config) ->
    beamai_llm_provider_common:build_bearer_auth_headers(Config).

%% @private 构建请求体（使用管道模式）
build_request_body(Config, Request) ->
    Messages = maps:get(messages, Request, []),
    Base = #{
        <<"model">> => maps:get(model, Config, ?MOONSHOT_MODEL),
        <<"messages">> => beamai_llm_message_adapter:to_openai(Messages),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?MOONSHOT_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?MOONSHOT_TEMPERATURE)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> beamai_llm_provider_common:maybe_add_top_p(B, Config) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_params(B, Config, ?OPTIONAL_PARAMS) end,
        fun(B) -> maybe_add_thinking(B, Config) end,
        fun(B) -> maybe_add_response_format(B, Config, Request) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tools(B, Request) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tool_choice(B, Request) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_stream(B, Request) end
    ]).

%% @private 添加思考模式配置
%% Config 的 thinking 为 map，budget_tokens 转为 API 的 budget_tokens 字段。
maybe_add_thinking(Body, Config) ->
    case maps:get(thinking, Config, undefined) of
        Thinking when is_map(Thinking) ->
            Body#{<<"thinking">> => build_thinking(Thinking)};
        _ ->
            Body
    end.

%% @private 归一化 thinking 结构（接受 atom / binary 键值）
build_thinking(Thinking) ->
    Base = case maps:get(type, Thinking, maps:get(<<"type">>, Thinking, undefined)) of
        undefined -> #{};
        Type -> #{<<"type">> => to_binary(Type)}
    end,
    case maps:get(budget_tokens, Thinking, maps:get(<<"budget_tokens">>, Thinking, undefined)) of
        undefined -> Base;
        Budget when is_integer(Budget) -> Base#{<<"budget_tokens">> => Budget};
        _ -> Base
    end.

%% @private 添加响应格式（Request 优先于 Config）
%% json_schema 的顶层 $schema 关键字会导致 Kimi 输出异常，统一剔除。
maybe_add_response_format(Body, Config, Request) ->
    case maps:get(response_format, Request, maps:get(response_format, Config, undefined)) of
        Format when is_map(Format) -> Body#{<<"response_format">> => strip_dollar_schema(Format)};
        _ -> Body
    end.

%% @private 剔除 json_schema.schema 中的顶层 $schema
strip_dollar_schema(#{<<"json_schema">> := JsonSchema} = Format) when is_map(JsonSchema) ->
    case maps:get(<<"schema">>, JsonSchema, undefined) of
        Schema when is_map(Schema) ->
            Format#{<<"json_schema">> => JsonSchema#{
                <<"schema">> => maps:without([<<"$schema">>], Schema)}};
        _ ->
            Format
    end;
strip_dollar_schema(Format) ->
    Format.

%% @private atom / binary 统一为 binary
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_binary(V) -> beamai_utils:to_binary(V).
