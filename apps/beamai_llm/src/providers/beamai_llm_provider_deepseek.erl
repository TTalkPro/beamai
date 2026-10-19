%%%-------------------------------------------------------------------
%%% @doc DeepSeek LLM Provider 实现
%%%
%%% DeepSeek API 与 OpenAI Chat Completions 兼容，请求/响应格式相同。
%%% 文档: https://api-docs.deepseek.com/zh-cn/api/create-chat-completion
%%%
%%% 支持的模型：
%%%   - deepseek-flash: V4.1-Flash（思考与非思考模式，默认思考；支持图像理解）
%%%   - deepseek-v4-pro: V4-Pro（仅思考模式；不支持图像理解）
%%%
%%% 支持的功能：
%%%   - 基本对话 (chat/stream_chat)
%%%   - 工具调用 (tools + tool_choice)，流式分片工具调用累加
%%%   - 思考模式：thinking（enabled / disabled）+ reasoning_effort
%%%     （none / low / high / max），思维链内容 reasoning_content（同步与流式）
%%%   - 多模态输入（图片 image_url / 文件 file，经 message_adapter 转换）
%%%   - 停止序列 (stop) / 对数概率 (logprobs, top_logprobs) / 用户隔离 (user_id)
%%%   - JSON 输出模式 (response_format: json_object)
%%%   - 上下文硬盘缓存统计（usage.details 中的 prompt_cache_hit/miss_tokens、
%%%     prompt_tokens_details.cached_tokens、completion_tokens_details.reasoning_tokens）
%%%   - 对话前缀续写（Beta）：末尾 assistant 消息标 prefix => true
%%%   - FIM 填空补全（Beta，仅非思考模式支持）：fim/2 和 stream_fim/3
%%%
%%% == 思考模式 ==
%%%
%%% - `thinking' 接受 `#{type => enabled | disabled}'，也接受裸原子
%%%   enabled / disabled（binary 同理）。
%%% - `reasoning_effort' 接受 none / low / high / max；出于兼容，
%%%   minimal 映射为 low，medium / xhigh 映射为 high。
%%% - 思考模式下 temperature 不生效、top_p 有效区间为 0.95-1.0（服务端按 0.95
%%%   处理），两者按文档原样发送，由服务端规整。
%%% - 思考模式下 tool_choice 不支持 `required' 与指定具体 tool（API 返回 400），
%%%   构建请求时降级为 `auto'；确需强制调用请显式 `thinking => disabled'。
%%%
%%% == 已废弃 / 不支持的参数 ==
%%%
%%% - frequency_penalty / presence_penalty：官方标注 deprecated（传入无效果），不发送。
%%% - seed / n / logit_bias / parallel_tool_calls / service_tier / user：API 未提供，不发送。
%%% - response_format 仅支持 text 与 json_object，不支持 json_schema（配置了会被剔除）。
%%%
%%% == Beta 端点 ==
%%%
%%% 对话前缀续写与 FIM 补全要求 base_url 为 https://api.deepseek.com/beta，
%%% 本模块用等价 URL 表达（拼接结果与该 base_url 相同）：
%%%   - 对话前缀续写：<base_url>/beta/chat/completions
%%%   - FIM 补全：<base_url>/beta/completions
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% Config = beamai_chat_model:create(deepseek, #{
%%%     api_key => <<"sk-...">>,
%%%     model => <<"deepseek-flash">>,
%%%     thinking => #{type => enabled},
%%%     reasoning_effort => high
%%% }),
%%% {ok, Resp} = beamai_chat_model:chat(Config, Messages).
%%% ```
%%%
%%% @end
%%%
%%%-------------------------------------------------------------------
-module(beamai_llm_provider_deepseek).
-behaviour(beamai_llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([base_url/1, endpoint/2, headers/2, body/2, parser/1,
         stream_accumulator/1, stream_finalizer/1]).
-export([supports_tools/0, supports_streaming/0]).

%% DeepSeek 特有 API（beta）
-export([fim/2, stream_fim/3]).

-ifdef(TEST).
-export([build_request_body/2, build_fim_request_body/2, chat_endpoint/1,
         build_fim_url/1]).
-endif.

%% 默认值
-define(DEEPSEEK_BASE_URL, <<"https://api.deepseek.com">>).
-define(DEEPSEEK_ENDPOINT, <<"/chat/completions">>).
-define(DEEPSEEK_CHAT_BETA_ENDPOINT, <<"/beta/chat/completions">>).
-define(DEEPSEEK_FIM_ENDPOINT, <<"/beta/completions">>).
-define(DEEPSEEK_MODEL, <<"deepseek-flash">>).
-define(DEEPSEEK_MAX_TOKENS, 4096).
-define(DEEPSEEK_TEMPERATURE, 1.0).

%% Config 中可选参数与 DeepSeek API 字段的映射
%% （frequency_penalty / presence_penalty 已被官方标注 deprecated，不再发送）
-define(OPTIONAL_PARAMS, [
    {stop, <<"stop">>},
    {logprobs, <<"logprobs">>},
    {top_logprobs, <<"top_logprobs">>},
    {user_id, <<"user_id">>}
]).

%% 思考模式下 tool_choice 不支持 required 与指定具体 tool（API 返回 400），
%% 这两种取值降级为 auto。
-define(THINKING_DOWNGRADED_TOOL_CHOICES, [required, {tool, _}]).

%% FIM 请求级参数映射
-define(FIM_REQUEST_PARAMS, [
    {suffix, <<"suffix">>},
    {echo, <<"echo">>},
    {logprobs, <<"logprobs">>},   %% FIM 的 logprobs 为整数（0-20），与 chat 不同
    {stop, <<"stop">>}
]).

%% FIM Config 级参数映射
-define(FIM_CONFIG_PARAMS, [
    {stop, <<"stop">>}
]).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"DeepSeek">>.

default_config() ->
    #{
        base_url => ?DEEPSEEK_BASE_URL,
        model => ?DEEPSEEK_MODEL,
        timeout => beamai_llm_provider_common:default_timeout(deepseek),
        max_tokens => ?DEEPSEEK_MAX_TOKENS,
        temperature => ?DEEPSEEK_TEMPERATURE
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
%% 使用 DeepSeek 专用解析器，提取 reasoning_content（deepseek-reasoner）
%% 和缓存统计。消息含 prefix => true 时自动路由到 /beta 端点。
chat(Config, Request) ->
    beamai_llm_http_provider:chat(?MODULE, Config, Request).

%% @doc 发送流式聊天请求
%% 流式累加结果经 finalize_openai_stream 转换为与同步模式一致的统一响应
%% （含分片工具调用拼接和 reasoning_content 累加）。
stream_chat(Config, Request, Callback) ->
    beamai_llm_http_provider:stream_chat(?MODULE, Config, Request, Callback).

%%====================================================================
%% FIM 填空补全 API（beta）
%%====================================================================

%% @doc 发送 FIM（Fill-In-the-Middle）填空补全请求
%%
%% 使用 /beta/completions 端点，根据 prompt（前缀）和可选的
%% suffix（后缀）补全中间内容，适用于代码补全等场景。
%%
%% Request 参数：
%%   prompt   - 必填，补全前缀
%%   suffix   - 可选，补全后缀
%%   echo     - 可选，是否回显 prompt
%%   logprobs - 可选，整数（0-20）
%%   stop     - 可选，停止序列
%%
%% ```erlang
%% {ok, Resp} = beamai_llm_provider_deepseek:fim(Config, #{
%%     prompt => <<"def fib(n):">>,
%%     suffix => <<"    return fib(n-1) + fib(n-2)">>
%% }),
%% Completion = beamai_chat_response:content(Resp)
%% ```
-spec fim(map(), map()) -> {ok, map()} | {error, term()}.
fim(Config, Request) ->
    Url = build_fim_url(Config),
    Headers = build_headers(Config),
    Body = build_fim_request_body(Config, Request),
    Opts = beamai_llm_provider_common:with_pool_opt(
               #{timeout => beamai_llm_provider_common:request_timeout(Config, deepseek)},
               Config),
    beamai_llm_http_client:request(Url, Headers, Body, Opts,
                                   beamai_llm_response_parser:parser_deepseek_fim()).

%% @doc 发送流式 FIM 填空补全请求
-spec stream_fim(map(), map(), fun((term()) -> any())) -> {ok, map()} | {error, term()}.
stream_fim(Config, Request, Callback) ->
    Url = build_fim_url(Config),
    Headers = build_headers(Config),
    Body = build_fim_request_body(Config, beamai_chat_request:put_option(Request, stream, true)),
    Opts = beamai_llm_provider_common:with_pool_opt(#{
        timeout => beamai_llm_provider_common:request_timeout(Config, deepseek),
        finalizer => fun(Acc) ->
            beamai_llm_provider_common:finalize_completions_stream(
                Acc, beamai_llm_response_parser:parser_deepseek_fim())
        end
    }, Config),
    beamai_llm_http_client:stream_request(Url, Headers, Body, Opts, Callback,
                                          fun beamai_llm_provider_common:accumulate_completions_event/2).

%%====================================================================
%% 声明式回调：底层信息（怎么发由 beamai_llm_http_provider 统一负责）
%%====================================================================

base_url(_Config) -> ?DEEPSEEK_BASE_URL.

endpoint(_Config, Request) -> chat_endpoint(Request).

headers(Config, _Request) -> build_headers(Config).

body(Config, Request) -> build_request_body(Config, Request).

parser(_Config) -> beamai_llm_response_parser:parser_deepseek().

stream_accumulator(_Config) -> fun beamai_llm_provider_common:accumulate_openai_event/2.

stream_finalizer(_Config) ->
    fun(Acc) -> beamai_llm_provider_common:finalize_openai_stream(Acc, deepseek) end.

%%====================================================================
%% 请求构建（使用公共模块）
%%====================================================================


%% @private 构建 FIM 请求 URL
%% FIM 使用 beta 端点，可通过 Config 的 fim_endpoint 覆盖
%% （与 chat 的 endpoint 配置互不干扰）。
%% 官方要求 base_url 为 https://api.deepseek.com/beta，本实现用
%% <base_url>/beta/completions 表达，最终 URL 与之一致。
build_fim_url(Config) ->
    BaseUrl = maps:get(base_url, Config, ?DEEPSEEK_BASE_URL),
    Endpoint = maps:get(fim_endpoint, Config, ?DEEPSEEK_FIM_ENDPOINT),
    <<BaseUrl/binary, Endpoint/binary>>.

%% @private 选择 chat 端点
%% 对话前缀续写（Beta）：任一消息标记 prefix => true 时路由到 /beta 端点
%% （官方要求 base_url=https://api.deepseek.com/beta，拼接结果与此等价）。
chat_endpoint(#{messages := Messages}) when is_list(Messages) ->
    HasPrefix = lists:any(
        fun(#{prefix := true}) -> true;
           (_) -> false
        end, Messages),
    case HasPrefix of
        true -> ?DEEPSEEK_CHAT_BETA_ENDPOINT;
        false -> ?DEEPSEEK_ENDPOINT
    end;
chat_endpoint(_) ->
    ?DEEPSEEK_ENDPOINT.

%% @private 构建请求头
build_headers(Config) ->
    beamai_llm_provider_common:build_bearer_auth_headers(Config).

%% @private 构建请求体（使用管道模式）
build_request_body(Config, Request) ->
    Messages = beamai_chat_request:messages(Request),
    Options = beamai_chat_request:options(Request),
    Base = #{
        <<"model">> => maps:get(model, Config, ?DEEPSEEK_MODEL),
        <<"messages">> => beamai_llm_message_adapter:to_openai(Messages),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?DEEPSEEK_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?DEEPSEEK_TEMPERATURE)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> beamai_llm_provider_common:maybe_add_top_p(B, Config) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_params(B, Config, ?OPTIONAL_PARAMS) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_tools(B, Options) end,
        fun(B) -> add_tool_choice(B, Config, Options) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_stream(B, Options) end,
        fun(B) -> maybe_add_response_format(B, Config, Options) end,
        fun(B) -> add_thinking(B, Config, Options) end,
        fun(B) -> add_reasoning_effort(B, Config, Options) end
    ]).

%% @private 构建 FIM 请求体
%% 注：FIM（补全）不是 chat，请求形状是自己的一套（prompt/suffix/echo/logprobs），
%% **不**走 beamai_chat_request——那是 chat 的输入类型。
build_fim_request_body(Config, Request) ->
    Base = #{
        <<"model">> => maps:get(model, Config, ?DEEPSEEK_MODEL),
        <<"prompt">> => maps:get(prompt, Request, <<>>),
        <<"max_tokens">> => maps:get(max_tokens, Config, ?DEEPSEEK_MAX_TOKENS),
        <<"temperature">> => maps:get(temperature, Config, ?DEEPSEEK_TEMPERATURE)
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> beamai_llm_provider_common:maybe_add_top_p(B, Config) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_params(B, Config, ?FIM_CONFIG_PARAMS) end,
        %% Request 级参数后应用，可覆盖 Config 同名参数（如 stop）
        fun(B) -> beamai_llm_provider_common:maybe_add_params(B, Request, ?FIM_REQUEST_PARAMS) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_stream(B, Request) end
    ]).

%% @private 添加响应格式（JSON 模式）
%% 优先取 Request，其次取 Config；只接受 text / json_object，
%% 并归一化为 binary 键值（API 不接受 json_schema，配置了则整项剔除）。
%% DeepSeek 的 response_format 只有 text / json_object，不支持 json_schema，
%% 配置了 json_schema 则整项剔除。
maybe_add_response_format(Body, Config, Options) ->
    case maps:get(response_format, Options, maps:get(response_format, Config, undefined)) of
        #{<<"type">> := <<"json_schema">>} -> Body;
        #{type := json_schema} -> Body;
        #{<<"type">> := Type} when Type =:= <<"text">>; Type =:= <<"json_object">> ->
            Body#{<<"response_format">> => #{<<"type">> => Type}};
        #{type := Type} when Type =:= text; Type =:= json_object; Type =:= <<"text">>;
                             Type =:= <<"json_object">> ->
            Body#{<<"response_format">> => #{<<"type">> => normalize_response_format_type(Type)}};
        _ -> Body
    end.

normalize_response_format_type(<<"text">>) -> <<"text">>;
normalize_response_format_type(<<"json_object">>) -> <<"json_object">>;
normalize_response_format_type(text) -> <<"text">>;
normalize_response_format_type(json_object) -> <<"json_object">>.

%% @private 添加思考模式开关
%% `thinking' 接受 #{type => enabled | disabled} 或裸原子 enabled / disabled；
%% 单次 Options 优先于 Config。
add_thinking(Body, Config, Options) ->
    case thinking_type(Options, Config) of
        undefined -> Body;
        Type -> Body#{<<"thinking">> => #{<<"type">> => Type}}
    end.

%% @private 添加思考强度
%% none / low / high / max；兼容别名 minimal -> low、medium / xhigh -> high。
add_reasoning_effort(Body, Config, Options) ->
    case maps:get(reasoning_effort, Options, maps:get(reasoning_effort, Config, undefined)) of
        undefined -> Body;
        Value ->
            case normalize_reasoning_effort(Value) of
                undefined -> Body;
                Effort -> Body#{<<"reasoning_effort">> => Effort}
            end
    end.

%% @private 解析思考模式开关取值
thinking_type(Options, Config) ->
    normalize_thinking(maps:get(thinking, Options, maps:get(thinking, Config, undefined))).

normalize_thinking(#{type := Type}) -> normalize_thinking(Type);
normalize_thinking(#{<<"type">> := Type}) -> normalize_thinking(Type);
normalize_thinking(Type) -> normalize_thinking_atom(Type).

normalize_thinking_atom(<<"enabled">>) -> <<"enabled">>;
normalize_thinking_atom(<<"disabled">>) -> <<"disabled">>;
normalize_thinking_atom(enabled) -> <<"enabled">>;
normalize_thinking_atom(disabled) -> <<"disabled">>;
normalize_thinking_atom(_) -> undefined.

%% @private 思考强度归一化（含官方兼容别名）
normalize_reasoning_effort(<<"none">>) -> <<"none">>;
normalize_reasoning_effort(<<"low">>) -> <<"low">>;
normalize_reasoning_effort(<<"minimal">>) -> <<"low">>;
normalize_reasoning_effort(<<"high">>) -> <<"high">>;
normalize_reasoning_effort(<<"medium">>) -> <<"high">>;
normalize_reasoning_effort(<<"xhigh">>) -> <<"high">>;
normalize_reasoning_effort(<<"max">>) -> <<"max">>;
normalize_reasoning_effort(none) -> <<"none">>;
normalize_reasoning_effort(low) -> <<"low">>;
normalize_reasoning_effort(minimal) -> <<"low">>;
normalize_reasoning_effort(high) -> <<"high">>;
normalize_reasoning_effort(medium) -> <<"high">>;
normalize_reasoning_effort(xhigh) -> <<"high">>;
normalize_reasoning_effort(max) -> <<"max">>;
normalize_reasoning_effort(_) -> undefined.

%% @private 思考模式下 tool_choice 的 required / 指定具体 tool 会被 API 拒绝，
%% 降级为 auto（确需强制调用请显式关闭思考模式）。
add_tool_choice(Body, Config, Options) ->
    ToolChoice = maps:get(tool_choice, Options, maps:get(tool_choice, Config, undefined)),
    case thinking_enabled(Config, Options) of
        true -> beamai_llm_provider_common:maybe_add_tool_choice(Body, #{tool_choice => downgrade_tool_choice(ToolChoice)});
        false -> beamai_llm_provider_common:maybe_add_tool_choice(Body, Options)
    end.

downgrade_tool_choice(required) -> auto;
downgrade_tool_choice({tool, _Name}) -> auto;
downgrade_tool_choice(#{type := <<"function">>}) -> auto;
downgrade_tool_choice(#{<<"type">> := <<"function">>}) -> auto;
downgrade_tool_choice(_) -> undefined.

%% @private 思考模式是否启用
%% 未显式配置时按启用处理：deepseek-v4-pro 仅思考模式，deepseek-flash 默认思考。
thinking_enabled(Config, Options) ->
    case thinking_type(Options, Config) of
        <<"disabled">> -> false;
        _ ->
            case maps:get(reasoning_effort, Options,
                          maps:get(reasoning_effort, Config, undefined)) of
                undefined -> true;
                Value -> normalize_reasoning_effort(Value) =/= <<"none">>
            end
    end.
