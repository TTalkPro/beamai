%%%-------------------------------------------------------------------
%%% @doc OpenAI Responses API Provider 实现
%%%
%%% 走 `/v1/responses'（OpenAI 推荐的新一代接口）而非 Chat Completions，同时
%%% 兼容实现了 Responses 协议的网关（LiteGate、OpenRouter、vLLM 等）。与
%%% beamai_llm_provider_openai 的差别全在**协议形状**，统一消息 / 工具 / 响应
%%% 结构对上层不变：
%%%
%%%   - 输入：system 消息 → `instructions'；其余消息 → `input' 项列表
%%%     （message / function_call / function_call_output / reasoning），
%%%     见 beamai_llm_message_adapter:to_openai_responses/1
%%%   - 工具：扁平函数定义（无 `function' 嵌套），内置工具（web_search 等）
%%%     以 binary 键 map 原样透传，见 beamai_llm_tool_adapter:to_openai_responses/1
%%%   - 响应：`output' 项列表归一为 beamai_chat_response（content / tool_calls /
%%%     reasoning 内容块 / usage 明细），见 beamai_llm_response_parser:from_openai_responses/1
%%%   - 流式：语义事件累加，response.completed 携带的完整对象为准
%%%
%%% == 参数取值 ==
%%%
%%% 本次调用的 ChatRequest options 优先，其次 provider Config：
%%%   - model / temperature / top_p / user / store / previous_response_id /
%%%     metadata / parallel_tool_calls / truncation / include / service_tier /
%%%     max_tool_calls / prompt_cache_key / safety_identifier / background /
%%%     conversation —— 同名透传
%%%   - max_output_tokens（也接受 max_completion_tokens / max_tokens）
%%%   - reasoning（map）或 reasoning_effort + reasoning_summary → `reasoning'
%%%   - response_format（Chat 风格 json_schema / json_object / text）或
%%%     text（Responses 原生）→ `text.format'
%%%   - instructions（Config / options）：与 system 消息合并（配置在前）
%%%   - tools / tool_choice / stream：仅看 options（与其他 provider 一致）
%%%
%%% 不默认发送 temperature 与 max_output_tokens：推理模型（o 系列 / gpt-5）
%%% 拒绝 temperature，且推理 token 计入输出上限，默认值容易把回答截断。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_provider_openai_responses).
-behaviour(beamai_llm_provider_behaviour).

-include_lib("beamai_core/include/beamai_common.hrl").

%% Behaviour 回调
-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([base_url/1, endpoint/2, headers/2, body/2, parser/1,
         stream_accumulator/1, stream_finalizer/1]).
-export([supports_tools/0, supports_streaming/0]).

%% 供测试与调试直接构建请求体
-export([build_request_body/2]).

-define(BASE_URL, <<"https://api.openai.com">>).
-define(ENDPOINT, <<"/v1/responses">>).
-define(MODEL, <<"gpt-5">>).

%% 同名直接透传的可选参数（Request options 优先于 Config）
-define(PASSTHROUGH_PARAMS, [
    {temperature, <<"temperature">>},
    {top_p, <<"top_p">>},
    {user, <<"user">>},
    {store, <<"store">>},
    {previous_response_id, <<"previous_response_id">>},
    {metadata, <<"metadata">>},
    {parallel_tool_calls, <<"parallel_tool_calls">>},
    {truncation, <<"truncation">>},
    {include, <<"include">>},
    {service_tier, <<"service_tier">>},
    {max_tool_calls, <<"max_tool_calls">>},
    {prompt_cache_key, <<"prompt_cache_key">>},
    {safety_identifier, <<"safety_identifier">>},
    {background, <<"background">>},
    {conversation, <<"conversation">>}
]).

%%====================================================================
%% Behaviour 回调实现
%%====================================================================

name() -> <<"OpenAI Responses">>.

default_config() ->
    #{
        base_url => ?BASE_URL,
        model => ?MODEL,
        timeout => beamai_llm_provider_common:default_timeout(openai_responses)
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

chat(Config, Request) ->
    beamai_llm_http_provider:chat(?MODULE, Config, Request).

stream_chat(Config, Request, Callback) ->
    beamai_llm_http_provider:stream_chat(?MODULE, Config, Request, Callback).

%%====================================================================
%% 声明式回调
%%====================================================================

base_url(_Config) -> ?BASE_URL.

endpoint(_Config, _Request) -> ?ENDPOINT.

headers(Config, _Request) ->
    beamai_llm_provider_common:build_bearer_auth_headers(Config).

body(Config, Request) -> build_request_body(Config, Request).

parser(_Config) -> beamai_llm_response_parser:parser_openai_responses().

stream_accumulator(_Config) -> fun beamai_llm_provider_common:accumulate_responses_event/2.

stream_finalizer(_Config) -> fun beamai_llm_provider_common:finalize_responses_stream/1.

%%====================================================================
%% 请求构建
%%====================================================================

%% @doc 构建 Responses API 请求体
-spec build_request_body(map(), beamai_chat_request:t()) -> map().
build_request_body(Config, Request) ->
    Options = beamai_chat_request:options(Request),
    Params = maps:merge(Config, Options),
    {SystemInstructions, Input} =
        beamai_llm_message_adapter:to_openai_responses(beamai_chat_request:messages(Request)),
    Base = #{
        <<"model">> => maps:get(model, Params, ?MODEL),
        <<"input">> => Input
    },
    ?BUILD_BODY_PIPELINE(Base, [
        fun(B) -> add_instructions(B, maps:get(instructions, Params, undefined), SystemInstructions) end,
        fun(B) -> add_max_output_tokens(B, Params) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_params(B, Params, ?PASSTHROUGH_PARAMS) end,
        fun(B) -> add_reasoning(B, Params) end,
        fun(B) -> add_text_format(B, Params) end,
        fun(B) -> add_tools(B, Options) end,
        fun(B) -> add_tool_choice(B, Options) end,
        fun(B) -> beamai_llm_provider_common:maybe_add_stream(B, Options) end
    ]).

%% @private instructions：配置 / 本次选项给的在前，system 消息在后
add_instructions(Body, undefined, undefined) -> Body;
add_instructions(Body, Configured, undefined) when is_binary(Configured) -> Body#{<<"instructions">> => Configured};
add_instructions(Body, undefined, FromMessages) -> Body#{<<"instructions">> => FromMessages};
add_instructions(Body, Configured, FromMessages) when is_binary(Configured) ->
    Body#{<<"instructions">> => <<Configured/binary, "\n\n", FromMessages/binary>>};
add_instructions(Body, _, FromMessages) -> Body#{<<"instructions">> => FromMessages}.

%% @private 输出上限：max_output_tokens > max_completion_tokens > max_tokens
add_max_output_tokens(Body, Params) ->
    case first_defined([max_output_tokens, max_completion_tokens, max_tokens], Params) of
        N when is_integer(N), N > 0 -> Body#{<<"max_output_tokens">> => N};
        _ -> Body
    end.

%% @private reasoning：整 map 优先，否则由 reasoning_effort / reasoning_summary 拼
add_reasoning(Body, #{reasoning := Reasoning}) when is_map(Reasoning), map_size(Reasoning) > 0 ->
    Body#{<<"reasoning">> => binary_keys(Reasoning)};
add_reasoning(Body, Params) ->
    R0 = case maps:get(reasoning_effort, Params, undefined) of
        undefined -> #{};
        Effort -> #{<<"effort">> => to_bin(Effort)}
    end,
    R = case maps:get(reasoning_summary, Params, undefined) of
        undefined -> R0;
        Summary -> R0#{<<"summary">> => to_bin(Summary)}
    end,
    case map_size(R) of
        0 -> Body;
        _ -> Body#{<<"reasoning">> => R}
    end.

%% @private text.format：Responses 原生 text 优先，否则由 Chat 风格 response_format 转换
add_text_format(Body, #{text := Text}) when is_map(Text), map_size(Text) > 0 ->
    Body#{<<"text">> => binary_keys(Text)};
add_text_format(Body, #{response_format := Format}) when is_map(Format) ->
    case response_format_to_text(normalize_response_format(Format)) of
        undefined -> Body;
        TextFormat -> Body#{<<"text">> => #{<<"format">> => TextFormat}}
    end;
add_text_format(Body, _) ->
    Body.

response_format_to_text(#{<<"type">> := <<"json_schema">>, <<"json_schema">> := Schema}) when is_map(Schema) ->
    maps:filter(fun(_, V) -> V =/= undefined end, #{
        <<"type">> => <<"json_schema">>,
        <<"name">> => maps:get(<<"name">>, Schema, <<"response">>),
        <<"schema">> => maps:get(<<"schema">>, Schema, undefined),
        <<"strict">> => maps:get(<<"strict">>, Schema, undefined),
        <<"description">> => maps:get(<<"description">>, Schema, undefined)
    });
response_format_to_text(#{<<"type">> := <<"json_schema">>} = Flat) ->
    %% 已是 Responses 的扁平形态
    Flat;
response_format_to_text(#{<<"type">> := <<"json_object">>}) ->
    #{<<"type">> => <<"json_object">>};
response_format_to_text(#{<<"type">> := <<"text">>}) ->
    #{<<"type">> => <<"text">>};
response_format_to_text(_) ->
    undefined.

%% @private Chat 风格 response_format 的键与 type 值可能是 atom（统一格式），
%% 统一成 binary 再匹配；json_schema 子 map 同样处理
normalize_response_format(Format) ->
    F = binary_keys(Format),
    F1 = case maps:get(<<"type">>, F, undefined) of
        undefined -> F;
        Type -> F#{<<"type">> => to_bin(Type)}
    end,
    case maps:get(<<"json_schema">>, F1, undefined) of
        Schema when is_map(Schema) -> F1#{<<"json_schema">> => binary_keys(Schema)};
        _ -> F1
    end.

add_tools(Body, #{tools := Tools}) when is_list(Tools), Tools =/= [] ->
    Body#{<<"tools">> => beamai_llm_tool_adapter:to_openai_responses(Tools)};
add_tools(Body, _) ->
    Body.

%% @private tool_choice：指定函数时为扁平 {type: function, name}
add_tool_choice(Body, #{tool_choice := {tool, Name}}) when is_binary(Name) ->
    Body#{<<"tool_choice">> => #{<<"type">> => <<"function">>, <<"name">> => Name}};
add_tool_choice(Body, #{tool_choice := #{<<"type">> := <<"function">>, <<"function">> := #{<<"name">> := Name}}}) ->
    Body#{<<"tool_choice">> => #{<<"type">> => <<"function">>, <<"name">> => Name}};
add_tool_choice(Body, Options) ->
    beamai_llm_provider_common:maybe_add_tool_choice(Body, Options).

%%====================================================================
%% 内部工具
%%====================================================================

first_defined([], _Params) -> undefined;
first_defined([Key | Rest], Params) ->
    case maps:get(Key, Params, undefined) of
        undefined -> first_defined(Rest, Params);
        Value -> Value
    end.

%% @private 顶层键 atom → binary（值原样）
binary_keys(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) -> Acc#{to_bin(K) => V} end, #{}, Map).

to_bin(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_bin(B) when is_binary(B) -> B;
to_bin(Other) -> beamai_utils:to_binary(Other).
