%%%-------------------------------------------------------------------
%%% @doc LLM 统一错误结构
%%%
%%% 各 provider / HTTP 层返回的错误形态各异（{http_error, Code, Body}、
%%% {api_error, _}、{request_failed, _}、{invalid_response, _}、
%%% missing_api_key 等）。本模块把它们归一化为统一结构，便于上层
%%% 一致地判断错误类型、是否可重试、建议退避时间等。
%%%
%%% 统一结构（llm_error()）：
%%% ```erlang
%%% #{
%%%     '__llm_error__' => true,
%%%     type           => rate_limit | server_error | client_error | auth |
%%%                       timeout | network | invalid_response | api_error | unknown,
%%%     status         => non_neg_integer() | undefined,   %% HTTP 状态码
%%%     message        => binary(),                         %% 人类可读描述
%%%     provider       => atom() | undefined,
%%%     retryable      => boolean(),
%%%     retry_after_ms => non_neg_integer() | undefined,    %% 服务端建议退避
%%%     raw            => term()                             %% 原始错误 Reason
%%% }
%%% ```
%%%
%%% 用法：
%%% ```erlang
%%% case beamai_chat_completion:chat(Config, Messages) of
%%%     {ok, Resp} -> ...;
%%%     {error, Reason} ->
%%%         Err = beamai_llm_error:from_reason(Reason, anthropic),
%%%         case beamai_llm_error:type(Err) of
%%%             rate_limit -> ...;
%%%             auth -> ...;
%%%             _ -> ...
%%%         end
%%% end.
%%% ```
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_error).

-export([from_reason/1, from_reason/2]).
-export([is_error/1, type/1, status/1, message/1, provider/1,
         retryable/1, retry_after_ms/1, raw/1]).

-type error_type() :: rate_limit | server_error | client_error | auth |
                      timeout | network | invalid_response | api_error | unknown.
-type llm_error() :: #{
    '__llm_error__' := true,
    type := error_type(),
    status := non_neg_integer() | undefined,
    message := binary(),
    provider := atom() | undefined,
    retryable := boolean(),
    retry_after_ms := non_neg_integer() | undefined,
    raw := term()
}.

-export_type([llm_error/0, error_type/0]).

%%====================================================================
%% 归一化
%%====================================================================

%% @doc 将错误 Reason 归一化为统一结构（provider 未知）
-spec from_reason(term()) -> llm_error().
from_reason(Reason) ->
    from_reason(Reason, undefined).

%% @doc 将错误 Reason 归一化为统一结构
%%
%% 接受 {error, Reason} 或裸 Reason，统一拆出 Reason 再分类。
-spec from_reason(term(), atom() | undefined) -> llm_error().
from_reason({error, Reason}, Provider) ->
    from_reason(Reason, Provider);
from_reason(Reason, Provider) ->
    classify(Reason, Provider).

%%====================================================================
%% 分类
%%====================================================================

%% 429：限流（可重试，可能带 Retry-After）
classify({http_error, 429, Body}, Provider) ->
    mk(rate_limit, 429, <<"Rate limited">>, Provider, true, undefined, {http_error, 429, Body});
classify({http_error, 429, Body, Meta}, Provider) ->
    mk(rate_limit, 429, <<"Rate limited">>, Provider, true,
       retry_after_from_meta(Meta), {http_error, 429, Body, Meta});
%% 5xx：服务端错误（可重试）
classify({http_error, Code, Body}, Provider) when is_integer(Code), Code >= 500 ->
    mk(server_error, Code, status_message(Code), Provider, true, undefined, {http_error, Code, Body});
classify({http_error, Code, Body, Meta}, Provider) when is_integer(Code), Code >= 500 ->
    mk(server_error, Code, status_message(Code), Provider, true,
       retry_after_from_meta(Meta), {http_error, Code, Body, Meta});
%% 401/403：鉴权（不可重试）
classify({http_error, Code, Body}, Provider) when Code =:= 401; Code =:= 403 ->
    mk(auth, Code, status_message(Code), Provider, false, undefined, {http_error, Code, Body});
classify({http_error, Code, Body, _Meta}, Provider) when Code =:= 401; Code =:= 403 ->
    mk(auth, Code, status_message(Code), Provider, false, undefined, {http_error, Code, Body});
%% 其它 4xx：客户端错误（不可重试）
classify({http_error, Code, Body}, Provider) when is_integer(Code), Code >= 400 ->
    mk(client_error, Code, status_message(Code), Provider, false, undefined, {http_error, Code, Body});
classify({http_error, Code, Body, _Meta}, Provider) when is_integer(Code), Code >= 400 ->
    mk(client_error, Code, status_message(Code), Provider, false, undefined, {http_error, Code, Body});
%% 网络 / 超时
classify({request_failed, timeout}, Provider) ->
    mk(timeout, undefined, <<"Request timeout">>, Provider, true, undefined, {request_failed, timeout});
classify({request_failed, {closed, _} = R}, Provider) ->
    mk(network, undefined, <<"Connection closed">>, Provider, true, undefined, {request_failed, R});
classify({request_failed, R}, Provider) ->
    mk(network, undefined, to_message(R), Provider, false, undefined, {request_failed, R});
%% provider / 解析层错误
classify({api_error, E}, Provider) ->
    mk(api_error, undefined, api_error_message(E), Provider, false, undefined, {api_error, E});
classify({invalid_response, R}, Provider) ->
    mk(invalid_response, undefined, <<"Invalid response">>, Provider, false, undefined, {invalid_response, R});
classify({parse_error, R}, Provider) ->
    mk(invalid_response, undefined, <<"Response parse error">>, Provider, false, undefined, {parse_error, R});
classify(missing_api_key, Provider) ->
    mk(auth, undefined, <<"Missing API key">>, Provider, false, undefined, missing_api_key);
classify(Reason, Provider) ->
    mk(unknown, undefined, to_message(Reason), Provider, false, undefined, Reason).

%%====================================================================
%% 访问器
%%====================================================================

-spec is_error(term()) -> boolean().
is_error(#{'__llm_error__' := true}) -> true;
is_error(_) -> false.

-spec type(llm_error()) -> error_type().
type(#{type := T}) -> T.

-spec status(llm_error()) -> non_neg_integer() | undefined.
status(#{status := S}) -> S.

-spec message(llm_error()) -> binary().
message(#{message := M}) -> M.

-spec provider(llm_error()) -> atom() | undefined.
provider(#{provider := P}) -> P.

-spec retryable(llm_error()) -> boolean().
retryable(#{retryable := R}) -> R.

-spec retry_after_ms(llm_error()) -> non_neg_integer() | undefined.
retry_after_ms(#{retry_after_ms := Ms}) -> Ms.

-spec raw(llm_error()) -> term().
raw(#{raw := R}) -> R.

%%====================================================================
%% 内部
%%====================================================================

mk(Type, Status, Message, Provider, Retryable, RetryAfterMs, Raw) ->
    #{
        '__llm_error__' => true,
        type => Type,
        status => Status,
        message => Message,
        provider => Provider,
        retryable => Retryable,
        retry_after_ms => RetryAfterMs,
        raw => Raw
    }.

retry_after_from_meta(#{retry_after_ms := Ms}) when is_integer(Ms) -> Ms;
retry_after_from_meta(_) -> undefined.

status_message(400) -> <<"Bad request">>;
status_message(401) -> <<"Unauthorized">>;
status_message(403) -> <<"Forbidden">>;
status_message(404) -> <<"Not found">>;
status_message(422) -> <<"Unprocessable entity">>;
status_message(429) -> <<"Rate limited">>;
status_message(500) -> <<"Internal server error">>;
status_message(502) -> <<"Bad gateway">>;
status_message(503) -> <<"Service unavailable">>;
status_message(504) -> <<"Gateway timeout">>;
status_message(Code) -> <<"HTTP error ", (integer_to_binary(Code))/binary>>.

%% @private 从 provider 的 api_error 体里尽量取 message
api_error_message(#{<<"message">> := M}) when is_binary(M) -> M;
api_error_message(#{<<"error">> := #{<<"message">> := M}}) when is_binary(M) -> M;
api_error_message(#{<<"type">> := T}) when is_binary(T) -> T;
api_error_message(_) -> <<"API error">>.

to_message(B) when is_binary(B) -> B;
to_message(A) when is_atom(A) -> atom_to_binary(A, utf8);
to_message(Other) -> iolist_to_binary(io_lib:format("~p", [Other])).
