%%%-------------------------------------------------------------------
%%% @doc TypeSafe 官方决策 Provider（System One 协议的出处）
%%%
%%% 站点 https://api.typesafe.ai/v1，默认模型 jev-latest（最新稳定版 Jev）。
%%% Key 在 https://console.typesafe.ai/keys 申请。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_decision_provider_typesafe).
-behaviour(beamai_decision_provider_behaviour).

-export([name/0, default_config/0, validate_config/1, decide/2]).

-define(BASE_URL, <<"https://api.typesafe.ai/v1">>).
-define(MODEL, <<"jev-latest">>).

name() -> <<"TypeSafe">>.

default_config() ->
    #{
        base_url => ?BASE_URL,
        model => ?MODEL,
        timeout => beamai_llm_provider_common:default_timeout(typesafe)
    }.

validate_config(#{api_key := Key}) when is_binary(Key), byte_size(Key) > 0 ->
    ok;
validate_config(_) ->
    {error, missing_api_key}.

decide(Config, Request) ->
    beamai_decision_common:decide_http(typesafe, Config, Request, ?BASE_URL, ?MODEL).
