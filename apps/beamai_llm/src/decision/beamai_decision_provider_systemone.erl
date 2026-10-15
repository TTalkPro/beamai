%%%-------------------------------------------------------------------
%%% @doc 通用 System One 决策 Provider
%%%
%%% 面向任意实现了 TypeSafe System One 协议的服务：LiteGate 网关
%%% （`http://127.0.0.1:4000/v1'，模型为路由名）、本地 Laya、vLLM 等。
%%% 没有默认站点，`base_url' 与 `model' 必填；`api_key' 可选（本地服务常无鉴权）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_decision_provider_systemone).
-behaviour(beamai_decision_provider_behaviour).

-export([name/0, default_config/0, validate_config/1, decide/2]).

name() -> <<"System One">>.

default_config() ->
    #{timeout => beamai_llm_provider_common:default_timeout(systemone)}.

validate_config(#{base_url := Base, model := Model})
  when is_binary(Base), byte_size(Base) > 0, is_binary(Model), byte_size(Model) > 0 ->
    ok;
validate_config(#{base_url := Base}) when is_binary(Base), byte_size(Base) > 0 ->
    {error, missing_model};
validate_config(_) ->
    {error, missing_base_url}.

%% @doc 执行决策请求（base_url 必填，故默认站点为空）
decide(Config, Request) ->
    case validate_config(Config) of
        ok -> beamai_decision_common:decide_http(systemone, Config, Request, <<>>, <<>>);
        Error -> Error
    end.
