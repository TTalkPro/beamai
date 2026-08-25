%%%-------------------------------------------------------------------
%%% @doc 测试用 provider：按 Config 里的计数器与 fail_n 决定前几次失败
%%%
%%% 经 `{custom, ?MODULE}` provider 接入，不用 meck——这样重试真的发生在
%%% beamai_chat_model 内部（被测的正是那段）。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_flaky_provider).
-behaviour(beamai_llm_provider_behaviour).

-export([name/0, default_config/0, validate_config/1]).
-export([chat/2, stream_chat/3]).
-export([supports_tools/0, supports_streaming/0]).

name() -> <<"Flaky">>.
default_config() -> #{model => <<"flaky-model">>}.
validate_config(_Config) -> ok.
supports_tools() -> false.
supports_streaming() -> false.

chat(Config, _Request) ->
    case bump(Config) < maps:get(fail_n, Config, 0) of
        true -> {error, error_of(Config)};
        false -> {ok, #{content => <<"ok">>, finish_reason => <<"stop">>}}
    end.

%% 带 events 时回放这些流式事件（用于验证 token 投递），否则按 fail_n 行为失败
stream_chat(#{events := Events} = Config, _Request, Callback) ->
    _ = bump(Config),
    lists:foreach(fun(E) -> Callback(E) end, Events),
    {ok, #{content => <<"done">>, finish_reason => <<"stop">>}};
stream_chat(Config, _Request, _Callback) ->
    _ = bump(Config),
    {error, error_of(Config)}.

%% @private 记一次真实请求，返回本次之前已发生的次数
bump(#{attempts := Ctr}) ->
    N = counters:get(Ctr, 1),
    counters:add(Ctr, 1, 1),
    N.

error_of(Config) -> maps:get(error, Config, {http_error, 503, <<"busy">>}).
