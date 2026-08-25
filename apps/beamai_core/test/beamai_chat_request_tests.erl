%%%-------------------------------------------------------------------
%%% @doc ChatRequest（对标 Spring AI 的 Prompt）测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_chat_request_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MSGS, [#{role => user, content => <<"hi">>}]).

new_defaults_test() ->
    R = beamai_chat_request:new(?MSGS),
    ?assertEqual(?MSGS, beamai_chat_request:messages(R)),
    ?assertEqual(#{}, beamai_chat_request:options(R)),
    ?assertEqual([], beamai_chat_request:tools(R)),
    ?assertNot(beamai_chat_request:is_stream(R)).

new_with_options_test() ->
    R = beamai_chat_request:new(?MSGS, #{temperature => 0.2, stream => true}),
    ?assertEqual(0.2, beamai_chat_request:option(R, temperature)),
    ?assert(beamai_chat_request:is_stream(R)),
    ?assertEqual(undefined, beamai_chat_request:option(R, top_p)),
    ?assertEqual(7, beamai_chat_request:option(R, top_p, 7)).

%% 改写返回新请求，原请求不动（filter 会依赖这一点）
rewrite_is_pure_test() ->
    R0 = beamai_chat_request:new(?MSGS, #{temperature => 0.2}),
    R1 = beamai_chat_request:with_messages(R0, []),
    R2 = beamai_chat_request:put_option(R0, stream, true),
    R3 = beamai_chat_request:merge_options(R0, #{temperature => 0.9, top_p => 0.5}),
    ?assertEqual(?MSGS, beamai_chat_request:messages(R0)),
    ?assertEqual([], beamai_chat_request:messages(R1)),
    ?assertNot(beamai_chat_request:is_stream(R0)),
    ?assert(beamai_chat_request:is_stream(R2)),
    ?assertEqual(0.2, beamai_chat_request:option(R0, temperature)),
    ?assertEqual(0.9, beamai_chat_request:option(R3, temperature)),
    ?assertEqual(0.5, beamai_chat_request:option(R3, top_p)).

with_options_replaces_test() ->
    R0 = beamai_chat_request:new(?MSGS, #{temperature => 0.2, top_p => 0.1}),
    R1 = beamai_chat_request:with_options(R0, #{stream => true}),
    ?assertEqual(#{stream => true}, beamai_chat_request:options(R1)),
    ?assertEqual(undefined, beamai_chat_request:option(R1, temperature)).

%% ChatModel 层的选项（重试三件套）不下发给 provider
model_level_opts_not_in_request_test() ->
    Config = beamai_chat_model:create({custom, beamai_flaky_provider},
                                      #{attempts => counters:new(1, []), fail_n => 0}),
    Self = self(),
    meck:new(beamai_flaky_provider, [passthrough]),
    meck:expect(beamai_flaky_provider, chat, fun(_C, Req) ->
        Self ! {req, Req},
        {ok, #{content => <<"ok">>, finish_reason => <<"stop">>}}
    end),
    try
        {ok, _} = beamai_chat_model:chat(Config, ?MSGS,
                    #{max_retries => 5, retry_delay => 1, temperature => 0.3,
                      tools => [#{name => <<"t">>}]}),
        receive {req, Req} ->
            Opts = beamai_chat_request:options(Req),
            ?assertEqual(?MSGS, beamai_chat_request:messages(Req)),
            %% 模型参数下发
            ?assertEqual(0.3, maps:get(temperature, Opts)),
            ?assertEqual(1, length(beamai_chat_request:tools(Req))),
            %% 重试参数留在 ChatModel 层
            ?assertNot(maps:is_key(max_retries, Opts)),
            ?assertNot(maps:is_key(retry_delay, Opts))
        after 500 -> ?assert(false)
        end
    after
        meck:unload(beamai_flaky_provider)
    end.
