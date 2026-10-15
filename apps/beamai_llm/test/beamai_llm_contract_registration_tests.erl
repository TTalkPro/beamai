%%%-------------------------------------------------------------------
%%% @doc Ensures every built-in non-experimental chat provider has an
%%%      offline contract harness. All checks run offline.
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_contract_registration_tests).

-include_lib("eunit/include/eunit.hrl").

registered_provider_modules_test() ->
    Registry = lists:sort(beamai_chat_model:chat_provider_modules()),
    Harness = lists:sort(beamai_llm_contract:providers()),
    ?assertEqual(Registry, Harness).
