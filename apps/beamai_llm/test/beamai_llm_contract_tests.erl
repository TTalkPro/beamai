%%%-------------------------------------------------------------------
%%% @doc Provider contract harness 入口测试；全部离线执行。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_contract_tests).

-include_lib("eunit/include/eunit.hrl").

provider_contract_test_() ->
    [{atom_to_list(ProviderName),
      fun() -> ?assertEqual(ok, beamai_llm_contract:verify(ProviderName)) end}
     || ProviderName <- beamai_llm_contract:providers()]
    ++ [?_assertEqual(ok, beamai_llm_contract:assert_error_contract()),
        ?_assertEqual(ok, beamai_llm_contract:assert_multimodal_contract())].
