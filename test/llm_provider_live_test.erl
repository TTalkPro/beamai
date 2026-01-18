%%%-------------------------------------------------------------------
%%% @doc LLM Provider Live Test
%%%
%%% Test different Provider configurations with actual API calls.
%%% Requires ZHIPU_API_KEY environment variable.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_provider_live_test).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Test 1: GLM-4.7 via Anthropic Provider
%%====================================================================

glm47_anthropic_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"GLM-4.7 via Anthropic provider - simple chat",
       {timeout, 120, fun test_glm47_anthropic_chat/0}},
      {"GLM-4.7 via Anthropic provider - agent run",
       {timeout, 120, fun test_glm47_anthropic_agent/0}}
     ]}.

%%====================================================================
%% Test 2: GLM-4.6 via Zhipu Provider
%%====================================================================

glm46_zhipu_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     [
      {"GLM-4.6 via Zhipu provider - simple chat",
       {timeout, 120, fun test_glm46_zhipu_chat/0}},
      {"GLM-4.6 via Zhipu provider - agent run",
       {timeout, 120, fun test_glm46_zhipu_agent/0}}
     ]}.

%%====================================================================
%% Setup / Cleanup
%%====================================================================

setup() ->
    application:ensure_all_started(beamai_runtime),
    ok.

cleanup(_) ->
    ok.

%%====================================================================
%% GLM-4.7 Anthropic Provider Test Implementation
%%====================================================================

%% @doc Get GLM-4.7 Anthropic config
glm47_anthropic_config() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            io:format("~n[SKIP] ZHIPU_API_KEY not set~n"),
            skip;
        Key ->
            llm_client:create(anthropic, #{
                model => <<"glm-4.7">>,
                api_key => list_to_binary(Key),
                base_url => <<"https://open.bigmodel.cn/api/anthropic">>,
                timeout => 60000,
                max_tokens => 1024
            })
    end.

test_glm47_anthropic_chat() ->
    io:format("~n=== Testing GLM-4.7 via Anthropic Provider ===~n"),
    case glm47_anthropic_config() of
        skip ->
            io:format("Test skipped: API Key not set~n"),
            ok;
        Config ->
            io:format("Config: ~p~n", [maps:remove(api_key, Config)]),

            Messages = [
                #{role => user, content => <<"What is 1+1? Answer with just the number.">>}
            ],

            io:format("Sending message: ~p~n", [Messages]),
            Result = llm_client:chat(Config, Messages),
            io:format("Response: ~p~n", [Result]),

            case Result of
                {ok, Response} ->
                    Content = maps:get(content, Response, <<>>),
                    io:format("~nGLM-4.7 Anthropic response OK~n"),
                    io:format("Content: ~s~n", [Content]),
                    ?assert(is_binary(Content)),
                    ?assert(byte_size(Content) > 0);
                {error, Reason} ->
                    io:format("~nError: ~p~n", [Reason]),
                    ?assert(false)
            end
    end.

test_glm47_anthropic_agent() ->
    io:format("~n=== Testing GLM-4.7 Agent (Anthropic Provider) ===~n"),
    case glm47_anthropic_config() of
        skip ->
            io:format("Test skipped: API Key not set~n"),
            ok;
        LLMConfig ->
            {ok, Agent} = beamai_agent:start_link(<<"test_glm47">>, #{
                system_prompt => <<"You are a concise assistant. Keep responses short.">>,
                llm => LLMConfig
            }),

            io:format("Agent started~n"),
            Result = beamai_agent:run(Agent, <<"Hello, what is 2+2?">>),
            io:format("Agent result: ~p~n", [Result]),

            case Result of
                {ok, #{final_response := Response}} ->
                    io:format("~nAgent run OK~n"),
                    io:format("Response: ~s~n", [Response]),
                    ?assert(is_binary(Response));
                {ok, #{response := Response}} ->
                    io:format("~nAgent run OK~n"),
                    io:format("Response: ~s~n", [Response]),
                    ?assert(is_binary(Response));
                {ok, Other} ->
                    io:format("~nAgent result (other): ~p~n", [Other]),
                    ?assert(true);
                {error, Reason} ->
                    io:format("~nAgent error: ~p~n", [Reason]),
                    ?assert(false)
            end,

            beamai_agent:stop(Agent),
            ok
    end.

%%====================================================================
%% GLM-4.6 Zhipu Provider Test Implementation
%%====================================================================

%% @doc Get GLM-4.6 Zhipu config
glm46_zhipu_config() ->
    case os:getenv("ZHIPU_API_KEY") of
        false ->
            io:format("~n[SKIP] ZHIPU_API_KEY not set~n"),
            skip;
        Key ->
            llm_client:create(zhipu, #{
                model => <<"glm-4.6">>,
                api_key => list_to_binary(Key),
                timeout => 60000,
                max_tokens => 1024
            })
    end.

test_glm46_zhipu_chat() ->
    io:format("~n=== Testing GLM-4.6 via Zhipu Provider ===~n"),
    case glm46_zhipu_config() of
        skip ->
            io:format("Test skipped: API Key not set~n"),
            ok;
        Config ->
            io:format("Config: ~p~n", [maps:remove(api_key, Config)]),

            Messages = [
                #{role => user, content => <<"What is 2+2? Answer with just the number.">>}
            ],

            io:format("Sending message: ~p~n", [Messages]),
            Result = llm_client:chat(Config, Messages),
            io:format("Response: ~p~n", [Result]),

            case Result of
                {ok, Response} ->
                    Content = maps:get(content, Response, <<>>),
                    io:format("~nGLM-4.6 Zhipu response OK~n"),
                    io:format("Content: ~s~n", [Content]),
                    ?assert(is_binary(Content)),
                    ?assert(byte_size(Content) > 0);
                {error, Reason} ->
                    io:format("~nError: ~p~n", [Reason]),
                    ?assert(false)
            end
    end.

test_glm46_zhipu_agent() ->
    io:format("~n=== Testing GLM-4.6 Agent (Zhipu Provider) ===~n"),
    case glm46_zhipu_config() of
        skip ->
            io:format("Test skipped: API Key not set~n"),
            ok;
        LLMConfig ->
            {ok, Agent} = beamai_agent:start_link(<<"test_glm46">>, #{
                system_prompt => <<"You are a concise assistant. Keep responses short.">>,
                llm => LLMConfig
            }),

            io:format("Agent started~n"),
            Result = beamai_agent:run(Agent, <<"Hello, what is 3+3?">>),
            io:format("Agent result: ~p~n", [Result]),

            case Result of
                {ok, #{final_response := Response}} ->
                    io:format("~nAgent run OK~n"),
                    io:format("Response: ~s~n", [Response]),
                    ?assert(is_binary(Response));
                {ok, #{response := Response}} ->
                    io:format("~nAgent run OK~n"),
                    io:format("Response: ~s~n", [Response]),
                    ?assert(is_binary(Response));
                {ok, Other} ->
                    io:format("~nAgent result (other): ~p~n", [Other]),
                    ?assert(true);
                {error, Reason} ->
                    io:format("~nAgent error: ~p~n", [Reason]),
                    ?assert(false)
            end,

            beamai_agent:stop(Agent),
            ok
    end.
