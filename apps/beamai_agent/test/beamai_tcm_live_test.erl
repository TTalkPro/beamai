%%%-------------------------------------------------------------------
%%% @doc ToolCallingManager live 集成测试（MiniMax via Anthropic 兼容接口）
%%%
%%% 需要环境变量 MINIMAX_API_KEY。用 MiniMax-M2 模型经 Anthropic 兼容 API
%%% 验证 ToolCallingManager 经理分派的端到端工具调用。
%%%
%%% 手动运行：
%%%   MINIMAX_API_KEY=xxx ./rebar3 eunit --module=beamai_tcm_live_test
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tcm_live_test).

-include_lib("eunit/include/eunit.hrl").

%% MiniMax Anthropic 兼容端点
-define(MINIMAX_BASE_URL, <<"https://api.minimax.chat/anthropic">>).
-define(MINIMAX_MODEL, <<"MiniMax-M2">>).

%% 跳过条件：无 MINIMAX_API_KEY 时跳过（不 fail）
tcm_live_concurrent_test_() ->
    case os:getenv("MINIMAX_API_KEY") of
        false ->
            {skip, "MINIMAX_API_KEY not set"};
        _ ->
            {setup, fun ensure_apps/0, fun(_) ->
                {timeout, 60, fun test_concurrent/0}
            end}
    end.

tcm_live_sequential_test_() ->
    case os:getenv("MINIMAX_API_KEY") of
        false ->
            {skip, "MINIMAX_API_KEY not set"};
        _ ->
            {setup, fun ensure_apps/0, fun(_) ->
                {timeout, 60, fun test_sequential/0}
            end}
    end.

%% @private 确保 beamai_core + beamai_llm 应用已启动（HTTP pool 需要）
ensure_apps() ->
    {ok, _} = application:ensure_all_started(beamai_core),
    {ok, _} = application:ensure_all_started(beamai_llm),
    ok.

%%====================================================================
%% Tests
%%====================================================================

%% concurrent manager：多工具可并发执行
test_concurrent() ->
    run_with_manager(beamai_tool_calling_manager:concurrent(), <<"concurrent">>).

%% sequential manager：强制串行
test_sequential() ->
    run_with_manager(beamai_tool_calling_manager:sequential(), <<"sequential">>).

%%====================================================================
%% Helpers
%%====================================================================

run_with_manager(TCM, Label) ->
    ApiKey = list_to_binary(os:getenv("MINIMAX_API_KEY")),
    LLM = beamai_chat_completion:create(anthropic, #{
        model => ?MINIMAX_MODEL,
        api_key => ApiKey,
        base_url => ?MINIMAX_BASE_URL
    }),

    AddTool = #{
        name => <<"add">>,
        description => <<"Add two numbers and return the sum">>,
        parameters => #{
            a => #{type => integer, required => true, description => <<"first number">>},
            b => #{type => integer, required => true, description => <<"second number">>}
        },
        handler => fun(#{a := A, b := B}) -> {ok, A + B} end
    },
    MulTool = #{
        name => <<"mul">>,
        description => <<"Multiply two numbers and return the product">>,
        parameters => #{
            a => #{type => integer, required => true, description => <<"first number">>},
            b => #{type => integer, required => true, description => <<"second number">>}
        },
        handler => fun(#{a := A, b := B}) -> {ok, A * B} end
    },

    K0 = beamai_kernel:add_tools(beamai_kernel:new(), [AddTool, MulTool]),
    K1 = beamai_kernel:add_service(K0, LLM),
    {ok, Agent} = beamai_agent:new(#{
        kernel => K1,
        system_prompt => <<"You are a math assistant. Always use the provided tools for calculations. Be concise.">>,
        tool_calling_manager => TCM,
        max_tool_iterations => 5
    }),

    {ok, Result, _Agent2} = beamai_agent:run(Agent,
        <<"What is 17 + 25? Then multiply that result by 3.">>),

    Content = maps:get(content, Result),
    ToolCalls = maps:get(tool_calls_made, Result, []),
    Iterations = maps:get(iterations, Result, 0),

    %% 验证
    ?assert(is_binary(Content)),
    ?assert(byte_size(Content) > 0),
    ?assert(length(ToolCalls) >= 2), %% at least add + mul
    ?assert(Iterations >= 2),

    %% 结果应包含 126 (= 42 * 3)
    ResultStr = binary_to_list(Content),
    ?assert(lists:any(fun(S) -> string:find(S, "126") =/= nomatch end,
                      string:tokens(ResultStr, " \n,.;"))),

    io:format("~n[~s] Manager: ~p | tool_calls: ~p | iterations: ~p~n",
              [Label, element(1, TCM),
               [maps:get(name, TC) || TC <- ToolCalls], Iterations]),
    io:format("[~s] Response: ~ts~n", [Label, Content]),
    ok.
