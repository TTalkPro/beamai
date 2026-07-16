%%%-------------------------------------------------------------------
%%% @doc ToolCallingManager live 集成测试（MiniMax via Anthropic 兼容接口）
%%%
%%% 需要环境变量 MINIMAX_API_KEY。用 MiniMax-M2 模型经 Anthropic 兼容 API
%%% 验证 ToolCallingManager 经理分派的端到端工具调用。
%%%
%%% 手动运行：
%%%   MINIMAX_API_KEY=xxx rebar3 eunit --module=beamai_tcm_live_test
%%%
%%% 测试设计：工具返回**模型不可能猜到的不透明数据**（内部目录价格），
%%% 因此最终答案含正确总价 <=> 两次查价都真的执行了、且结果真的回灌了模型。
%%% 用算术题（如 17+25）做不到这点——模型心算就能给出答案，工具没跑测试照样过。
%%%
%%% 断言分三层：
%%%   1. handler 副作用（ETS）——工具真的被调用了，参数正确
%%%   2. spy manager 分派记录——tool_calls 真的经 manager seam，没绕过去
%%%   3. 最终答案含总价——工具结果真的回灌了模型
%%%
%%% 并发/串行的重叠语义由 beamai_tool_calling_manager_tests 确定性覆盖
%%% （sequential_no_overlap / concurrent_overlap），此处不重复断言——
%%% 真实模型分几批发 tool_calls 是模型自由，不该让测试依赖它。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tcm_live_test).

-include_lib("eunit/include/eunit.hrl").

%% MiniMax Anthropic 兼容端点
-define(MINIMAX_BASE_URL, <<"https://api.minimax.chat/anthropic">>).
-define(MINIMAX_MODEL, <<"MiniMax-M2">>).

%% 不透明价格（单位：分）——模型无从猜起，只能查工具
-define(WIDGET_PRICE, 4173).
-define(GADGET_PRICE, 2891).
-define(TOTAL_PRICE, 7064).

%% 跳过条件：无 MINIMAX_API_KEY 时跳过（不 fail）
tcm_live_concurrent_test_() ->
    live_case(fun test_concurrent/0).

tcm_live_sequential_test_() ->
    live_case(fun test_sequential/0).

%% @private 有 key 才跑，否则 skip
live_case(Fun) ->
    case os:getenv("MINIMAX_API_KEY") of
        false ->
            {skip, "MINIMAX_API_KEY not set"};
        _ ->
            {setup, fun ensure_apps/0, fun(_) -> {timeout, 60, Fun} end}
    end.

%% @private 确保 beamai_core + beamai_llm 应用已启动（HTTP pool 需要）
ensure_apps() ->
    {ok, _} = application:ensure_all_started(beamai_core),
    {ok, _} = application:ensure_all_started(beamai_llm),
    ok.

%%====================================================================
%% Tests
%%====================================================================

%% concurrent manager：同批多工具可并发执行
test_concurrent() ->
    run_with_manager(beamai_tool_calling_manager:concurrent(), <<"concurrent">>).

%% sequential manager：强制串行
test_sequential() ->
    run_with_manager(beamai_tool_calling_manager:sequential(), <<"sequential">>).

%%====================================================================
%% Helpers
%%====================================================================

run_with_manager(Inner, Label) ->
    %% public 表：工具 handler 在 spawn 出的进程里跑（concurrent 路径）
    Tab = ets:new(tcm_live_observations, [public, ordered_set]),
    TCM = beamai_spy_tcm_impl:wrap(Inner, Tab),

    ApiKey = list_to_binary(os:getenv("MINIMAX_API_KEY")),
    LLM = beamai_chat_completion:create(anthropic, #{
        model => ?MINIMAX_MODEL,
        api_key => ApiKey,
        base_url => ?MINIMAX_BASE_URL
    }),

    K0 = beamai_kernel:add_tools(beamai_kernel:new(), [price_tool(Tab)]),
    K1 = beamai_kernel:add_service(K0, LLM),
    {ok, Agent} = beamai_agent:new(#{
        kernel => K1,
        system_prompt => <<"You are a store assistant. Item prices are secret and are "
                           "ONLY available via the lookup_price tool — never guess a price. "
                           "Be concise.">>,
        tool_calling_manager => TCM,
        max_tool_iterations => 5
    }),

    {ok, Result, _Agent2} = beamai_agent:run(Agent,
        <<"What is the total price in cents of one widget plus one gadget?">>),

    Content = maps:get(content, Result),
    Dispatches = beamai_spy_tcm_impl:dispatches(Tab),
    Looked = looked_up_items(Tab),

    ?debugFmt("[~s] manager=~p dispatches=~p looked_up=~p iterations=~p~nresponse: ~ts",
              [Label, element(1, Inner), Dispatches, Looked,
               maps:get(iterations, Result, 0), Content]),

    %% 1. 工具真的执行了，两个商品都查了（顺序不限）
    ?assertEqual([<<"gadget">>, <<"widget">>], lists:sort(lists:usort(Looked))),

    %% 2. tool_calls 经 manager seam 分派（spy 记到了批次，且都是 lookup_price）
    ?assert(length(Dispatches) >= 1),
    ?assertEqual([<<"lookup_price">>], lists:usort(lists:flatten(Dispatches))),

    %% 3. 工具结果回灌了模型：总价只能从两次查价得出
    ?assert(is_binary(Content)),
    ?assert(contains_total(Content)),
    ok.

%% @private 查价工具：价格不透明，且把每次调用记进 ETS
price_tool(Tab) ->
    #{
        name => <<"lookup_price">>,
        description => <<"Look up the price in cents of an item in the internal catalog. "
                         "This is the only source of prices.">>,
        parameters => #{
            item => #{type => string, required => true,
                      description => <<"item name, e.g. widget or gadget">>}
        },
        %% 注意：parameters 用 atom key 声明，但 handler 收到的 args 是 **binary key**
        %% （beamai_tool:parse_args/1 走 jsx:decode(return_maps)，不 atom 化）。
        handler => fun(#{<<"item">> := Item}) ->
            Norm = normalize(Item),
            ets:insert(Tab, {{call, erlang:unique_integer([monotonic])}, Norm}),
            case Norm of
                <<"widget">> -> {ok, ?WIDGET_PRICE};
                <<"gadget">> -> {ok, ?GADGET_PRICE};
                _ -> {error, {unknown_item, Norm}}
            end
        end
    }.

%% @private 模型可能传 "Widget" / " widget"，比价前归一
normalize(Item) when is_binary(Item) ->
    string:lowercase(string:trim(Item));
normalize(Item) when is_list(Item) ->
    normalize(list_to_binary(Item)).

%% @private 取出所有被查过的商品名（按发生序）
looked_up_items(Tab) ->
    [Item || {{call, _Seq}, Item} <- lists:sort(ets:tab2list(Tab))].

%% @private 答案是否含总价（容忍 7064 / 7,064 等写法）
contains_total(Content) ->
    Total = integer_to_binary(?TOTAL_PRICE),
    Stripped = binary:replace(Content, <<",">>, <<>>, [global]),
    binary:match(Stripped, Total) =/= nomatch.
