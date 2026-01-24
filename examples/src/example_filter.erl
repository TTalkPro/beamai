%%%-------------------------------------------------------------------
%%% @doc Filter 管道示例
%%%
%%% 演示如何使用 beamai 的过滤器机制：
%%%   - pre_invocation: 函数调用前拦截（日志、参数验证）
%%%   - post_invocation: 函数调用后拦截（结果转换）
%%%   - pre_chat: Chat 请求前拦截（注入提示词）
%%%   - post_chat: Chat 响应后拦截（内容审计）
%%%
%%% 使用方法:
%%% ```
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%% example_filter:run_invoke().
%%% example_filter:run_chat().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_filter).

-export([run_invoke/0, run_chat/0, run_chat/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 演示函数调用过滤器（不需要 LLM）
%%
%% 注册 pre/post 过滤器，观察函数调用前后的拦截效果。
-spec run_invoke() -> ok.
run_invoke() ->
    io:format("=== BeamAI Filter Example (Invoke) ===~n~n"),

    %% 1. 创建 Kernel 并注册一个简单函数
    K0 = beamai:kernel(),
    K1 = beamai:add_plugin(K0, <<"math">>, [
        beamai:function(<<"add">>,
            fun(#{a := A, b := B}) -> {ok, A + B} end,
            #{description => <<"Add two numbers">>,
              parameters => #{
                  a => #{type => integer, required => true},
                  b => #{type => integer, required => true}
              }})
    ]),

    %% 2. 添加前置过滤器：记录调用日志
    K2 = beamai:add_filter(K1, <<"log_pre">>, pre_invocation,
        fun(#{function := #{name := Name}, args := Args} = Ctx) ->
            io:format("  [PRE] Calling ~ts with args: ~p~n", [Name, Args]),
            {continue, Ctx}
        end),

    %% 3. 添加后置过滤器：将结果翻倍
    K3 = beamai:add_filter(K2, <<"double_post">>, post_invocation,
        fun(#{result := Result} = Ctx) ->
            Doubled = Result * 2,
            io:format("  [POST] Result ~p -> doubled to ~p~n", [Result, Doubled]),
            {continue, Ctx#{result => Doubled}}
        end),

    %% 4. 调用函数，观察过滤器效果
    io:format("Invoking math.add(3, 5):~n"),
    case beamai:invoke(K3, <<"math.add">>, #{a => 3, b => 5}) of
        {ok, Value, _} ->
            io:format("  Final result: ~p~n~n", [Value]);
        {error, Reason} ->
            io:format("  Error: ~p~n~n", [Reason])
    end,

    %% 5. 演示 skip 过滤器：参数验证拒绝
    K4 = beamai:add_filter(K3, <<"validate">>, pre_invocation,
        fun(#{args := #{a := A}} = _Ctx) when A > 100 ->
            io:format("  [PRE] Rejected: a=~p exceeds limit~n", [A]),
            {skip, {error, too_large}};
           (Ctx) ->
            {continue, Ctx}
        end),

    io:format("Invoking math.add(200, 1) with validation:~n"),
    case beamai:invoke(K4, <<"math.add">>, #{a => 200, b => 1}) of
        {ok, Value2, _} ->
            io:format("  Final result: ~p~n", [Value2]);
        {error, Reason2} ->
            io:format("  Rejected: ~p~n", [Reason2])
    end,
    ok.

%% @doc 演示 Chat 过滤器（需要 LLM）
-spec run_chat() -> ok.
run_chat() ->
    LLMConfig = example_llm_config:zhipu(),
    run_chat(LLMConfig).

%% @doc 演示 Chat 过滤器
%%
%% pre_chat: 自动注入 system 消息
%% post_chat: 记录响应统计
-spec run_chat(beamai_chat_completion:config()) -> ok.
run_chat(LLMConfig) ->
    io:format("=== BeamAI Filter Example (Chat) ===~n~n"),

    K0 = beamai:kernel(),
    K1 = beamai:add_llm(K0, LLMConfig),

    %% pre_chat 过滤器：自动注入简洁回答的 system 消息
    K2 = beamai:add_filter(K1, <<"inject_system">>, pre_chat,
        fun(#{messages := Msgs} = Ctx) ->
            HasSystem = lists:any(fun(#{role := R}) -> R =:= system; (_) -> false end, Msgs),
            case HasSystem of
                true ->
                    {continue, Ctx};
                false ->
                    SystemMsg = #{role => system, content => <<"请用一句话简洁回答。"/utf8>>},
                    io:format("  [PRE_CHAT] Injected system message~n"),
                    {continue, Ctx#{messages => [SystemMsg | Msgs]}}
            end
        end),

    %% post_chat 过滤器：打印响应字数
    K3 = beamai:add_filter(K2, <<"audit_response">>, post_chat,
        fun(#{result := #{content := Content}} = Ctx) when is_binary(Content) ->
            Len = byte_size(Content),
            io:format("  [POST_CHAT] Response length: ~B bytes~n", [Len]),
            {continue, Ctx};
           (Ctx) ->
            {continue, Ctx}
        end),

    %% 调用时不传 system 消息，过滤器会自动注入
    Messages = [
        #{role => user, content => <<"什么是 GenServer？"/utf8>>}
    ],
    io:format("User: 什么是 GenServer？~n~n"),

    case beamai:chat(K3, Messages) of
        {ok, #{content := Content}, _} ->
            io:format("Assistant: ~ts~n", [Content]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.
