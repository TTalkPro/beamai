%%%-------------------------------------------------------------------
%%% @doc Filter 洋葱链示例
%%%
%%% 演示 beamai 的洋葱式 filter 机制：一个 filter 最多绑定 4 个 hook
%%% （pre_chat/post_chat/pre_tool/post_tool），同一 filter 的 pre/post 配成
%%% 一层洋葱包裹同一次调用，回程自动逆序。
%%%   - tool filter: pre_tool 改写参数 / post_tool 改写结果 / pre_tool 短路(halt)
%%%   - chat filter: pre_chat 注入 system 消息 / post_chat 审计响应
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

%% @doc 演示 tool filter（不需要 LLM）
-spec run_invoke() -> ok.
run_invoke() ->
    io:format("=== BeamAI Filter Example (Tool) ===~n~n"),

    %% 1. 创建 Kernel 并注册一个简单工具
    K0 = beamai:kernel(),
    K1 = beamai:add_tools(K0, [
        beamai:tool(<<"add">>,
            fun(#{a := A, b := B}) -> {ok, A + B} end,
            #{description => <<"Add two numbers">>,
              parameters => #{
                  a => #{type => integer, required => true},
                  b => #{type => integer, required => true}
              }})
    ]),

    %% 2. 一个 filter 同时管前后（洋葱同层）：pre_tool 记日志、post_tool 翻倍
    K2 = beamai:add_filter(K1, <<"log_double">>, #{
        pre_tool => fun(#{tool := #{name := Name}, args := Args} = Req) ->
            io:format("  [pre_tool] calling ~ts with ~p~n", [Name, Args]),
            Req
        end,
        post_tool => fun(#{result := Result} = Resp) when is_number(Result) ->
            io:format("  [post_tool] result ~p -> ~p~n", [Result, Result * 2]),
            Resp#{result => Result * 2};
           (Resp) ->
            Resp
        end
    }),

    io:format("Invoking add(3, 5):~n"),
    case beamai:invoke_tool(K2, <<"add">>, #{a => 3, b => 5}, beamai:context()) of
        {ok, Value, _} -> io:format("  Final result: ~p~n~n", [Value]);
        {error, Reason} -> io:format("  Error: ~p~n~n", [Reason])
    end,

    %% 3. pre_tool 短路(halt)：参数校验拒绝，跳过工具执行
    K3 = beamai:add_filter(K2, <<"validate">>, #{
        pre_tool => fun(#{args := #{a := A}, context := Ctx}) when A > 100 ->
            io:format("  [pre_tool] rejected: a=~p exceeds limit~n", [A]),
            {halt, #{result => {error, too_large}, context => Ctx}};
           (Req) ->
            Req
        end
    }),

    io:format("Invoking add(200, 1) with validation:~n"),
    case beamai:invoke_tool(K3, <<"add">>, #{a => 200, b => 1}, beamai:context()) of
        {ok, Value2, _} -> io:format("  Final result: ~p~n", [Value2]);
        {error, Reason2} -> io:format("  Rejected: ~p~n", [Reason2])
    end,
    ok.

%% @doc 演示 chat filter（需要 LLM）
-spec run_chat() -> ok.
run_chat() ->
    run_chat(example_llm_config:anthropic()).

%% @doc 演示 chat filter
%%
%% pre_chat: 自动注入 system 消息；post_chat: 记录响应统计
-spec run_chat(beamai_chat_completion:config()) -> ok.
run_chat(LLMConfig) ->
    io:format("=== BeamAI Filter Example (Chat) ===~n~n"),

    K0 = beamai:kernel(),
    K1 = beamai:add_llm(K0, LLMConfig),

    %% chat filter：pre_chat 注入 system、post_chat 打印响应字数（洋葱同层）
    K2 = beamai:add_filter(K1, <<"system_audit">>, #{
        pre_chat => fun(#{messages := Msgs} = Req) ->
            HasSystem = lists:any(fun(#{role := R}) -> R =:= system; (_) -> false end, Msgs),
            case HasSystem of
                true ->
                    Req;
                false ->
                    SystemMsg = #{role => system, content => <<"请用一句话简洁回答。"/utf8>>},
                    io:format("  [pre_chat] injected system message~n"),
                    Req#{messages => [SystemMsg | Msgs]}
            end
        end,
        post_chat => fun(#{response := Response} = Resp) ->
            Content = beamai_llm_response:content(Response),
            case is_binary(Content) of
                true -> io:format("  [post_chat] response length: ~B bytes~n", [byte_size(Content)]);
                false -> ok
            end,
            Resp
        end
    }),

    Messages = [#{role => user, content => <<"什么是 GenServer？"/utf8>>}],
    io:format("User: 什么是 GenServer？~n~n"),

    case beamai:chat(K2, Messages) of
        {ok, #{content := Content}, _} -> io:format("Assistant: ~ts~n", [Content]);
        {error, Reason} -> io:format("Error: ~p~n", [Reason])
    end,
    ok.
