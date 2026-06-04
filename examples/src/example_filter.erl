%%%-------------------------------------------------------------------
%%% @doc Filter 洋葱链示例
%%%
%%% 演示 beamai 的洋葱式 filter 机制：一个 filter 最多绑定 2 个 around hook
%%% （around_chat/around_tool），每个 around 用单个闭包
%%% `fun(Req, FCtx, Next) -> Resp | {Resp, NewFCtx}` 包裹一次调用，前置/后置
%%% 同处一处，不调 Next 即短路。
%%%   - tool filter: around_tool 前置改写参数 / 后置改写结果 / 短路拒绝
%%%   - chat filter: around_chat 注入 system 消息 + 审计响应
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

    %% 2. 一个 around_tool 同时管前后：记日志（前置）+ 结果翻倍（后置）
    K2 = beamai:add_filter(K1, <<"log_double">>, #{
        around_tool => fun(#{tool := #{name := Name}, args := Args} = Req, _FCtx, Next) ->
            io:format("  [前置] calling ~ts with ~p~n", [Name, Args]),
            Resp = Next(Req),
            case Resp of
                #{result := Result} when is_number(Result) ->
                    io:format("  [后置] result ~p -> ~p~n", [Result, Result * 2]),
                    Resp#{result => Result * 2};
                _ ->
                    Resp
            end
        end
    }),

    io:format("Invoking add(3, 5):~n"),
    case beamai:invoke_tool(K2, <<"add">>, #{a => 3, b => 5}, beamai:context()) of
        {ok, Value, _} -> io:format("  Final result: ~p~n~n", [Value]);
        {error, Reason} -> io:format("  Error: ~p~n~n", [Reason])
    end,

    %% 3. around_tool 短路：参数校验拒绝（不调 Next，跳过工具执行）
    K3 = beamai:add_filter(K2, <<"validate">>, #{
        around_tool => fun(#{args := #{a := A}, context := Ctx} = Req, _FCtx, Next) ->
            case A > 100 of
                true ->
                    io:format("  [短路] rejected: a=~p exceeds limit~n", [A]),
                    #{result => {error, too_large}, context => Ctx};
                false ->
                    Next(Req)
            end
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
%% around_chat: 前置自动注入 system 消息；后置记录响应统计
-spec run_chat(beamai_chat_completion:config()) -> ok.
run_chat(LLMConfig) ->
    io:format("=== BeamAI Filter Example (Chat) ===~n~n"),

    K0 = beamai:kernel(),
    K1 = beamai:add_llm(K0, LLMConfig),

    %% chat filter：around_chat 前置注入 system、后置打印响应字数（同一闭包）
    K2 = beamai:add_filter(K1, <<"system_audit">>, #{
        around_chat => fun(#{messages := Msgs} = Req, _FCtx, Next) ->
            HasSystem = lists:any(fun(#{role := R}) -> R =:= system; (_) -> false end, Msgs),
            Req1 = case HasSystem of
                true ->
                    Req;
                false ->
                    SystemMsg = #{role => system, content => <<"请用一句话简洁回答。"/utf8>>},
                    io:format("  [前置] injected system message~n"),
                    Req#{messages => [SystemMsg | Msgs]}
            end,
            #{response := Response} = Resp = Next(Req1),
            Content = beamai_llm_response:content(Response),
            case is_binary(Content) of
                true -> io:format("  [后置] response length: ~B bytes~n", [byte_size(Content)]);
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
