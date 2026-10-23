%%%-------------------------------------------------------------------
%%% @doc 消息适配单元测试
%%%-------------------------------------------------------------------
-module(beamai_llm_message_adapter_tests).

-include_lib("eunit/include/eunit.hrl").

%% Anthropic 回放「文本 + 工具调用」时，文本不能丢失。
anthropic_text_with_tool_calls_test() ->
    Msg = #{role => assistant,
            content => <<"I found it.">>,
            tool_calls => [#{id => <<"c1">>, name => <<"t">>, arguments => #{}}]},
    [AnthropicMsg] = beamai_llm_message_adapter:to_anthropic([Msg]),
    ?assertEqual(#{<<"role">> => <<"assistant">>,
                   <<"content">> => [
                       #{<<"type">> => <<"text">>, <<"text">> => <<"I found it.">>},
                       #{<<"type">> => <<"tool_use">>, <<"id">> => <<"c1">>,
                         <<"name">> => <<"t">>, <<"input">> => #{}
                       }]},
                 AnthropicMsg).

%% 纯工具回合（content null/空）不应插入空 text 块。
anthropic_null_text_with_tool_calls_test() ->
    Msg = #{role => assistant,
            content => null,
            tool_calls => [#{id => <<"c1">>, name => <<"t">>, arguments => #{}}]},
    [AnthropicMsg] = beamai_llm_message_adapter:to_anthropic([Msg]),
    Blocks = maps:get(<<"content">>, AnthropicMsg),
    ?assertMatch([#{<<"type">> := <<"tool_use">>}], Blocks).
