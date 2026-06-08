%%%-------------------------------------------------------------------
%%% @doc Anthropic 引用（Citations）响应解析单元测试
%%%
%%% 覆盖 from_anthropic 对 text block 内 citations 数组的提取。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_anthropic_advanced_tests).

-include_lib("eunit/include/eunit.hrl").

citations_extracted_to_metadata_test() ->
    Citation = #{
        <<"type">> => <<"char_location">>,
        <<"cited_text">> => <<"地球是圆的"/utf8>>,
        <<"document_index">> => 0,
        <<"start_char_index">> => 10,
        <<"end_char_index">> => 20
    },
    Raw = #{
        <<"id">> => <<"msg-1">>,
        <<"model">> => <<"claude-sonnet-4-5">>,
        <<"type">> => <<"message">>,
        <<"role">> => <<"assistant">>,
        <<"content">> => [
            #{<<"type">> => <<"text">>, <<"text">> => <<"据资料，"/utf8>>},
            #{<<"type">> => <<"text">>, <<"text">> => <<"地球是圆的。"/utf8>>,
              <<"citations">> => [Citation]}
        ],
        <<"stop_reason">> => <<"end_turn">>,
        <<"usage">> => #{<<"input_tokens">> => 10, <<"output_tokens">> => 5}
    },
    {ok, Resp} = beamai_llm_response_parser:from_anthropic(Raw),
    Meta = beamai_llm_response:metadata(Resp),
    ?assertEqual([Citation], maps:get(citations, Meta)),
    %% 文本仍正常拼接
    ?assertEqual(<<"据资料，地球是圆的。"/utf8>>, beamai_llm_response:content(Resp)).

no_citations_no_metadata_key_test() ->
    Raw = #{
        <<"id">> => <<"msg-2">>,
        <<"model">> => <<"claude-sonnet-4-5">>,
        <<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => <<"普通回复"/utf8>>}],
        <<"stop_reason">> => <<"end_turn">>,
        <<"usage">> => #{<<"input_tokens">> => 3, <<"output_tokens">> => 2}
    },
    {ok, Resp} = beamai_llm_response_parser:from_anthropic(Raw),
    ?assertNot(maps:is_key(citations, beamai_llm_response:metadata(Resp))).

%%====================================================================
%% Web Search 响应侧解析
%%====================================================================

web_search_results_extracted_test() ->
    Result = #{
        <<"type">> => <<"web_search_result">>,
        <<"title">> => <<"Erlang">>,
        <<"url">> => <<"https://erlang.org">>,
        <<"page_age">> => <<"2024">>
    },
    Raw = #{
        <<"id">> => <<"msg-ws">>,
        <<"model">> => <<"claude-sonnet-4-5">>,
        <<"content">> => [
            #{<<"type">> => <<"server_tool_use">>, <<"id">> => <<"srv_1">>,
              <<"name">> => <<"web_search">>, <<"input">> => #{<<"query">> => <<"erlang">>}},
            #{<<"type">> => <<"web_search_tool_result">>, <<"tool_use_id">> => <<"srv_1">>,
              <<"content">> => [Result]},
            #{<<"type">> => <<"text">>, <<"text">> => <<"Erlang 是一门语言"/utf8>>}
        ],
        <<"stop_reason">> => <<"end_turn">>,
        <<"usage">> => #{<<"input_tokens">> => 10, <<"output_tokens">> => 5,
                         <<"server_tool_use">> => #{<<"web_search_requests">> => 1}}
    },
    {ok, Resp} = beamai_llm_response_parser:from_anthropic(Raw),
    Meta = beamai_llm_response:metadata(Resp),
    ?assertEqual([Result], maps:get(web_search_results, Meta)),
    %% 文本正常提取（server_tool_use / web_search_tool_result 块不混入正文）
    ?assertEqual(<<"Erlang 是一门语言"/utf8>>, beamai_llm_response:content(Resp)),
    %% server_tool_use 用量进入 usage.details
    Details = maps:get(details, beamai_llm_response:usage(Resp)),
    ?assertEqual(#{<<"web_search_requests">> => 1}, maps:get(server_tool_use, Details)).

no_web_search_no_metadata_key_test() ->
    Raw = #{
        <<"id">> => <<"msg-3">>,
        <<"model">> => <<"claude-sonnet-4-5">>,
        <<"content">> => [#{<<"type">> => <<"text">>, <<"text">> => <<"hi">>}],
        <<"stop_reason">> => <<"end_turn">>,
        <<"usage">> => #{<<"input_tokens">> => 1, <<"output_tokens">> => 1}
    },
    {ok, Resp} = beamai_llm_response_parser:from_anthropic(Raw),
    ?assertNot(maps:is_key(web_search_results, beamai_llm_response:metadata(Resp))).
