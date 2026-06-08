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
