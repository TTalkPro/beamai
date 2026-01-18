%%%-------------------------------------------------------------------
%%% @doc Output Parser 测试
%%% @end
%%%-------------------------------------------------------------------
-module(output_parser_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% JSON Parser 测试
%%====================================================================

%% @doc 测试简单 JSON 解析
simple_json_test() ->
    Text = <<"{\"name\": \"John\", \"age\": 30}">>,
    Parser = beamai_output_parser:json(),
    {ok, Result} = beamai_output_parser:parse(Parser, Text),
    ?assertEqual(<<"John">>, maps:get(<<"name">>, Result)),
    ?assertEqual(30, maps:get(<<"age">>, Result)).

%% @doc 测试从 markdown 代码块提取 JSON
extract_json_codeblock_test() ->
    Text = <<"```json\n{\"key\": \"value\"}\n```">>,
    Parser = beamai_output_parser:json(),
    {ok, Result} = beamai_output_parser:parse(Parser, Text),
    ?assertEqual(<<"value">>, maps:get(<<"key">>, Result)).

%% @doc 测试修复尾随逗号
repair_trailing_comma_test() ->
    Text = <<"{\"name\": \"John\", \"age\": 30,}">>,
    Parser = beamai_output_parser:json(#{repair_common => true}),
    {ok, Result} = beamai_output_parser:parse(Parser, Text),
    ?assertEqual(<<"John">>, maps:get(<<"name">>, Result)).

%% @doc 测试从混合文本中提取 JSON
extract_from_mixed_text_test() ->
    Text = <<"Here's the result: {\"status\": \"success\", \"data\": [1, 2, 3]}">>,
    Parser = beamai_output_parser:json(#{extract_codeblock => true}),
    {ok, Result} = beamai_output_parser:parse(Parser, Text),
    ?assertEqual(<<"success">>, maps:get(<<"status">>, Result)).

%% @doc 测试嵌套 JSON 解析
nested_json_test() ->
    Text = <<"{\"user\": {\"name\": \"Alice\", \"address\": {\"city\": \"NYC\"}}}">>,
    Parser = beamai_output_parser:json(),
    {ok, Result} = beamai_output_parser:parse(Parser, Text),
    User = maps:get(<<"user">>, Result),
    ?assertEqual(<<"Alice">>, maps:get(<<"name">>, User)),
    Address = maps:get(<<"address">>, User),
    ?assertEqual(<<"NYC">>, maps:get(<<"city">>, Address)).

%% @doc 测试 JSON 数组解析
json_array_test() ->
    Text = <<"[{\"id\": 1}, {\"id\": 2}, {\"id\": 3}]">>,
    Parser = beamai_output_parser:json(),
    {ok, Result} = beamai_output_parser:parse(Parser, Text),
    ?assertEqual(3, length(Result)),
    ?assertEqual(1, maps:get(<<"id">>, lists:nth(1, Result))).

%%====================================================================
%% 格式指令测试
%%====================================================================

%% @doc 测试 JSON 格式指令生成
json_instructions_test() ->
    Instructions = beamai_output_parser:get_instructions(json),
    ?assert(is_binary(Instructions)),
    ?assert(binary:match(Instructions, <<"JSON">>) =/= nomatch).

%% @doc 测试带 Schema 的 JSON 指令
json_with_schema_instructions_test() ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{
                <<"type">> => <<"string">>,
                <<"description">> => <<"Person's name">>
            }
        },
        <<"required">> => [<<"name">>]
    },
    Instructions = beamai_output_parser:get_instructions(json, #{schema => Schema}),
    ?assert(binary:match(Instructions, <<"name">>) =/= nomatch),
    ?assert(binary:match(Instructions, <<"string">>) =/= nomatch).

%%====================================================================
%% 重试机制测试
%%====================================================================

%% @doc 测试重试解析
retry_parse_test() ->
    Parser = beamai_output_parser:json(),
    %% 第一次会失败，然后重试
    BadText = <<"{invalid json}">>,
    Result = beamai_output_parser:parse_with_retry(Parser, BadText, 2),
    ?assertMatch({error, {max_retries_exceeded, _}}, Result).

%% @doc 测试带回调的重试
retry_with_callback_test() ->
    Parser = beamai_output_parser:json(),
    Callback = fun(_Error, Attempt) ->
        ?assert(Attempt > 0)
    end,
    BadText = <<"{invalid}">>,
    beamai_output_parser:parse_with_retry(
        Parser,
        BadText,
        2,
        #{on_retry => Callback}
    ).

%%====================================================================
%% 边界情况测试
%%====================================================================

%% @doc 测试空字符串
empty_string_test() ->
    Parser = beamai_output_parser:json(),
    Text = <<"">>,
    Result = beamai_output_parser:parse(Parser, Text),
    ?assertMatch({error, _}, Result).

%% @doc 测试仅有空白
whitespace_only_test() ->
    Parser = beamai_output_parser:json(),
    Text = <<"   \n\t   ">>,
    Result = beamai_output_parser:parse(Parser, Text),
    ?assertMatch({error, _}, Result).

%% @doc 测试未转义的换行符
unescaped_newline_test() ->
    Text = <<"{\"text\": \"line1\\nline2\"}">>,
    Parser = beamai_output_parser:json(),
    {ok, Result} = beamai_output_parser:parse(Parser, Text),
    ?assertEqual(<<"line1\nline2">>, maps:get(<<"text">>, Result)).

%% @doc 测试 Unicode 字符
unicode_test() ->
    Text = <<"{\"emoji\": \"😀\", \"chinese\": \"你好\"}">>,
    Parser = beamai_output_parser:json(),
    {ok, Result} = beamai_output_parser:parse(Parser, Text),
    ?assertEqual(<<"😀">>, maps:get(<<"emoji">>, Result)),
    ?assertEqual(<<"你好">>, maps:get(<<"chinese">>, Result)).
