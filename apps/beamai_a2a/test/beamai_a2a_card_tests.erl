%%%-------------------------------------------------------------------
%%% @doc beamai_a2a_card 模块单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_card_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% Agent Card 生成测试
%%====================================================================

generate_basic_card_test() ->
    Config = #{
        name => <<"test-agent">>,
        description => <<"A test agent">>,
        url => <<"https://example.com/a2a">>
    },
    {ok, Card} = beamai_a2a_card:generate(Config),

    ?assertEqual(<<"test-agent">>, maps:get(name, Card)),
    ?assertEqual(<<"A test agent">>, maps:get(description, Card)),
    ?assertEqual(<<"https://example.com/a2a">>, maps:get(url, Card)),
    ?assertEqual(<<"0.3.0">>, maps:get(protocol_version, Card)),
    ?assertEqual([], maps:get(skills, Card)).

generate_card_with_tools_test() ->
    Tools = [
        #{
            name => <<"check_stock">>,
            description => <<"Check inventory stock levels">>
        },
        #{
            name => <<"reserve_items">>,
            description => <<"Reserve items for an order">>
        }
    ],
    Config = #{
        name => <<"inventory-agent">>,
        description => <<"Inventory management agent">>,
        url => <<"https://inventory.example.com/a2a">>,
        tools => Tools
    },
    {ok, Card} = beamai_a2a_card:generate(Config),

    Skills = maps:get(skills, Card),
    ?assertEqual(2, length(Skills)),

    [Skill1, Skill2] = Skills,
    ?assertEqual(<<"check_stock">>, maps:get(id, Skill1)),
    ?assertEqual(<<"Check Stock">>, maps:get(name, Skill1)),
    ?assertEqual(<<"reserve_items">>, maps:get(id, Skill2)),
    ?assertEqual(<<"Reserve Items">>, maps:get(name, Skill2)).

generate_card_missing_required_field_test() ->
    %% 缺少 name
    Config1 = #{
        description => <<"A test agent">>,
        url => <<"https://example.com/a2a">>
    },
    ?assertMatch({error, {missing_required_field, name}}, beamai_a2a_card:generate(Config1)),

    %% 缺少 description
    Config2 = #{
        name => <<"test-agent">>,
        url => <<"https://example.com/a2a">>
    },
    ?assertMatch({error, {missing_required_field, description}}, beamai_a2a_card:generate(Config2)),

    %% 缺少 url
    Config3 = #{
        name => <<"test-agent">>,
        description => <<"A test agent">>
    },
    ?assertMatch({error, {missing_required_field, url}}, beamai_a2a_card:generate(Config3)).

%%====================================================================
%% JSON 序列化测试
%%====================================================================

to_json_test() ->
    Config = #{
        name => <<"test-agent">>,
        description => <<"A test agent">>,
        url => <<"https://example.com/a2a">>
    },
    {ok, Card} = beamai_a2a_card:generate(Config),

    JsonBin = beamai_a2a_card:to_json(Card),
    ?assert(is_binary(JsonBin)),

    %% 解析 JSON 验证格式
    JsonMap = jsx:decode(JsonBin, [return_maps]),
    ?assertEqual(<<"test-agent">>, maps:get(<<"name">>, JsonMap)),
    ?assertEqual(<<"A test agent">>, maps:get(<<"description">>, JsonMap)),
    ?assertEqual(<<"https://example.com/a2a">>, maps:get(<<"url">>, JsonMap)),
    ?assertEqual(<<"0.3.0">>, maps:get(<<"protocolVersion">>, JsonMap)).

from_json_test() ->
    JsonBin = jsx:encode(#{
        <<"name">> => <<"test-agent">>,
        <<"description">> => <<"A test agent">>,
        <<"url">> => <<"https://example.com/a2a">>,
        <<"protocolVersion">> => <<"0.3.0">>,
        <<"skills">> => []
    }, []),

    {ok, Card} = beamai_a2a_card:from_json(JsonBin),

    ?assertEqual(<<"test-agent">>, maps:get(name, Card)),
    ?assertEqual(<<"A test agent">>, maps:get(description, Card)),
    ?assertEqual(<<"https://example.com/a2a">>, maps:get(url, Card)).

roundtrip_test() ->
    Config = #{
        name => <<"roundtrip-agent">>,
        description => <<"Test roundtrip serialization">>,
        url => <<"https://example.com/a2a">>,
        tools => [
            #{name => <<"tool1">>, description => <<"Tool 1">>}
        ]
    },
    {ok, Card1} = beamai_a2a_card:generate(Config),

    JsonBin = beamai_a2a_card:to_json(Card1),
    {ok, Card2} = beamai_a2a_card:from_json(JsonBin),

    ?assertEqual(maps:get(name, Card1), maps:get(name, Card2)),
    ?assertEqual(maps:get(description, Card1), maps:get(description, Card2)),
    ?assertEqual(maps:get(url, Card1), maps:get(url, Card2)),
    ?assertEqual(length(maps:get(skills, Card1)), length(maps:get(skills, Card2))).

%%====================================================================
%% Tools 转换测试
%%====================================================================

tools_to_skills_test() ->
    Tools = [
        #{name => <<"get_weather">>, description => <<"Get weather information">>},
        #{name => <<"search_flights">>, description => <<"Search for available flights">>}
    ],

    Skills = beamai_a2a_card:tools_to_skills(Tools),

    ?assertEqual(2, length(Skills)),

    [S1, S2] = Skills,
    ?assertEqual(<<"get_weather">>, maps:get(id, S1)),
    ?assertEqual(<<"Get Weather">>, maps:get(name, S1)),
    ?assertEqual(<<"search_flights">>, maps:get(id, S2)),
    ?assertEqual(<<"Search Flights">>, maps:get(name, S2)).

tools_to_skills_empty_test() ->
    ?assertEqual([], beamai_a2a_card:tools_to_skills([])).

%%====================================================================
%% 验证测试
%%====================================================================

validate_valid_card_test() ->
    Config = #{
        name => <<"valid-agent">>,
        description => <<"A valid agent">>,
        url => <<"https://example.com/a2a">>
    },
    {ok, Card} = beamai_a2a_card:generate(Config),
    ?assertEqual(ok, beamai_a2a_card:validate(Card)).

validate_missing_name_test() ->
    Card = #{
        description => <<"A test agent">>,
        url => <<"https://example.com/a2a">>
    },
    ?assertMatch({error, {missing_required_field, name}}, beamai_a2a_card:validate(Card)).
