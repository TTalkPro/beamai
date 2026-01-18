%%%-------------------------------------------------------------------
%%% @doc beamai_tool 模块的单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_tests).
-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% simple_tool/3 测试
%%====================================================================

simple_tool_auto_required_test() ->
    Name = <<"test_tool">>,
    Desc = <<"Test description">>,
    Props = #{
        <<"param1">> => #{type => string, description => <<"Param 1">>},
        <<"param2">> => #{type => string, description => <<"Param 2">>}
    },
    Result = beamai_tool:simple_tool(Name, Desc, Props),
    ?assertEqual(function, maps:get(type, Result)),
    ?assertEqual(Name, maps:get(name, maps:get(function, Result))),
    ?assertEqual(Desc, maps:get(description, maps:get(function, Result))),
    ?assertEqual([<<"param1">>, <<"param2">>], maps:get(required, maps:get(parameters, maps:get(function, Result)))).

simple_tool_explicit_required_test() ->
    Name = <<"test_tool">>,
    Desc = <<"Test description">>,
    Props = #{
        <<"param1">> => #{type => string, description => <<"Param 1">>},
        <<"param2">> => #{type => string, description => <<"Param 2">>}
    },
    Result = beamai_tool:simple_tool(Name, Desc, Props, [<<"param1">>]),
    ?assertEqual([<<"param1">>], maps:get(required, maps:get(parameters, maps:get(function, Result)))).

%%====================================================================
%% delegation_tool/1 测试
%%====================================================================

delegation_tool_test() ->
    Name = <<"researcher">>,
    Result = beamai_tool:delegation_tool(Name),
    ?assertEqual(function, maps:get(type, Result)),
    ?assertEqual(<<"delegate_to_researcher">>, maps:get(name, maps:get(function, Result))),
    ?assertEqual(<<"Delegate task to researcher">>, maps:get(description, maps:get(function, Result))),
    ?assertEqual([<<"task">>], maps:get(required, maps:get(parameters, maps:get(function, Result)))).

%%====================================================================
%% string_param/1,2 测试
%%====================================================================

string_param_default_test() ->
    Result = beamai_tool:string_param(<<"test_param">>),
    ?assertEqual(#{<<"test_param">> => #{type => string, description => <<"Parameter">>}}, Result).

string_param_with_desc_test() ->
    Result = beamai_tool:string_param(<<"test_param">>, <<"Test description">>),
    ?assertEqual(#{<<"test_param">> => #{type => string, description => <<"Test description">>}}, Result).

%%====================================================================
%% string_params/1 测试
%%====================================================================

string_params_test() ->
    Params = [
        {<<"param1">>, <<"First param">>},
        {<<"param2">>, <<"Second param">>}
    ],
    Result = beamai_tool:string_params(Params),
    ?assertEqual(2, map_size(Result)),
    ?assertEqual(#{type => string, description => <<"First param">>}, maps:get(<<"param1">>, Result)),
    ?assertEqual(#{type => string, description => <<"Second param">>}, maps:get(<<"param2">>, Result)).

%%====================================================================
%% int_param/1,2 测试
%%====================================================================

int_param_default_test() ->
    Result = beamai_tool:int_param(<<"count">>),
    ?assertEqual(#{<<"count">> => #{type => integer, description => <<"Integer parameter">>}}, Result).

int_param_with_desc_test() ->
    Result = beamai_tool:int_param(<<"count">>, <<"Item count">>),
    ?assertEqual(#{<<"count">> => #{type => integer, description => <<"Item count">>}}, Result).

int_params_test() ->
    Params = [
        {<<"limit">>, <<"Max items">>},
        {<<"offset">>, <<"Start position">>}
    ],
    Result = beamai_tool:int_params(Params),
    ?assertEqual(2, map_size(Result)),
    ?assertEqual(#{type => integer, description => <<"Max items">>}, maps:get(<<"limit">>, Result)),
    ?assertEqual(#{type => integer, description => <<"Start position">>}, maps:get(<<"offset">>, Result)).

%%====================================================================
%% array_param/2 测试
%%====================================================================

array_param_test() ->
    Result = beamai_tool:array_param(<<"items">>, <<"string">>),
    Expected = #{<<"items">> => #{
        type => array,
        description => <<"Array of strings">>,
        items => #{type => <<"string">>}
    }},
    ?assertEqual(Expected, Result).

%%====================================================================
%% object_array_param/2 测试
%%====================================================================

object_array_param_test() ->
    ObjectProps = #{
        <<"name">> => #{type => string},
        <<"age">> => #{type => integer}
    },
    Result = beamai_tool:object_array_param(<<"users">>, ObjectProps),
    Expected = #{<<"users">> => #{
        type => array,
        description => <<"Array of objects">>,
        items => #{
            type => object,
            properties => ObjectProps
        }
    }},
    ?assertEqual(Expected, Result).

%%====================================================================
%% mixed_params/1 测试
%%====================================================================

mixed_params_test() ->
    Params = [
        {<<"name">>, string, <<"User name">>},
        {<<"age">>, integer, <<"User age">>},
        {<<"active">>, boolean, <<"Is active">>},
        {<<"score">>, number, <<"Score value">>}
    ],
    Result = beamai_tool:mixed_params(Params),
    ?assertEqual(4, map_size(Result)),
    ?assertEqual(#{type => string, description => <<"User name">>}, maps:get(<<"name">>, Result)),
    ?assertEqual(#{type => integer, description => <<"User age">>}, maps:get(<<"age">>, Result)),
    ?assertEqual(#{type => boolean, description => <<"Is active">>}, maps:get(<<"active">>, Result)),
    ?assertEqual(#{type => number, description => <<"Score value">>}, maps:get(<<"score">>, Result)).

%%====================================================================
%% 集成测试（已移除依赖未实现模块的测试）
%%====================================================================
