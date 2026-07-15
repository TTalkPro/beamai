%%%-------------------------------------------------------------------
%%% @doc JSON Schema 子集校验器测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_json_schema_tests).

-include_lib("eunit/include/eunit.hrl").

%% 断言校验失败并取出错误列表
errs(Schema, Instance) ->
    {error, Errors} = beamai_json_schema:validate(Schema, Instance),
    Errors.

%% 取错误的 {path, keyword} 对（避免跨文件比对中文消息字节）
kws(Schema, Instance) ->
    [{P, K} || #{path := P, keyword := K} <- errs(Schema, Instance)].

%%====================================================================
%% type
%%====================================================================

%% type：binary key + binary 值
type_binary_key_test() ->
    ?assertEqual(ok, beamai_json_schema:validate(#{<<"type">> => <<"string">>}, <<"hi">>)),
    ?assertEqual([{<<"">>, type}], kws(#{<<"type">> => <<"string">>}, 1)).

%% type：atom key + atom 值（beamai_tool:to_tool_spec/1 生成的形态）
type_atom_key_test() ->
    ?assertEqual(ok, beamai_json_schema:validate(#{type => string}, <<"hi">>)),
    ?assertEqual([{<<"">>, type}], kws(#{type => string}, 1)).

%% type：各基础类型正例
type_all_kinds_test() ->
    ?assertEqual(ok, beamai_json_schema:validate(#{type => object}, #{})),
    ?assertEqual(ok, beamai_json_schema:validate(#{type => array}, [])),
    ?assertEqual(ok, beamai_json_schema:validate(#{type => boolean}, true)),
    ?assertEqual(ok, beamai_json_schema:validate(#{type => null}, null)),
    ?assertEqual(ok, beamai_json_schema:validate(#{type => number}, 1.5)).

%% type：布尔不是 number，null 不是 object
type_negatives_test() ->
    ?assertMatch({error, _}, beamai_json_schema:validate(#{type => number}, true)),
    ?assertMatch({error, _}, beamai_json_schema:validate(#{type => object}, null)),
    ?assertMatch({error, _}, beamai_json_schema:validate(#{type => array}, #{})).

%% type：整数同时满足 integer 与 number
type_integer_is_number_test() ->
    ?assertEqual(ok, beamai_json_schema:validate(#{type => integer}, 3)),
    ?assertEqual(ok, beamai_json_schema:validate(#{type => number}, 3)).

%% type：零小数浮点算 integer（DRAFT 2020-12），带小数则不算
type_float_integral_test() ->
    ?assertEqual(ok, beamai_json_schema:validate(#{type => integer}, 1.0)),
    ?assertEqual(ok, beamai_json_schema:validate(#{type => number}, 1.5)),
    ?assertEqual([{<<"">>, type}], kws(#{type => integer}, 1.5)).

%% type：联合类型列表
type_union_test() ->
    S = #{type => [string, null]},
    ?assertEqual(ok, beamai_json_schema:validate(S, <<"a">>)),
    ?assertEqual(ok, beamai_json_schema:validate(S, null)),
    ?assertMatch({error, _}, beamai_json_schema:validate(S, 1)).

%% nullable：在 type 之外额外放行 null
nullable_test() ->
    S = #{type => string, nullable => true},
    ?assertEqual(ok, beamai_json_schema:validate(S, null)),
    ?assertEqual(ok, beamai_json_schema:validate(S, <<"a">>)),
    ?assertMatch({error, _}, beamai_json_schema:validate(#{type => string}, null)).

%%====================================================================
%% enum / const
%%====================================================================

%% enum：命中与未命中
enum_test() ->
    S = #{enum => [<<"a">>, <<"b">>]},
    ?assertEqual(ok, beamai_json_schema:validate(S, <<"b">>)),
    ?assertEqual([{<<"">>, enum}], kws(S, <<"c">>)).

%% enum：atom 枚举值视同其文本（param spec 常见写法）
enum_atom_values_test() ->
    ?assertEqual(ok, beamai_json_schema:validate(#{enum => [celsius, fahrenheit]}, <<"celsius">>)),
    ?assertMatch({error, _}, beamai_json_schema:validate(#{enum => [celsius]}, <<"kelvin">>)).

%% const：JSON 相等（1 与 1.0 相等）
const_test() ->
    ?assertEqual(ok, beamai_json_schema:validate(#{const => 1}, 1.0)),
    ?assertEqual(ok, beamai_json_schema:validate(#{const => <<"x">>}, <<"x">>)),
    ?assertEqual([{<<"">>, const}], kws(#{const => <<"x">>}, <<"y">>)).

%%====================================================================
%% 数值
%%====================================================================

%% minimum / maximum：含端点
min_max_test() ->
    S = #{type => number, minimum => 1, maximum => 10},
    ?assertEqual(ok, beamai_json_schema:validate(S, 1)),
    ?assertEqual(ok, beamai_json_schema:validate(S, 10)),
    ?assertEqual([{<<"">>, minimum}], kws(S, 0)),
    ?assertEqual([{<<"">>, maximum}], kws(S, 11)).

%% exclusiveMinimum / exclusiveMaximum：不含端点
exclusive_bounds_test() ->
    S = #{exclusiveMinimum => 1, exclusiveMaximum => 10},
    ?assertEqual(ok, beamai_json_schema:validate(S, 5)),
    ?assertEqual([{<<"">>, exclusiveMinimum}], kws(S, 1)),
    ?assertEqual([{<<"">>, exclusiveMaximum}], kws(S, 10)).

%% multipleOf：整数走 rem，浮点走商
multiple_of_test() ->
    ?assertEqual(ok, beamai_json_schema:validate(#{multipleOf => 3}, 9)),
    ?assertEqual([{<<"">>, multipleOf}], kws(#{multipleOf => 3}, 10)),
    ?assertEqual(ok, beamai_json_schema:validate(#{multipleOf => 0.5}, 1.5)).

%% 数值关键字对非数值实例不生效（由 type 负责报错）
numeric_skips_non_number_test() ->
    ?assertEqual(ok, beamai_json_schema:validate(#{minimum => 5}, <<"str">>)).

%%====================================================================
%% 字符串
%%====================================================================

%% minLength / maxLength：按码点计（中文 1 字符 3 字节）
length_counts_codepoints_test() ->
    ?assertEqual(ok, beamai_json_schema:validate(#{maxLength => 2}, <<"你好"/utf8>>)),
    ?assertEqual([{<<"">>, maxLength}], kws(#{maxLength => 1}, <<"你好"/utf8>>)),
    ?assertEqual([{<<"">>, minLength}], kws(#{minLength => 3}, <<"ab">>)).

%% pattern：匹配与不匹配
pattern_test() ->
    S = #{pattern => <<"^[a-z]+$">>},
    ?assertEqual(ok, beamai_json_schema:validate(S, <<"abc">>)),
    ?assertEqual([{<<"">>, pattern}], kws(S, <<"Abc1">>)).

%%====================================================================
%% 数组
%%====================================================================

%% minItems / maxItems
items_count_test() ->
    S = #{type => array, minItems => 1, maxItems => 2},
    ?assertEqual(ok, beamai_json_schema:validate(S, [1])),
    ?assertEqual([{<<"">>, minItems}], kws(S, [])),
    ?assertEqual([{<<"">>, maxItems}], kws(S, [1, 2, 3])).

%% uniqueItems：1 与 1.0 视作重复
unique_items_test() ->
    S = #{uniqueItems => true},
    ?assertEqual(ok, beamai_json_schema:validate(S, [1, 2, 3])),
    ?assertEqual([{<<"">>, uniqueItems}], kws(S, [1, 1])),
    ?assertEqual([{<<"">>, uniqueItems}], kws(S, [1, 1.0])).

%% items：子 schema 施加于每个元素，路径带下标
items_schema_paths_test() ->
    S = #{type => array, items => #{type => string}},
    ?assertEqual(ok, beamai_json_schema:validate(S, [<<"a">>, <<"b">>])),
    ?assertEqual([{<<"/1">>, type}, {<<"/2">>, type}], kws(S, [<<"a">>, 1, true])).

%%====================================================================
%% 对象
%%====================================================================

%% required：缺失属性报错（名字 atom / binary 皆可）
required_test() ->
    ?assertEqual([{<<"">>, required}], kws(#{required => [<<"a">>]}, #{})),
    ?assertEqual([{<<"">>, required}], kws(#{required => [a]}, #{})),
    ?assertEqual(ok, beamai_json_schema:validate(#{required => [<<"a">>]}, #{<<"a">> => 1})).

%% properties：只校验实例中出现的属性
properties_test() ->
    S = #{type => object, properties => #{<<"n">> => #{type => integer}}},
    ?assertEqual(ok, beamai_json_schema:validate(S, #{})),
    ?assertEqual(ok, beamai_json_schema:validate(S, #{<<"n">> => 1})),
    ?assertEqual([{<<"/n">>, type}], kws(S, #{<<"n">> => <<"x">>})).

%% properties：atom 属性名也能对上实例的 binary key
properties_atom_name_test() ->
    S = #{properties => #{n => #{type => integer}}},
    ?assertEqual([{<<"/n">>, type}], kws(S, #{<<"n">> => <<"x">>})).

%% additionalProperties => false：多余键报错
additional_false_test() ->
    S = #{type => object, properties => #{<<"a">> => #{type => integer}},
          additionalProperties => false},
    ?assertEqual(ok, beamai_json_schema:validate(S, #{<<"a">> => 1})),
    ?assertEqual([{<<"">>, additionalProperties}], kws(S, #{<<"a">> => 1, <<"b">> => 2})).

%% additionalProperties => true / 子 schema
additional_schema_test() ->
    Open = #{properties => #{<<"a">> => #{type => integer}}, additionalProperties => true},
    ?assertEqual(ok, beamai_json_schema:validate(Open, #{<<"b">> => <<"any">>})),
    Typed = #{properties => #{<<"a">> => #{type => integer}},
              additionalProperties => #{type => string}},
    ?assertEqual(ok, beamai_json_schema:validate(Typed, #{<<"b">> => <<"s">>})),
    ?assertEqual([{<<"/b">>, type}], kws(Typed, #{<<"b">> => 1})).

%% 嵌套路径：/items/0/name 形态
nested_path_test() ->
    S = #{type => object,
          properties => #{<<"items">> =>
              #{type => array,
                items => #{type => object,
                           properties => #{<<"name">> => #{type => string}}}}}},
    Instance = #{<<"items">> => [#{<<"name">> => <<"ok">>}, #{<<"name">> => 42}]},
    ?assertEqual([{<<"/items/1/name">>, type}], kws(S, Instance)).

%%====================================================================
%% 组合
%%====================================================================

%% anyOf：命中任一即可
any_of_test() ->
    S = #{anyOf => [#{type => string}, #{type => integer}]},
    ?assertEqual(ok, beamai_json_schema:validate(S, <<"a">>)),
    ?assertEqual(ok, beamai_json_schema:validate(S, 1)),
    ?assertEqual([{<<"">>, anyOf}], kws(S, true)).

%% allOf：分支错误直接并入
all_of_test() ->
    S = #{allOf => [#{type => integer}, #{minimum => 10}]},
    ?assertEqual(ok, beamai_json_schema:validate(S, 11)),
    ?assertEqual([{<<"">>, minimum}], kws(S, 1)).

%% oneOf：须恰好命中 1 个分支
one_of_test() ->
    S = #{oneOf => [#{type => integer}, #{type => string}]},
    ?assertEqual(ok, beamai_json_schema:validate(S, 1)),
    ?assertEqual([{<<"">>, oneOf}], kws(S, true)),
    %% 两分支都命中 → 失败
    Both = #{oneOf => [#{minimum => 0}, #{maximum => 100}]},
    ?assertEqual([{<<"">>, oneOf}], kws(Both, 5)).

%% not：子 schema 必须不通过
not_test() ->
    S = #{'not' => #{type => string}},
    ?assertEqual(ok, beamai_json_schema:validate(S, 1)),
    ?assertEqual([{<<"">>, 'not'}], kws(S, <<"a">>)).

%%====================================================================
%% 布尔 schema / 未知关键字 / 选项
%%====================================================================

%% true schema 恒过，false schema 恒拒
boolean_schema_test() ->
    ?assertEqual(ok, beamai_json_schema:validate(true, #{<<"anything">> => 1})),
    ?assertEqual([{<<"">>, false_schema}], kws(false, 1)),
    %% 作为子 schema 同样生效
    ?assertEqual([{<<"/a">>, false_schema}], kws(#{properties => #{<<"a">> => false}},
                                                 #{<<"a">> => 1})).

%% 未实现/未知关键字静默忽略（$ref、format、patternProperties…）
unknown_keywords_ignored_test() ->
    S = #{type => string, format => <<"email">>, <<"$ref">> => <<"#/$defs/x">>,
          patternProperties => #{<<"^x">> => #{type => integer}},
          <<"weird">> => 1},
    ?assertEqual(ok, beamai_json_schema:validate(S, <<"not-an-email">>)).

%% 多错误：默认收集全部
collects_all_errors_test() ->
    S = #{type => object,
          required => [<<"a">>, <<"b">>],
          properties => #{<<"c">> => #{type => string}, <<"d">> => #{type => string}}},
    ?assertEqual([{<<"">>, required}, {<<"">>, required}, {<<"/c">>, type}, {<<"/d">>, type}],
                 kws(S, #{<<"c">> => 1, <<"d">> => 2})).

%% max_errors：收满即停
max_errors_test() ->
    S = #{required => [<<"a">>, <<"b">>, <<"c">>]},
    {error, Errors} = beamai_json_schema:validate(S, #{}, #{max_errors => 2}),
    ?assertEqual(2, length(Errors)),
    {error, All} = beamai_json_schema:validate(S, #{}),
    ?assertEqual(3, length(All)).

%% 错误结构：三个键齐备
error_shape_test() ->
    [E] = errs(#{type => string}, 1),
    ?assertMatch(#{path := <<"">>, keyword := type, message := _}, E),
    ?assert(is_binary(maps:get(message, E))).

%% error_message：拼成单条 binary，根路径显示为 #
error_message_test() ->
    Msg = beamai_json_schema:error_message(errs(#{type => string}, 1)),
    ?assert(is_binary(Msg)),
    ?assertEqual(<<"#">>, binary:part(Msg, 0, 1)),
    Msg2 = beamai_json_schema:error_message(
             errs(#{properties => #{<<"a">> => #{type => string}}}, #{<<"a">> => 1})),
    ?assertNotEqual(nomatch, binary:match(Msg2, <<"/a: ">>)),
    ?assertEqual(<<"">>, beamai_json_schema:error_message([])).

%%====================================================================
%% 与 beamai_tool 生成的 schema 对接
%%====================================================================

%% beamai_tool:to_tool_spec/1 产出的 schema 可直接用于校验
tool_generated_schema_test() ->
    Tool = beamai_tool:new(<<"get_weather">>, fun(_) -> {ok, <<>>} end,
                           #{parameters => #{
                               city => #{type => string, required => true},
                               days => #{type => integer}}}),
    #{parameters := Schema} = beamai_tool:to_tool_spec(Tool),
    ?assertEqual(ok, beamai_json_schema:validate(Schema, #{<<"city">> => <<"BJ">>, <<"days">> => 3})),
    ?assertEqual([{<<"">>, required}], kws(Schema, #{<<"days">> => 3})),
    ?assertEqual([{<<"/days">>, type}], kws(Schema, #{<<"city">> => <<"BJ">>,
                                                      <<"days">> => <<"3">>})).
