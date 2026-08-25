%%%-------------------------------------------------------------------
%%% @doc JSON Schema 子集校验器（零依赖，纯 Erlang/OTP）
%%%
%%% 用途：校验 LLM 结构化输出（已解码的 JSON term）是否符合给定 schema，
%%% 支撑 Spring AI `StructuredOutputValidationAdvisor` 的对等能力——开箱即用的
%%% 封装是 `beamai_filters:schema_validation_turn_filter/2,3`（本模块是它的校验
%%% 内核，也可单独用）。Spring AI 走 networknt json-schema-validator
%%% （DRAFT 2020-12）；本模块**不引入任何依赖**，只实现 LLM 结构化输出实际会
%%% 用到的实用子集。
%%%
%%% == 双形态 key ==
%%%
%%% schema 同时接受 **binary key**（`<<"type">>`，手写/外部 JSON 来源）和
%%% **atom key**（`type`，本项目 beamai_tool:to_tool_spec/1 生成的形态）；
%%% `type` 的值同样接受 `<<"string">>` 与 `string` 两种写法。
%%% 被校验的**实例**则一律是解码后的 JSON term：map 用 binary key，字符串是
%%% binary，`true`/`false`/`null` 是 atom，数组是 list。
%%%
%%% == 支持的关键字 ==
%%%
%%%   通用：`type`（含类型联合列表）、`enum`、`const`、`nullable`
%%%   对象：`properties`、`required`、`additionalProperties`（boolean 或 schema）
%%%   数组：`items`（单 schema）、`minItems`、`maxItems`、`uniqueItems`
%%%   数值：`minimum`、`maximum`、`exclusiveMinimum`、`exclusiveMaximum`、`multipleOf`
%%%   字符串：`minLength`、`maxLength`（按**码点**计，非字节）、`pattern`（re 模块）
%%%   组合：`anyOf`、`allOf`、`oneOf`、`not`
%%%   整个 schema 写 `true`（恒过）/ `false`（恒拒）
%%%
%%% `integer` vs `number` 按 DRAFT 2020-12：Erlang 整数同时满足两者；浮点满足
%%% `number`，仅当小数部分为零（如 `1.0`）才满足 `integer`。
%%%
%%% == 明确不实现（静默忽略，不报错）==
%%%
%%% `$ref`、`$defs`、`dependentSchemas`、`patternProperties`、`if`/`then`/`else`、
%%% `format`（DRAFT 2020-12 中 format 本就默认为注解而非断言）。未知关键字一律
%%% 按 JSON Schema 语义静默忽略。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_json_schema).

%% API
-export([validate/2, validate/3]).
-export([error_message/1]).

%% Types
-export_type([schema/0, error/0, opts/0]).

%% schema：JSON Schema map（binary 或 atom key），或布尔 schema（恒过/恒拒）
-type schema() :: map() | boolean().

%% error：path 为 JSON Pointer 形态（根为 `<<"">>`，如 `<<"/items/0/name">>`）
-type error() :: #{path := binary(), keyword := atom(), message := binary()}.

%% opts：max_errors 收集到 N 条即停（缺省收集全部）
-type opts() :: #{max_errors => pos_integer()}.

%% 内部：错误上限（pos_integer 或 infinity）
-type limit() :: pos_integer() | infinity.
%% 内部：倒序累积的错误列表
-type acc() :: [error()].

%%====================================================================
%% API
%%====================================================================

%% @doc 校验实例是否符合 schema（收集全部错误）
%%
%% @param Schema JSON Schema（map，binary/atom key 均可；也可直接是 true/false）
%% @param Instance 已解码的 JSON term（map 用 binary key）
%% @returns ok 或 {error, 错误列表}
-spec validate(schema(), term()) -> ok | {error, [error()]}.
validate(Schema, Instance) ->
    validate(Schema, Instance, #{}).

%% @doc 校验实例是否符合 schema（带选项）
%%
%% `#{max_errors => N}` 收集到 N 条错误即短路返回，缺省收集全部。
%%
%% @param Schema JSON Schema
%% @param Instance 已解码的 JSON term
%% @param Opts 选项 map
%% @returns ok 或 {error, 错误列表}
-spec validate(schema(), term(), opts()) -> ok | {error, [error()]}.
validate(Schema, Instance, Opts) ->
    Limit = maps:get(max_errors, Opts, infinity),
    %% check/5 内部以 throw 短路（收满 Limit 即抛），此处兜住
    Rev = try check(Schema, Instance, <<"">>, [], Limit)
          catch throw:{halt, Halted} -> Halted
          end,
    case lists:reverse(Rev) of
        [] -> ok;
        Errors -> {error, Errors}
    end.

%% @doc 把错误列表拼成一条人类可读消息（每条形如 `路径: 说明`，`; ` 分隔）
%%
%% 根路径显示为 `#`。适合直接作为反馈文本回给模型。
%%
%% @param Errors validate/2,3 返回的错误列表
%% @returns 拼接后的 binary
-spec error_message([error()]) -> binary().
error_message(Errors) ->
    Lines = [render_error(E) || E <- Errors],
    beamai_utils:binary_join(<<"; ">>, Lines).

%%====================================================================
%% 内部 - 分发
%%====================================================================

%% @private 校验单个节点：布尔 schema 直接定论，map schema 逐关键字过一遍
-spec check(term(), term(), binary(), acc(), limit()) -> acc().
check(true, _Instance, _Path, Acc, _Limit) ->
    Acc;
check(false, _Instance, Path, Acc, Limit) ->
    add(Path, false_schema, <<"schema 恒为 false，拒绝任何实例"/utf8>>, Acc, Limit);
check(Schema, Instance, Path, Acc0, Limit) when is_map(Schema) ->
    Acc1 = check_type(Schema, Instance, Path, Acc0, Limit),
    Acc2 = check_enum(Schema, Instance, Path, Acc1, Limit),
    Acc3 = check_const(Schema, Instance, Path, Acc2, Limit),
    Acc4 = check_number(Schema, Instance, Path, Acc3, Limit),
    Acc5 = check_string(Schema, Instance, Path, Acc4, Limit),
    Acc6 = check_array(Schema, Instance, Path, Acc5, Limit),
    Acc7 = check_object(Schema, Instance, Path, Acc6, Limit),
    check_combinators(Schema, Instance, Path, Acc7, Limit);
%% 非 map 非布尔：不是 schema，无从断言 → 忽略
check(_Schema, _Instance, _Path, Acc, _Limit) ->
    Acc.

%%====================================================================
%% 内部 - 通用关键字
%%====================================================================

%% @private type：支持单类型 / 类型联合列表；nullable => true 额外放行 null
check_type(Schema, Instance, Path, Acc, Limit) ->
    case sget(type, Schema) of
        error ->
            Acc;
        {ok, Type} ->
            Types = [to_bin(T) || T <- listify(Type)],
            case type_matches_any(Types, Instance) orelse nullable_null(Schema, Instance) of
                true ->
                    Acc;
                false ->
                    Msg = <<"期望类型 "/utf8, (beamai_utils:binary_join(<<"|">>, Types))/binary,
                            "，实际 "/utf8, (type_name(Instance))/binary>>,
                    add(Path, type, Msg, Acc, Limit)
            end
    end.

%% @private nullable => true 时 null 恒合法（部分生成器用它表达可空）
nullable_null(Schema, null) -> sget(nullable, Schema) =:= {ok, true};
nullable_null(_Schema, _Instance) -> false.

%% @private 类型联合：命中任一即可
type_matches_any(Types, Instance) ->
    lists:any(fun(T) -> type_matches(T, Instance) end, Types).

%% @private 单类型匹配（DRAFT 2020-12：零小数的浮点也算 integer）
type_matches(<<"object">>, I) when is_map(I) -> true;
type_matches(<<"array">>, I) when is_list(I) -> true;
type_matches(<<"string">>, I) when is_binary(I) -> true;
type_matches(<<"boolean">>, I) when is_boolean(I) -> true;
type_matches(<<"null">>, null) -> true;
type_matches(<<"number">>, I) when is_number(I) -> true;
type_matches(<<"integer">>, I) when is_integer(I) -> true;
type_matches(<<"integer">>, I) when is_float(I) -> I == trunc(I);
type_matches(_Type, _Instance) -> false.

%% @private enum：实例须与列表中某值 JSON 相等
%%
%% 错误信息**必须列出允许值**：这条反馈会被 schema_validation_turn_filter 原样
%% 喂回模型自纠，只说「不在允许值之内」而不说允许什么，模型无从改起——真实
%% 模型实测下会反复给出近义词（「晴朗」之于「晴」）直到重试耗尽。
check_enum(Schema, Instance, Path, Acc, Limit) ->
    case sget(enum, Schema) of
        {ok, Values} when is_list(Values) ->
            case lists:any(fun(V) -> json_equal(V, Instance) end, Values) of
                true ->
                    Acc;
                false ->
                    Msg = <<"值 "/utf8, (fmt_value(Instance))/binary,
                            " 不在 enum 允许值之内，只能是 "/utf8,
                            (fmt_values(Values))/binary>>,
                    add(Path, enum, Msg, Acc, Limit)
            end;
        _ ->
            Acc
    end.

%% @private const：实例须与常量 JSON 相等
check_const(Schema, Instance, Path, Acc, Limit) ->
    case sget(const, Schema) of
        error ->
            Acc;
        {ok, Const} ->
            case json_equal(Const, Instance) of
                true ->
                    Acc;
                false ->
                    Msg = <<"值 "/utf8, (fmt_value(Instance))/binary,
                            " 与 const 约定值不符，必须是 "/utf8,
                            (fmt_value(Const))/binary>>,
                    add(Path, const, Msg, Acc, Limit)
            end
    end.

%%====================================================================
%% 内部 - 数值关键字
%%====================================================================

%% @private 数值约束（非数值实例跳过，交由 type 报错）
check_number(Schema, Instance, Path, Acc0, Limit) when is_number(Instance) ->
    Acc1 = bound(sget(minimum, Schema), Instance, fun(V, M) -> V >= M end,
                 minimum, <<"小于 minimum"/utf8>>, Path, Acc0, Limit),
    Acc2 = bound(sget(maximum, Schema), Instance, fun(V, M) -> V =< M end,
                 maximum, <<"大于 maximum"/utf8>>, Path, Acc1, Limit),
    Acc3 = bound(sget(exclusiveMinimum, Schema), Instance, fun(V, M) -> V > M end,
                 exclusiveMinimum, <<"不大于 exclusiveMinimum"/utf8>>, Path, Acc2, Limit),
    Acc4 = bound(sget(exclusiveMaximum, Schema), Instance, fun(V, M) -> V < M end,
                 exclusiveMaximum, <<"不小于 exclusiveMaximum"/utf8>>, Path, Acc3, Limit),
    bound(sget(multipleOf, Schema), Instance, fun multiple_of/2,
          multipleOf, <<"不是 multipleOf 的整数倍"/utf8>>, Path, Acc4, Limit);
check_number(_Schema, _Instance, _Path, Acc, _Limit) ->
    Acc.

%% @private 单条数值约束判定：关键字缺失或取值非数值则跳过
bound({ok, Limit0}, Value, Pred, Keyword, Msg, Path, Acc, Limit) when is_number(Limit0) ->
    case Pred(Value, Limit0) of
        true -> Acc;
        false -> add(Path, Keyword, <<Msg/binary, " "/utf8, (num_bin(Limit0))/binary>>, Acc, Limit)
    end;
bound(_Other, _Value, _Pred, _Keyword, _Msg, _Path, Acc, _Limit) ->
    Acc.

%% @private multipleOf：整数走 rem 免浮点误差；含浮点走商是否为整
multiple_of(_Value, Div) when Div == 0 -> false;
multiple_of(Value, Div) when is_integer(Value), is_integer(Div) -> Value rem Div =:= 0;
multiple_of(Value, Div) -> Q = Value / Div, Q == trunc(Q).

%%====================================================================
%% 内部 - 字符串关键字
%%====================================================================

%% @private 字符串约束（非 binary 实例跳过，交由 type 报错）
check_string(Schema, Instance, Path, Acc0, Limit) when is_binary(Instance) ->
    %% 长度按**码点**计（JSON Schema 语义），非字节数
    Len = string:length(Instance),
    Acc1 = len_bound(sget(minLength, Schema), Len, fun(L, M) -> L >= M end,
                     minLength, <<"长度小于 minLength"/utf8>>, Path, Acc0, Limit),
    Acc2 = len_bound(sget(maxLength, Schema), Len, fun(L, M) -> L =< M end,
                     maxLength, <<"长度大于 maxLength"/utf8>>, Path, Acc1, Limit),
    check_pattern(sget(pattern, Schema), Instance, Path, Acc2, Limit);
check_string(_Schema, _Instance, _Path, Acc, _Limit) ->
    Acc.

%% @private 长度约束判定：取值须为非负整数，否则跳过
len_bound({ok, N}, Len, Pred, Keyword, Msg, Path, Acc, Limit) when is_integer(N), N >= 0 ->
    case Pred(Len, N) of
        true -> Acc;
        false -> add(Path, Keyword, <<Msg/binary, " "/utf8, (integer_to_binary(N))/binary>>,
                     Acc, Limit)
    end;
len_bound(_Other, _Len, _Pred, _Keyword, _Msg, _Path, Acc, _Limit) ->
    Acc.

%% @private pattern：每次现编现用（结构化输出实例小，无需缓存 MP）
check_pattern({ok, Pattern}, Str, Path, Acc, Limit) ->
    try re:run(Str, Pattern, [{capture, none}, unicode]) of
        %% 带上 pattern 原文：模型得知道要匹配什么才改得对（同 check_enum 的理由）
        nomatch ->
            add(Path, pattern,
                <<"不匹配 pattern "/utf8, (fmt_value(Pattern))/binary>>, Acc, Limit);
        _Match -> Acc
    catch
        %% 非法正则：不可断言 → 按未知关键字处理，忽略
        error:badarg -> Acc
    end;
check_pattern(_Other, _Str, _Path, Acc, _Limit) ->
    Acc.

%%====================================================================
%% 内部 - 数组关键字
%%====================================================================

%% @private 数组约束（非 list 实例跳过，交由 type 报错）
check_array(Schema, Instance, Path, Acc0, Limit) when is_list(Instance) ->
    Len = length(Instance),
    Acc1 = len_bound(sget(minItems, Schema), Len, fun(L, M) -> L >= M end,
                     minItems, <<"元素数少于 minItems"/utf8>>, Path, Acc0, Limit),
    Acc2 = len_bound(sget(maxItems, Schema), Len, fun(L, M) -> L =< M end,
                     maxItems, <<"元素数多于 maxItems"/utf8>>, Path, Acc1, Limit),
    Acc3 = check_unique(sget(uniqueItems, Schema), Instance, Len, Path, Acc2, Limit),
    check_items(sget(items, Schema), Instance, Path, Acc3, Limit);
check_array(_Schema, _Instance, _Path, Acc, _Limit) ->
    Acc.

%% @private uniqueItems：usort 按 `==` 去重，恰合 JSON 的 1 与 1.0 相等语义
check_unique({ok, true}, Instance, Len, Path, Acc, Limit) ->
    case length(lists:usort(Instance)) =:= Len of
        true -> Acc;
        false -> add(Path, uniqueItems, <<"数组元素存在重复"/utf8>>, Acc, Limit)
    end;
check_unique(_Other, _Instance, _Len, _Path, Acc, _Limit) ->
    Acc.

%% @private items：单 schema 施加于每个元素，路径追加下标
check_items({ok, ItemSchema}, Instance, Path, Acc, Limit) ->
    {_, Acc1} = lists:foldl(
        fun(Element, {Idx, A}) ->
            {Idx + 1, check(ItemSchema, Element, join(Path, integer_to_binary(Idx)), A, Limit)}
        end, {0, Acc}, Instance),
    Acc1;
check_items(error, _Instance, _Path, Acc, _Limit) ->
    Acc.

%%====================================================================
%% 内部 - 对象关键字
%%====================================================================

%% @private 对象约束（非 map 实例跳过，交由 type 报错）
check_object(Schema, Instance, Path, Acc0, Limit) when is_map(Instance) ->
    Acc1 = check_required(sget(required, Schema), Instance, Path, Acc0, Limit),
    Acc2 = check_properties(sget(properties, Schema), Instance, Path, Acc1, Limit),
    check_additional(Schema, Instance, Path, Acc2, Limit);
check_object(_Schema, _Instance, _Path, Acc, _Limit) ->
    Acc.

%% @private required：逐个查键（名字支持 atom / binary 两形）
check_required({ok, Names}, Instance, Path, Acc, Limit) when is_list(Names) ->
    lists:foldl(
        fun(Name, A) ->
            Key = to_bin(Name),
            case maps:is_key(Key, Instance) of
                true -> A;
                false -> add(Path, required, <<"缺少必填属性 "/utf8, Key/binary>>, A, Limit)
            end
        end, Acc, Names);
check_required(_Other, _Instance, _Path, Acc, _Limit) ->
    Acc.

%% @private properties：仅校验实例中实际出现的属性（缺失由 required 管）
check_properties({ok, Props}, Instance, Path, Acc, Limit) when is_map(Props) ->
    %% 按键排序遍历，保证多错误的报告次序稳定
    lists:foldl(
        fun(PropKey, A) ->
            Key = to_bin(PropKey),
            case maps:find(Key, Instance) of
                {ok, Value} ->
                    check(maps:get(PropKey, Props), Value, join(Path, Key), A, Limit);
                error ->
                    A
            end
        end, Acc, lists:sort(maps:keys(Props)));
check_properties(_Other, _Instance, _Path, Acc, _Limit) ->
    Acc.

%% @private additionalProperties：false 则多余键报错；schema 则校验多余键的值
%%
%% 未实现 patternProperties，故"多余"= 不在 properties 中声明的键。
check_additional(Schema, Instance, Path, Acc, Limit) ->
    case sget(additionalProperties, Schema) of
        error ->
            Acc;
        {ok, true} ->
            Acc;
        {ok, AP} ->
            Declared = declared_props(Schema),
            Extra = lists:sort([K || K <- maps:keys(Instance), not lists:member(K, Declared)]),
            lists:foldl(
                fun(Key, A) -> additional_one(AP, Key, Instance, Path, A, Limit) end,
                Acc, Extra)
    end.

%% @private 单个多余键：false → 报错；否则按子 schema 校验
additional_one(false, Key, _Instance, Path, Acc, Limit) ->
    add(Path, additionalProperties, <<"不允许的额外属性 "/utf8, Key/binary>>, Acc, Limit);
additional_one(APSchema, Key, Instance, Path, Acc, Limit) ->
    check(APSchema, maps:get(Key, Instance), join(Path, Key), Acc, Limit).

%% @private properties 中声明的属性名（归一为 binary）
declared_props(Schema) ->
    case sget(properties, Schema) of
        {ok, Props} when is_map(Props) -> [to_bin(K) || K <- maps:keys(Props)];
        _ -> []
    end.

%%====================================================================
%% 内部 - 组合关键字
%%====================================================================

%% @private allOf / anyOf / oneOf / not
check_combinators(Schema, Instance, Path, Acc0, Limit) ->
    Acc1 = check_all_of(sget(allOf, Schema), Instance, Path, Acc0, Limit),
    Acc2 = check_any_of(sget(anyOf, Schema), Instance, Path, Acc1, Limit),
    Acc3 = check_one_of(sget(oneOf, Schema), Instance, Path, Acc2, Limit),
    check_not(sget('not', Schema), Instance, Path, Acc3, Limit).

%% @private allOf：每个分支的错误直接并入（分支错误比"allOf 不过"更有诊断价值）
check_all_of({ok, Schemas}, Instance, Path, Acc, Limit) when is_list(Schemas) ->
    lists:foldl(fun(S, A) -> check(S, Instance, Path, A, Limit) end, Acc, Schemas);
check_all_of(_Other, _Instance, _Path, Acc, _Limit) ->
    Acc.

%% @private anyOf：至少一个分支通过
check_any_of({ok, Schemas}, Instance, Path, Acc, Limit) when is_list(Schemas) ->
    case lists:any(fun(S) -> valid(S, Instance) end, Schemas) of
        true -> Acc;
        false -> add(Path, anyOf, <<"不满足 anyOf 的任一分支"/utf8>>, Acc, Limit)
    end;
check_any_of(_Other, _Instance, _Path, Acc, _Limit) ->
    Acc.

%% @private oneOf：恰好一个分支通过
check_one_of({ok, Schemas}, Instance, Path, Acc, Limit) when is_list(Schemas) ->
    case length([S || S <- Schemas, valid(S, Instance)]) of
        1 -> Acc;
        N -> add(Path, oneOf,
                 <<"oneOf 须恰好命中 1 个分支，实际命中 "/utf8,
                   (integer_to_binary(N))/binary>>, Acc, Limit)
    end;
check_one_of(_Other, _Instance, _Path, Acc, _Limit) ->
    Acc.

%% @private not：子 schema 必须不通过
check_not({ok, Sub}, Instance, Path, Acc, Limit) ->
    case valid(Sub, Instance) of
        true -> add(Path, 'not', <<"不应满足 not 指定的 schema"/utf8>>, Acc, Limit);
        false -> Acc
    end;
check_not(error, _Instance, _Path, Acc, _Limit) ->
    Acc.

%% @private 组合分支只问"过不过"：上限设 1，首个错误即短路
valid(Schema, Instance) ->
    try check(Schema, Instance, <<"">>, [], 1) of
        [] -> true;
        [_ | _] -> false
    catch
        throw:{halt, _} -> false
    end.

%%====================================================================
%% 内部 - 错误累积
%%====================================================================

%% @private 追加一条错误（倒序累积），收满 Limit 即 throw 短路
-spec add(binary(), atom(), binary(), acc(), limit()) -> acc().
add(Path, Keyword, Message, Acc, Limit) ->
    halt_if_full([#{path => Path, keyword => Keyword, message => Message} | Acc], Limit).

%% @private 达到 max_errors 上限则抛出，由 validate/3 兜住
halt_if_full(Acc, infinity) -> Acc;
halt_if_full(Acc, Limit) when length(Acc) < Limit -> Acc;
halt_if_full(Acc, _Limit) -> throw({halt, Acc}).

%% @private 渲染单条错误为 `路径: 说明`（根路径显示为 #）
render_error(#{path := <<"">>, message := Msg}) -> <<"#: ", Msg/binary>>;
render_error(#{path := Path, message := Msg}) -> <<Path/binary, ": ", Msg/binary>>.

%%====================================================================
%% 内部 - 通用工具
%%====================================================================

%% @private 读 schema 关键字：先 atom key，再 binary key
sget(Key, Schema) ->
    case maps:find(Key, Schema) of
        {ok, _} = Found -> Found;
        error -> maps:find(atom_to_binary(Key, utf8), Schema)
    end.

%% @private 拼 JSON Pointer 路径（根 `<<"">>` + `name` → `<<"/name">>`）
join(Path, Segment) -> <<Path/binary, "/", Segment/binary>>.

%% @private 类型/键名归一为 binary（atom 与 binary 两形皆可）
to_bin(V) when is_binary(V) -> V;
to_bin(V) -> beamai_utils:to_binary(V).

%% @private 单值包成单元素列表（type 支持联合列表）
listify(V) when is_list(V) -> V;
listify(V) -> [V].

%% @private JSON 相等：数值跨整浮比较（1 与 1.0 相等）；
%% 非 true/false/null 的 atom 视同其文本（兼容 param spec 里写 `enum => [celsius]`）
json_equal(A, B) when is_number(A), is_number(B) -> A == B;
json_equal(A, B) when is_atom(A), is_binary(B), A =/= true, A =/= false, A =/= null ->
    atom_to_binary(A, utf8) =:= B;
json_equal(A, B) -> A =:= B.

%% @private 实例的 JSON 类型名（用于错误消息）
type_name(I) when is_map(I) -> <<"object">>;
type_name(I) when is_list(I) -> <<"array">>;
type_name(I) when is_binary(I) -> <<"string">>;
type_name(I) when is_boolean(I) -> <<"boolean">>;
type_name(null) -> <<"null">>;
type_name(I) when is_integer(I) -> <<"integer">>;
type_name(I) when is_float(I) -> <<"number">>;
type_name(_) -> <<"unknown">>.

%% @private 数值转 binary（错误消息用）
num_bin(N) when is_integer(N) -> integer_to_binary(N);
num_bin(N) -> float_to_binary(N, [{decimals, 10}, compact]).

%% @private 值渲染为错误消息里的可读片段（字符串加引号以便与字面量区分）
fmt_value(V) when is_binary(V) -> <<$", V/binary, $">>;
fmt_value(V) when is_integer(V) -> integer_to_binary(V);
fmt_value(V) when is_float(V) -> num_bin(V);
fmt_value(V) when is_atom(V) -> atom_to_binary(V, utf8);
fmt_value(V) -> unicode:characters_to_binary(io_lib:format("~p", [V])).

%% @private 值列表渲染为 `a、b、c`（enum 允许值用）
fmt_values(Values) ->
    iolist_to_binary(lists:join(<<"、"/utf8>>, [fmt_value(V) || V <- Values])).
