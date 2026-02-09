%%%-------------------------------------------------------------------
%%% @doc Pregel 公共工具模块
%%%
%%% 提供各模块共用的工具函数，避免代码重复:
%%% - Map 操作: 计数、过滤、分组
%%% - List 操作: 分组、聚合
%%% - 消息操作: 分组、合并
%%%
%%% 设计原则:
%%% - 函数式风格，无副作用
%%% - 通用性强，可复用于不同场景
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_pregel_utils).

-export([
    %% Map 操作
    map_count/2,
    map_sum/2,
    map_group_by/2,
    map_merge_with/3,

    %% List 操作
    list_group_by/2,
    list_sum_by/2,

    %% 消息操作
    group_messages/1,
    merge_message_groups/2,
    apply_to_groups/2
]).

%%====================================================================
%% Map 操作
%%====================================================================

%% @doc 统计 Map 中满足条件的元素数量
%% 示例: map_count(fun(V) -> V > 0 end, #{a => 1, b => -1}) => 1
-spec map_count(fun((term()) -> boolean()), #{term() => term()}) -> non_neg_integer().
map_count(Pred, Map) ->
    maps:fold(
        fun(_K, V, Acc) ->
            case Pred(V) of
                true -> Acc + 1;
                false -> Acc
            end
        end,
        0,
        Map
    ).

%% @doc 对 Map 中的值应用函数后求和
%% 示例: map_sum(fun(V) -> V * 2 end, #{a => 1, b => 2}) => 6
-spec map_sum(fun((term()) -> number()), #{term() => term()}) -> number().
map_sum(Fun, Map) ->
    maps:fold(fun(_K, V, Acc) -> Acc + Fun(V) end, 0, Map).

%% @doc 按键函数对 Map 的值进行分组
%% 示例: map_group_by(fun(V) -> V rem 2 end, #{a => 1, b => 2, c => 3})
%%       => #{0 => [2], 1 => [1, 3]}
-spec map_group_by(fun((term()) -> term()), #{term() => term()}) ->
    #{term() => [term()]}.
map_group_by(KeyFun, Map) ->
    maps:fold(
        fun(_K, V, Acc) ->
            GroupKey = KeyFun(V),
            Existing = maps:get(GroupKey, Acc, []),
            Acc#{GroupKey => [V | Existing]}
        end,
        #{},
        Map
    ).

%% @doc 合并两个 Map，使用自定义函数处理冲突
%% 示例: map_merge_with(fun(A, B) -> A + B end, #{a => 1}, #{a => 2, b => 3})
%%       => #{a => 3, b => 3}
-spec map_merge_with(fun((term(), term()) -> term()),
                     #{term() => term()},
                     #{term() => term()}) -> #{term() => term()}.
map_merge_with(MergeFun, Map1, Map2) ->
    maps:fold(
        fun(K, V2, Acc) ->
            case maps:find(K, Acc) of
                {ok, V1} -> Acc#{K => MergeFun(V1, V2)};
                error -> Acc#{K => V2}
            end
        end,
        Map1,
        Map2
    ).

%%====================================================================
%% List 操作
%%====================================================================

%% @doc 按键函数对列表元素进行分组
%% 示例: list_group_by(fun({K, _V}) -> K end, [{a, 1}, {b, 2}, {a, 3}])
%%       => #{a => [{a, 1}, {a, 3}], b => [{b, 2}]}
-spec list_group_by(fun((term()) -> term()), [term()]) -> #{term() => [term()]}.
list_group_by(KeyFun, List) ->
    lists:foldl(
        fun(Item, Acc) ->
            Key = KeyFun(Item),
            Existing = maps:get(Key, Acc, []),
            Acc#{Key => [Item | Existing]}
        end,
        #{},
        List
    ).

%% @doc 按键函数分组后对值求和
%% 示例: list_sum_by(fun({K, V}) -> {K, V} end, [{a, 1}, {a, 2}, {b, 3}])
%%       => #{a => 3, b => 3}
-spec list_sum_by(fun((term()) -> {term(), number()}), [term()]) ->
    #{term() => number()}.
list_sum_by(KeyValueFun, List) ->
    lists:foldl(
        fun(Item, Acc) ->
            {Key, Value} = KeyValueFun(Item),
            Existing = maps:get(Key, Acc, 0),
            Acc#{Key => Existing + Value}
        end,
        #{},
        List
    ).

%%====================================================================
%% 消息操作
%%====================================================================

%% @doc 将消息列表按目标分组
%% 输入: [{Target, Value}, ...]
%% 输出: #{Target => [Value, ...]}
-spec group_messages([{term(), term()}]) -> #{term() => [term()]}.
group_messages(Messages) ->
    lists:foldl(
        fun({Target, Value}, Acc) ->
            Existing = maps:get(Target, Acc, []),
            Acc#{Target => [Value | Existing]}
        end,
        #{},
        Messages
    ).

%% @doc 合并两个消息分组，值列表合并
%% 示例: merge_message_groups(#{a => [1]}, #{a => [2], b => [3]})
%%       => #{a => [1, 2], b => [3]}
-spec merge_message_groups(#{term() => [term()]}, #{term() => [term()]}) ->
    #{term() => [term()]}.
merge_message_groups(Group1, Group2) ->
    map_merge_with(fun(L1, L2) -> L1 ++ L2 end, Group1, Group2).

%% @doc 对分组后的值应用函数
%% 示例: apply_to_groups(fun lists:sum/1, #{a => [1, 2], b => [3]})
%%       => #{a => 3, b => 3}
-spec apply_to_groups(fun(([term()]) -> term()), #{term() => [term()]}) ->
    #{term() => term()}.
apply_to_groups(Fun, Groups) ->
    maps:map(fun(_K, V) -> Fun(V) end, Groups).
