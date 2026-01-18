%%%-------------------------------------------------------------------
%%% @doc Pregel 消息合并器模块
%%%
%%% 合并器用于减少网络通信开销:
%%% - 将多条发往同一顶点的消息合并为一条
%%% - 合并器必须满足交换律和结合律
%%%
%%% 内置合并器: sum, min, max, count, first, last, list, and, or
%%%
%%% 设计模式: 策略模式 + 工厂模式
%%% @end
%%%-------------------------------------------------------------------
-module(pregel_combiner).
-compile({no_auto_import, [get/1]}).

%% 内置合并器
-export([sum/0, min/0, max/0, count/0]).
-export([first/0, last/0, list/0]).
-export(['and'/0, 'or'/0]).

%% 自定义合并器
-export([make/2, make_topk/1, make_topk/2]).

%% 应用合并器
-export([combine/2, get/1]).

%% 类型导出
-export_type([combiner/0, spec/0]).

%%====================================================================
%% 类型定义
%%====================================================================

%% 合并器函数类型
-type combiner() :: fun(([term()]) -> term()).

%% 合并器规格（可以是内置名称或自定义函数）
-type spec() :: sum | min | max | count | first | last | list | 'and' | 'or' | combiner().

%%====================================================================
%% 内置合并器
%%====================================================================

%% @doc 累加合并器
-spec sum() -> combiner().
sum() -> fun lists:sum/1.

%% @doc 取最小值
-spec min() -> combiner().
min() ->
    fun([H | T]) -> lists:foldl(fun erlang:min/2, H, T);
       ([]) -> undefined
    end.

%% @doc 取最大值
-spec max() -> combiner().
max() ->
    fun([H | T]) -> lists:foldl(fun erlang:max/2, H, T);
       ([]) -> undefined
    end.

%% @doc 计数
-spec count() -> combiner().
count() -> fun erlang:length/1.

%% @doc 取首个值
-spec first() -> combiner().
first() ->
    fun([H | _]) -> H;
       ([]) -> undefined
    end.

%% @doc 取最后一个值
-spec last() -> combiner().
last() ->
    fun([]) -> undefined;
       (Vs) -> lists:last(Vs)
    end.

%% @doc 保留所有值（不合并）
-spec list() -> combiner().
list() -> fun(Vs) -> Vs end.

%% @doc 逻辑与
-spec 'and'() -> combiner().
'and'() -> fun(Vs) -> lists:all(fun(V) -> V =:= true end, Vs) end.

%% @doc 逻辑或
-spec 'or'() -> combiner().
'or'() -> fun(Vs) -> lists:any(fun(V) -> V =:= true end, Vs) end.

%%====================================================================
%% 自定义合并器
%%====================================================================

%% @doc 创建自定义规约合并器
%% ReduceFun: fun((Value, Acc) -> NewAcc)
-spec make(fun((term(), term()) -> term()), term()) -> combiner().
make(ReduceFun, Init) ->
    fun(Vs) -> lists:foldl(ReduceFun, Init, Vs) end.

%% @doc TopK 合并器（保留最小的 K 个值）
-spec make_topk(pos_integer()) -> combiner().
make_topk(K) ->
    make_topk(K, fun(A, B) -> A =< B end).

%% @doc TopK 合并器（自定义比较函数）
-spec make_topk(pos_integer(), fun((term(), term()) -> boolean())) -> combiner().
make_topk(K, CompareFun) ->
    fun(Vs) -> lists:sublist(lists:sort(CompareFun, Vs), K) end.

%%====================================================================
%% 应用合并器
%%====================================================================

%% @doc 使用合并器合并消息列表
-spec combine(spec(), [term()]) -> term().
combine(Combiner, Values) when is_function(Combiner) ->
    Combiner(Values);
combine(Name, Values) when is_atom(Name) ->
    (get(Name))(Values).

%% @doc 根据规格获取合并器函数
-spec get(spec()) -> combiner().
get(sum) -> sum();
get(min) -> min();
get(max) -> max();
get(count) -> count();
get(first) -> first();
get(last) -> last();
get(list) -> list();
get('and') -> 'and'();
get('or') -> 'or'();
get(Fun) when is_function(Fun) -> Fun.
