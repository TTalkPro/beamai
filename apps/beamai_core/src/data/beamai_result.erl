%%%-------------------------------------------------------------------
%%% @doc 错误处理的 Result 单子
%%%
%%% 提供用于链式组合可能失败的计算的单子操作。
%%% 所有函数都使用 {ok, Value} | {error, Reason} 元组格式。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_result).

-export([ok/1, error/1, is_ok/1, is_error/1]).
-export([map/2, flat_map/2, bind/2]).
-export([pipe/2, pipe_while/2]).
-export([unwrap/1, unwrap_or/2, unwrap_error/1]).
-export([from_boolean/2, from_maybe/2]).
-export([collect/1, partition/1]).
-export([tap/2, tap_error/2]).

-type result(T) :: {ok, T} | {error, term()}.
-type result(T, E) :: {ok, T} | {error, E}.

-export_type([result/1, result/2]).

%%====================================================================
%% 构造函数
%%====================================================================

-spec ok(T) -> {ok, T}.
ok(Value) -> {ok, Value}.

-spec error(E) -> {error, E}.
error(Reason) -> {error, Reason}.

%%====================================================================
%% 谓词函数
%%====================================================================

-spec is_ok(result(term())) -> boolean().
is_ok({ok, _}) -> true;
is_ok(_) -> false.

-spec is_error(result(term())) -> boolean().
is_error({error, _}) -> true;
is_error(_) -> false.

%%====================================================================
%% 转换函数
%%====================================================================

%% @doc 如果是 ok 则应用函数，否则透传错误
-spec map(result(A), fun((A) -> B)) -> result(B).
map({ok, Value}, Fun) -> {ok, Fun(Value)};
map({error, _} = E, _Fun) -> E.

%% @doc 应用返回 result 的函数，并展平结果
-spec flat_map(result(A), fun((A) -> result(B))) -> result(B).
flat_map({ok, Value}, Fun) -> Fun(Value);
flat_map({error, _} = E, _Fun) -> E.

%% @doc flat_map 的别名（Haskell 命名风格）
-spec bind(result(A), fun((A) -> result(B))) -> result(B).
bind(Result, Fun) -> flat_map(Result, Fun).

%%====================================================================
%% 管道操作
%%====================================================================

%% @doc 将值通过函数列表传递，遇到第一个错误时停止
-spec pipe(A, [fun((A) -> result(B)) | fun((A) -> B)]) -> result(B).
pipe(Value, []) ->
    {ok, Value};
pipe(Value, [Fun | Rest]) ->
    case apply_fun(Fun, Value) of
        {ok, NewValue} -> pipe(NewValue, Rest);
        {error, _} = E -> E
    end.

%% @doc 当谓词返回 true 时继续传递
-spec pipe_while(A, [{fun((A) -> boolean()), fun((A) -> result(A))}]) -> result(A).
pipe_while(Value, []) ->
    {ok, Value};
pipe_while(Value, [{Pred, Fun} | Rest]) ->
    case Pred(Value) of
        true ->
            case apply_fun(Fun, Value) of
                {ok, NewValue} -> pipe_while(NewValue, Rest);
                {error, _} = E -> E
            end;
        false ->
            {ok, Value}
    end.

%%====================================================================
%% 解包操作
%%====================================================================

%% @doc 获取值，如果是错误则抛出异常
-spec unwrap(result(T)) -> T.
unwrap({ok, Value}) -> Value;
unwrap({error, Reason}) -> erlang:error({unwrap_error, Reason}).

%% @doc 获取值，如果是错误则返回默认值
-spec unwrap_or(result(T), T) -> T.
unwrap_or({ok, Value}, _Default) -> Value;
unwrap_or({error, _}, Default) -> Default.

%% @doc 获取错误原因
-spec unwrap_error(result(term(), E)) -> E.
unwrap_error({error, Reason}) -> Reason;
unwrap_error({ok, _}) -> erlang:error(not_an_error).

%%====================================================================
%% 类型转换
%%====================================================================

%% @doc 将布尔值转换为 result
-spec from_boolean(boolean(), E) -> result(ok, E).
from_boolean(true, _Error) -> {ok, ok};
from_boolean(false, Error) -> {error, Error}.

%% @doc 将可能为 undefined 的值转换为 result
-spec from_maybe(T | undefined, E) -> result(T, E).
from_maybe(undefined, Error) -> {error, Error};
from_maybe(Value, _Error) -> {ok, Value}.

%%====================================================================
%% 集合操作
%%====================================================================

%% @doc 将 result 列表收集为列表的 result
-spec collect([result(T)]) -> result([T]).
collect(Results) ->
    collect(Results, []).

collect([], Acc) ->
    {ok, lists:reverse(Acc)};
collect([{ok, V} | Rest], Acc) ->
    collect(Rest, [V | Acc]);
collect([{error, _} = E | _], _Acc) ->
    E.

%% @doc 将结果列表分区为成功和错误两部分
-spec partition([result(T, E)]) -> {[T], [E]}.
partition(Results) ->
    partition(Results, [], []).

partition([], Oks, Errors) ->
    {lists:reverse(Oks), lists:reverse(Errors)};
partition([{ok, V} | Rest], Oks, Errors) ->
    partition(Rest, [V | Oks], Errors);
partition([{error, E} | Rest], Oks, Errors) ->
    partition(Rest, Oks, [E | Errors]).

%%====================================================================
%% 副作用操作
%%====================================================================

%% @doc 对 ok 值执行副作用函数，返回原始结果
-spec tap(result(T), fun((T) -> term())) -> result(T).
tap({ok, Value} = R, Fun) ->
    Fun(Value),
    R;
tap({error, _} = E, _Fun) ->
    E.

%% @doc 对错误执行副作用函数，返回原始结果
-spec tap_error(result(T, E), fun((E) -> term())) -> result(T, E).
tap_error({error, Reason} = E, Fun) ->
    Fun(Reason),
    E;
tap_error({ok, _} = R, _Fun) ->
    R.

%%====================================================================
%% 内部函数
%%====================================================================

apply_fun(Fun, Value) ->
    case Fun(Value) of
        {ok, _} = R -> R;
        {error, _} = E -> E;
        Other -> {ok, Other}
    end.
