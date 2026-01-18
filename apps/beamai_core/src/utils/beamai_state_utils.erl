%%%-------------------------------------------------------------------
%%% @doc 状态工具模块
%%%
%%% 提供图状态的批量操作和管道式更新功能，
%%% 用于替代 State1→State2→State3 链式更新模式。
%%%
%%% 设计原则：
%%% - 函数式风格：不可变状态，返回新状态
%%% - 批量操作：减少中间变量，提高可读性
%%% - 管道支持：支持函数组合进行复杂更新
%%%
%%% 使用示例：
%%% <pre>
%%% %% 链式更新（旧方式）
%%% State1 = graph:set(State, key1, val1),
%%% State2 = graph:set(State1, key2, val2),
%%% State3 = graph:set(State2, key3, val3),
%%%
%%% %% 批量更新（新方式）
%%% NewState = beamai_state_utils:set_many(State, [
%%%     {key1, val1},
%%%     {key2, val2},
%%%     {key3, val3}
%%% ]).
%%%
%%% %% 管道更新
%%% NewState = beamai_state_utils:set_pipeline(State, [
%%%     fun(S) -&gt; graph:set(S, key1, val1) end,
%%%     fun(S) -&gt; maybe_set_conditional(S) end
%%% ]).
%%% </pre>
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_state_utils).

%% API 导出
-export([set_many/2]).
-export([set_pipeline/2]).
-export([update_if/4]).
-export([get_many/2]).
-export([merge_into/3]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 批量设置状态值
%%
%% 参数：
%% - State: 图状态
%% - Pairs: 键值对列表 [{Key, Value}, ...]
%%
%% 返回更新后的状态
-spec set_many(map(), [{atom(), term()}]) -> map().
set_many(State, Pairs) ->
    lists:foldl(
        fun({Key, Value}, Acc) -> graph:set(Acc, Key, Value) end,
        State,
        Pairs
    ).

%% @doc 管道式状态更新
%%
%% 按顺序应用更新函数列表，每个函数接收状态返回新状态。
%% 适用于需要条件判断或复杂逻辑的更新场景。
%%
%% 参数：
%% - State: 初始状态
%% - Funs: 更新函数列表 [fun((State) -> NewState), ...]
-spec set_pipeline(map(), [fun((map()) -> map())]) -> map().
set_pipeline(State, Funs) ->
    lists:foldl(fun(F, Acc) -> F(Acc) end, State, Funs).

%% @doc 条件更新状态
%%
%% 当条件为 true 时设置值，否则返回原状态
%%
%% 参数：
%% - State: 图状态
%% - Key: 键
%% - Value: 值
%% - Condition: 布尔条件
-spec update_if(map(), atom(), term(), boolean()) -> map().
update_if(State, _Key, _Value, false) ->
    State;
update_if(State, Key, Value, true) ->
    graph:set(State, Key, Value).

%% @doc 批量获取状态值
%%
%% 参数：
%% - State: 图状态
%% - Keys: 键列表，每个元素可以是 Key 或 {Key, Default}
%%
%% 返回值列表，顺序与 Keys 对应
-spec get_many(map(), [atom() | {atom(), term()}]) -> [term()].
get_many(State, Keys) ->
    lists:map(
        fun({Key, Default}) -> graph:get(State, Key, Default);
           (Key) -> graph:get(State, Key, undefined)
        end,
        Keys
    ).

%% @doc 将源 Map 中的指定键合并到状态
%%
%% 参数：
%% - State: 目标状态
%% - Source: 源 Map
%% - Keys: 要合并的键列表
%%
%% 只有在 Source 中存在的键才会被合并
-spec merge_into(map(), map(), [atom()]) -> map().
merge_into(State, Source, Keys) ->
    lists:foldl(
        fun(Key, Acc) ->
            case maps:get(Key, Source, undefined) of
                undefined -> Acc;
                Value -> graph:set(Acc, Key, Value)
            end
        end,
        State,
        Keys
    ).
