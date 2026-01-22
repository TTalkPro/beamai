%%%-------------------------------------------------------------------
%%% @doc Graph State Reducer 模块
%%%
%%% 提供字段级 Reducer 功能，用于合并 delta 到 global_state。
%%% 在全局状态模式下，计算函数返回 delta，Master 使用此模块
%%% 的 field_reducers 按字段合并 delta 到 global_state。
%%%
%%% 内置 Reducer 策略：
%%% - append_reducer: 列表追加
%%% - merge_reducer: Map 深度合并
%%% - last_write_win_reducer: 后值覆盖（默认）
%%% - increment_reducer: 数值增量
%%%
%%% 使用方式：
%%% %% 业务层定义字段 Reducer
%%% FieldReducers = #{
%%%     messages => fun graph_state_reducer:append_reducer/2,
%%%     context => fun graph_state_reducer:merge_reducer/2,
%%%     counter => fun graph_state_reducer:increment_reducer/2
%%% },
%%% PregelOpts = #{
%%%     field_reducers => FieldReducers
%%% }
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_state_reducer).

%% API 导出
-export([
    apply_delta/3,
    apply_deltas/3,
    merge_states/1,
    merge_states/2
]).

%% 内置 Reducer 导出
-export([
    append_reducer/2,
    merge_reducer/2,
    last_write_win_reducer/2,
    increment_reducer/2
]).

%% 类型定义
-type field_reducer() :: fun((OldValue :: term(), NewValue :: term()) -> term()).
-type field_reducers() :: #{atom() | binary() => field_reducer()}.
-type delta() :: #{atom() | binary() => term()}.

-export_type([field_reducer/0, field_reducers/0, delta/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 将单个 delta 应用到 global_state
%%
%% 遍历 delta 的每个字段，使用对应的 field_reducer 合并到 state。
%% 未配置 reducer 的字段使用 last_write_win_reducer。
%%
%% @param State 当前全局状态
%% @param Delta 要应用的增量 #{field => value}
%% @param FieldReducers 字段 Reducer 配置
%% @returns 更新后的全局状态
-spec apply_delta(graph_state:state(), delta(), field_reducers()) -> graph_state:state().
apply_delta(State, Delta, FieldReducers) when map_size(Delta) == 0 ->
    State;
apply_delta(State, Delta, FieldReducers) ->
    maps:fold(
        fun(Field, NewValue, AccState) ->
            OldValue = graph_state:get(AccState, Field),
            Reducer = get_field_reducer(Field, FieldReducers),
            MergedValue = apply_reducer(Reducer, OldValue, NewValue),
            graph_state:set(AccState, Field, MergedValue)
        end,
        State,
        Delta
    ).

%% @doc 将多个 delta 批量应用到 global_state
%%
%% 按顺序将每个 delta 应用到 state。
%%
%% @param State 当前全局状态
%% @param Deltas delta 列表
%% @param FieldReducers 字段 Reducer 配置
%% @returns 更新后的全局状态
-spec apply_deltas(graph_state:state(), [delta()], field_reducers()) -> graph_state:state().
apply_deltas(State, [], _FieldReducers) ->
    State;
apply_deltas(State, Deltas, FieldReducers) ->
    lists:foldl(
        fun(Delta, AccState) ->
            apply_delta(AccState, Delta, FieldReducers)
        end,
        State,
        Deltas
    ).

%% @doc 合并多个状态（使用默认 Reducer，即 last_write_win）
%%
%% 保留此函数以支持向后兼容，但在全局状态模式下建议使用
%% apply_deltas/3 来处理增量更新。
-spec merge_states([graph_state:state()]) -> graph_state:state().
merge_states(States) ->
    merge_states(States, #{}).

%% @doc 合并多个状态（使用自定义 Reducer）
-spec merge_states([graph_state:state()], field_reducers()) -> graph_state:state().
merge_states([], _FieldReducers) ->
    graph_state:new();
merge_states([Single], _FieldReducers) ->
    Single;
merge_states([First | Rest], FieldReducers) ->
    lists:foldl(
        fun(State, Acc) ->
            merge_two_states(Acc, State, FieldReducers)
        end,
        First,
        Rest
    ).

%%====================================================================
%% 内置 Reducer
%%====================================================================

%% @doc Append Reducer - 列表追加
%%
%% 用于 messages, full_messages, scratchpad 等列表字段
-spec append_reducer(term(), term()) -> list().
append_reducer(undefined, New) when is_list(New) -> New;
append_reducer(Old, undefined) when is_list(Old) -> Old;
append_reducer(Old, New) when is_list(Old), is_list(New) -> Old ++ New;
append_reducer(_Old, New) -> New.  %% 类型不匹配时使用新值

%% @doc Merge Reducer - Map 深度合并
%%
%% 用于 context 等 Map 字段
-spec merge_reducer(term(), term()) -> map().
merge_reducer(undefined, New) when is_map(New) -> New;
merge_reducer(Old, undefined) when is_map(Old) -> Old;
merge_reducer(Old, New) when is_map(Old), is_map(New) -> maps:merge(Old, New);
merge_reducer(_Old, New) -> New.  %% 类型不匹配时使用新值

%% @doc Last Write Win Reducer - 后值覆盖
%%
%% 默认策略，新值覆盖旧值
-spec last_write_win_reducer(term(), term()) -> term().
last_write_win_reducer(_Old, New) -> New.

%% @doc Increment Reducer - 数值增量
%%
%% 用于 counter, iteration 等数值字段
%% NewValue 是增量值，不是绝对值
-spec increment_reducer(term(), term()) -> number().
increment_reducer(undefined, New) when is_number(New) -> New;
increment_reducer(Old, undefined) when is_number(Old) -> Old;
increment_reducer(Old, New) when is_number(Old), is_number(New) -> Old + New;
increment_reducer(_Old, New) when is_number(New) -> New;
increment_reducer(Old, _New) when is_number(Old) -> Old;
increment_reducer(_Old, _New) -> 0.  %% 两者都不是数字时返回 0

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 合并两个状态
-spec merge_two_states(graph_state:state(), graph_state:state(), field_reducers()) ->
    graph_state:state().
merge_two_states(State1, State2, FieldReducers) ->
    %% 获取两个状态的所有键
    Keys1 = graph_state:keys(State1),
    Keys2 = graph_state:keys(State2),
    AllKeys = lists:usort(Keys1 ++ Keys2),

    %% 对每个键应用对应的 Reducer
    lists:foldl(
        fun(Key, Acc) ->
            OldValue = graph_state:get(State1, Key),
            NewValue = graph_state:get(State2, Key),
            Reducer = get_field_reducer(Key, FieldReducers),
            MergedValue = apply_reducer(Reducer, OldValue, NewValue),
            graph_state:set(Acc, Key, MergedValue)
        end,
        graph_state:new(),
        AllKeys
    ).

%% @private 获取字段对应的 Reducer
-spec get_field_reducer(atom() | binary(), field_reducers()) -> field_reducer().
get_field_reducer(Key, FieldReducers) when is_atom(Key) ->
    %% 优先尝试 atom key，然后尝试 binary key
    case maps:get(Key, FieldReducers, undefined) of
        undefined ->
            BinaryKey = atom_to_binary(Key, utf8),
            maps:get(BinaryKey, FieldReducers, fun last_write_win_reducer/2);
        Reducer ->
            Reducer
    end;
get_field_reducer(Key, FieldReducers) when is_binary(Key) ->
    %% 优先尝试 binary key，然后尝试 atom key
    case maps:get(Key, FieldReducers, undefined) of
        undefined ->
            try
                AtomKey = binary_to_existing_atom(Key, utf8),
                maps:get(AtomKey, FieldReducers, fun last_write_win_reducer/2)
            catch
                error:badarg ->
                    fun last_write_win_reducer/2
            end;
        Reducer ->
            Reducer
    end.

%% @private 应用 Reducer
-spec apply_reducer(field_reducer(), term(), term()) -> term().
apply_reducer(_Reducer, undefined, undefined) ->
    undefined;
apply_reducer(_Reducer, OldValue, undefined) ->
    OldValue;
apply_reducer(_Reducer, undefined, NewValue) ->
    NewValue;
apply_reducer(Reducer, OldValue, NewValue) ->
    Reducer(OldValue, NewValue).
