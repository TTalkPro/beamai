%%%-------------------------------------------------------------------
%%% @doc Graph State Reducer 模块
%%%
%%% 提供字段级 Reducer 功能，用于合并多个 graph_state。
%%% 在 Pregel BSP 模型中，当多个状态消息发送到同一顶点时，
%%% 使用此 Reducer 进行智能合并。
%%%
%%% 这是一个通用的框架模块，不包含业务特定的字段配置。
%%% 业务层（如 beamai_agent）应该定义自己的 field_reducers。
%%%
%%% 内置 Reducer 策略：
%%% - append_reducer: 列表追加
%%% - merge_reducer: Map 深度合并
%%% - last_write_win_reducer: 后值覆盖（默认）
%%%
%%% 使用方式：
%%% %% 业务层定义字段 Reducer
%%% FieldReducers = #{
%%%     <<"messages">> => fun graph_state_reducer:append_reducer/2,
%%%     <<"context">> => fun graph_state_reducer:merge_reducer/2
%%% },
%%% PregelOpts = #{
%%%     state_reducer => graph_state_reducer:reducer(FieldReducers)
%%% }
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(graph_state_reducer).

%% API 导出
-export([
    reducer/0,
    reducer/1,
    merge_states/1,
    merge_states/2
]).

%% 内置 Reducer 导出
-export([
    append_reducer/2,
    merge_reducer/2,
    last_write_win_reducer/2
]).

%% 类型定义
-type field_reducer() :: fun((OldValue :: term(), NewValue :: term()) -> term()).
-type field_reducers() :: #{binary() => field_reducer()}.

-export_type([field_reducer/0, field_reducers/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 返回默认的 Pregel state_reducer 函数
%%
%% 使用空的 field_reducers，所有字段使用 last_write_win 策略。
%% 业务层应该使用 reducer/1 传入自定义的字段 Reducer。
-spec reducer() -> pregel_master:state_reducer().
reducer() ->
    reducer(#{}).

%% @doc 返回自定义字段 Reducer 的 Pregel state_reducer 函数
-spec reducer(field_reducers()) -> pregel_master:state_reducer().
reducer(FieldReducers) ->
    fun(#{messages := Messages}) ->
        %% 提取所有 {state, State} 消息
        States = extract_states(Messages),
        case States of
            [] -> [];
            [Single] -> [{state, Single}];
            Multiple ->
                %% 按字段应用 Reducer
                Merged = merge_states(Multiple, FieldReducers),
                [{state, Merged}]
        end
    end.

%% @doc 合并多个状态（使用默认 Reducer，即 last_write_win）
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

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 从消息列表中提取状态
-spec extract_states([term()]) -> [graph_state:state()].
extract_states(Messages) ->
    [S || {state, S} <- Messages].

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
-spec get_field_reducer(graph_state:key(), field_reducers()) -> field_reducer().
get_field_reducer(Key, FieldReducers) when is_atom(Key) ->
    BinaryKey = atom_to_binary(Key, utf8),
    get_field_reducer(BinaryKey, FieldReducers);
get_field_reducer(Key, FieldReducers) when is_binary(Key) ->
    maps:get(Key, FieldReducers, fun last_write_win_reducer/2).

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
