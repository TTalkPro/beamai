%%%-------------------------------------------------------------------
%%% @doc Agent Memory 公共工具模块
%%%
%%% 提供 beamai_memory 各子模块共享的工具函数，包括：
%%% - ID 生成：统一的唯一标识符生成
%%% - 时间戳：当前时间戳获取
%%% - 类型转换：二进制与原子的安全转换
%%% - 命名空间：命名空间路径构建
%%% - 存储选项：嵌入向量等选项构建
%%% - 搜索过滤：通用搜索条件构建
%%%
%%% 设计原则：
%%% - 所有函数均为纯函数，无副作用
%%% - 提供类型规范，便于 Dialyzer 检查
%%% - 统一错误处理模式
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_utils).

%%====================================================================
%% ID 生成函数
%%====================================================================
-export([
    generate_id/1,
    generate_id/2
]).

%%====================================================================
%% 时间戳函数
%%====================================================================
-export([
    current_timestamp/0,
    current_timestamp_micro/0
]).

%%====================================================================
%% 类型转换函数
%%====================================================================
-export([
    safe_binary_to_atom/1,
    maybe_atom_to_binary/1,
    maybe_binary_to_atom/1,
    to_binary/1
]).

%%====================================================================
%% 命名空间函数
%%====================================================================
-export([
    build_namespace/2,
    namespace_to_tuple/1,
    tuple_to_namespace/1
]).

%%====================================================================
%% 存储选项函数
%%====================================================================
-export([
    embedding_to_store_opts/1,
    merge_store_opts/2
]).

%%====================================================================
%% 搜索过滤函数
%%====================================================================
-export([
    build_search_opts/2,
    build_filter/2,
    add_query_opts/2,
    add_limit_opts/2
]).

%%====================================================================
%% 记录更新函数
%%====================================================================
-export([
    apply_updates/4
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type namespace() :: [binary()].
%% 命名空间路径，如 [<<"semantic">>, <<"user_123">>, <<"preferences">>]

-type filter_spec() :: [{atom(), filter_transform()}].
%% 过滤规范列表，定义字段到过滤条件的映射

-type filter_transform() :: fun((term()) -> term()) | direct.
%% 过滤值转换函数，direct 表示直接使用原值

-type search_opts() :: #{
    filter => map(),
    query => binary(),
    limit => pos_integer(),
    offset => non_neg_integer()
}.
%% 搜索选项

-export_type([namespace/0, filter_spec/0, filter_transform/0, search_opts/0]).

%%====================================================================
%% ID 生成函数实现
%%====================================================================

%% @doc 生成带前缀的唯一 ID
%%
%% 使用微秒时间戳 + 随机数生成唯一标识符。
%% 格式：{prefix}_{timestamp_hex}_{random_hex}
%%
%% 示例：
%% ```
%% generate_id(<<"cp">>) -> <<"cp_0001234567890abc_1a2b">>
%% generate_id(<<"skill">>) -> <<"skill_0001234567890abc_1a2b">>
%% ```
-spec generate_id(binary() | string()) -> binary().
generate_id(Prefix) when is_binary(Prefix) ->
    Ts = erlang:system_time(microsecond),
    Rand = rand:uniform(16#FFFF),
    list_to_binary(io_lib:format("~s_~16.16.0b_~4.16.0b", [Prefix, Ts, Rand]));
generate_id(Prefix) when is_list(Prefix) ->
    generate_id(list_to_binary(Prefix)).

%% @doc 生成带前缀和后缀的唯一 ID
%%
%% 用于需要额外标识信息的场景。
-spec generate_id(binary(), binary()) -> binary().
generate_id(Prefix, Suffix) ->
    BaseId = generate_id(Prefix),
    <<BaseId/binary, "_", Suffix/binary>>.

%%====================================================================
%% 时间戳函数实现
%%====================================================================

%% @doc 获取当前毫秒时间戳
%%
%% 用于记录创建时间、更新时间等。
-spec current_timestamp() -> integer().
current_timestamp() ->
    erlang:system_time(millisecond).

%% @doc 获取当前微秒时间戳
%%
%% 用于需要更高精度的场景，如 ID 生成。
-spec current_timestamp_micro() -> integer().
current_timestamp_micro() ->
    erlang:system_time(microsecond).

%%====================================================================
%% 类型转换函数实现
%%====================================================================

%% @doc 安全地将二进制转换为原子
%%
%% 优先使用 binary_to_existing_atom 避免原子表溢出，
%% 如果原子不存在则创建新原子。
%%
%% 注意：此函数可能创建新原子，应谨慎使用于不受信任的输入。
-spec safe_binary_to_atom(binary()) -> atom().
safe_binary_to_atom(Bin) when is_binary(Bin) ->
    try
        binary_to_existing_atom(Bin, utf8)
    catch
        error:badarg ->
            binary_to_atom(Bin, utf8)
    end;
safe_binary_to_atom(Atom) when is_atom(Atom) ->
    Atom.

%% @doc 将原子转换为二进制（如果是原子）
%%
%% 如果输入已经是二进制或 undefined，则原样返回。
-spec maybe_atom_to_binary(atom() | binary() | undefined) -> binary() | undefined.
maybe_atom_to_binary(undefined) -> undefined;
maybe_atom_to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
maybe_atom_to_binary(Bin) when is_binary(Bin) -> Bin.

%% @doc 将二进制转换为原子（如果是二进制）
%%
%% 如果输入已经是原子或 undefined，则原样返回。
-spec maybe_binary_to_atom(binary() | atom() | undefined) -> atom() | undefined.
maybe_binary_to_atom(undefined) -> undefined;
maybe_binary_to_atom(Bin) when is_binary(Bin) -> safe_binary_to_atom(Bin);
maybe_binary_to_atom(Atom) when is_atom(Atom) -> Atom.

%% @doc 将任意类型转换为二进制
%%
%% 支持：binary, atom, integer, list, 其他（使用 io_lib:format）
-spec to_binary(term()) -> binary().
to_binary(Bin) when is_binary(Bin) -> Bin;
to_binary(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
to_binary(Int) when is_integer(Int) -> integer_to_binary(Int);
to_binary(List) when is_list(List) -> list_to_binary(List);
to_binary(Term) -> list_to_binary(io_lib:format("~p", [Term])).

%%====================================================================
%% 命名空间函数实现
%%====================================================================

%% @doc 构建命名空间路径
%%
%% 将顶级命名空间与子路径组合成完整的命名空间路径。
%%
%% 示例：
%% ```
%% build_namespace(<<"semantic">>, [UserId, <<"preferences">>])
%% => [<<"semantic">>, UserId, <<"preferences">>]
%% ```
-spec build_namespace(binary(), [binary()]) -> namespace().
build_namespace(TopLevel, Parts) when is_binary(TopLevel), is_list(Parts) ->
    [TopLevel | Parts].

%% @doc 将命名空间列表转换为元组
%%
%% 用于 ETS 键的构建，元组比较更高效。
-spec namespace_to_tuple(namespace()) -> tuple().
namespace_to_tuple(Namespace) when is_list(Namespace) ->
    list_to_tuple(Namespace).

%% @doc 将元组转换为命名空间列表
-spec tuple_to_namespace(tuple()) -> namespace().
tuple_to_namespace(Tuple) when is_tuple(Tuple) ->
    tuple_to_list(Tuple).

%%====================================================================
%% 存储选项函数实现
%%====================================================================

%% @doc 将嵌入向量转换为存储选项
%%
%% 如果嵌入向量为 undefined，返回空 map。
%% 否则返回包含 embedding 键的 map。
-spec embedding_to_store_opts(undefined | [float()]) -> map().
embedding_to_store_opts(undefined) -> #{};
embedding_to_store_opts(Embedding) when is_list(Embedding) ->
    #{embedding => Embedding}.

%% @doc 合并存储选项
%%
%% 将两个选项 map 合并，后者覆盖前者的相同键。
-spec merge_store_opts(map(), map()) -> map().
merge_store_opts(Base, Override) ->
    maps:merge(Base, Override).

%%====================================================================
%% 搜索过滤函数实现
%%====================================================================

%% @doc 构建搜索选项
%%
%% 根据用户提供的选项和过滤规范构建标准化的搜索选项。
%%
%% FilterSpecs 定义了如何将用户选项转换为过滤条件：
%% - {FieldName, direct} - 直接使用值
%% - {FieldName, TransformFun} - 应用转换函数
%%
%% 示例：
%% ```
%% FilterSpecs = [
%%     {type, fun(V) -> atom_to_binary(V, utf8) end},
%%     {category, direct}
%% ],
%% build_search_opts(#{type => learning, limit => 10}, FilterSpecs)
%% => #{filter => #{<<"type">> => <<"learning">>}, limit => 10}
%% ```
-spec build_search_opts(map(), filter_spec()) -> search_opts().
build_search_opts(Opts, FilterSpecs) ->
    Filter = build_filter(Opts, FilterSpecs),
    Opts1 = add_filter_to_opts(#{}, Filter),
    Opts2 = add_query_opts(Opts1, Opts),
    add_limit_opts(Opts2, Opts).

%% @doc 根据规范构建过滤条件 map
-spec build_filter(map(), filter_spec()) -> map().
build_filter(Opts, FilterSpecs) ->
    lists:foldl(fun({Key, Transform}, Acc) ->
        case maps:get(Key, Opts, undefined) of
            undefined -> Acc;
            Value ->
                BinKey = to_binary(Key),
                TransformedValue = apply_transform(Transform, Value),
                Acc#{BinKey => TransformedValue}
        end
    end, #{}, FilterSpecs).

%% @doc 添加查询选项
-spec add_query_opts(map(), map()) -> map().
add_query_opts(SearchOpts, Opts) ->
    case maps:get(query, Opts, undefined) of
        undefined -> SearchOpts;
        Query -> SearchOpts#{query => Query}
    end.

%% @doc 添加限制选项
-spec add_limit_opts(map(), map()) -> map().
add_limit_opts(SearchOpts, Opts) ->
    Opts1 = case maps:get(limit, Opts, undefined) of
        undefined -> SearchOpts;
        Limit -> SearchOpts#{limit => Limit}
    end,
    case maps:get(offset, Opts, undefined) of
        undefined -> Opts1;
        Offset -> Opts1#{offset => Offset}
    end.

%%====================================================================
%% 记录更新函数实现
%%====================================================================

%% @doc 应用更新到记录
%%
%% 通用的记录字段更新函数，使用 maps:fold 遍历更新 map，
%% 根据 UpdateSpecs 定义的字段映射更新记录。
%%
%% UpdateSpecs 格式：[{FieldAtom, RecordIndex}]
%%
%% 示例：
%% ```
%% UpdateSpecs = [{name, 2}, {description, 3}],
%% apply_updates(Record, #{name => <<"New Name">>}, UpdateSpecs, Timestamp)
%% ```
-spec apply_updates(tuple(), map(), [{atom(), pos_integer()}], integer()) -> tuple().
apply_updates(Record, Updates, UpdateSpecs, Timestamp) ->
    Record1 = maps:fold(fun(Key, Value, Rec) ->
        case lists:keyfind(Key, 1, UpdateSpecs) of
            {_, Index} -> setelement(Index, Rec, Value);
            false -> Rec
        end
    end, Record, Updates),
    %% 更新 updated_at 字段（假设在最后一个位置之前）
    case lists:keyfind(updated_at, 1, UpdateSpecs) of
        {_, UpdatedAtIndex} -> setelement(UpdatedAtIndex, Record1, Timestamp);
        false -> Record1
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 应用过滤值转换
-spec apply_transform(filter_transform(), term()) -> term().
apply_transform(direct, Value) -> Value;
apply_transform(Fun, Value) when is_function(Fun, 1) -> Fun(Value).

%% @private 添加过滤条件到选项
-spec add_filter_to_opts(map(), map()) -> map().
add_filter_to_opts(SearchOpts, Filter) when map_size(Filter) =:= 0 ->
    SearchOpts;
add_filter_to_opts(SearchOpts, Filter) ->
    SearchOpts#{filter => Filter}.
