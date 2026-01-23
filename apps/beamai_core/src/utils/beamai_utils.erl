%%%-------------------------------------------------------------------
%%% @doc Agent 公共工具函数模块
%%%
%%% 提供各种 Agent 实现中常用的工具函数，避免代码重复。
%%% 所有函数都是纯函数，无副作用。
%%%
%%% 功能分类：
%%%   - 时间相关：时间戳生成、格式化
%%%   - Map 操作：安全访问、合并、转换
%%%   - 列表操作：分页、过滤、排序
%%%   - 验证函数：数据验证
%%%
%%% 注意：ID 生成已迁移至 beamai_id 模块
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_utils).

%% 导出公共 API
-export([timestamp/0, timestamp_seconds/0]).
-export([safe_get/3, safe_merge/2]).
-export([paginate/3, filter_by_time/3]).
-export([validate_binary/1, validate_map/1]).
-export([binary_join/2, to_binary/1, ensure_binary/1]).
-export([encode_body/1, decode_json_response/1]).
-export([parse_json/1, safe_execute/1, safe_execute/2]).
-export([format_error/1, format_error/2]).

%%====================================================================
%% 类型定义
%%====================================================================

-type timestamp() :: integer().

%%====================================================================
%% 时间相关函数
%%====================================================================

%% @doc 获取当前时间戳（毫秒）
%%
%% 返回从 Unix 纪元开始的毫秒数。
%%
%% @returns 时间戳（毫秒）
%%
-spec timestamp() -> timestamp().
timestamp() ->
    erlang:system_time(millisecond).

%% @doc 获取当前时间戳（秒）
%%
%% 返回从 Unix 纪元开始的秒数。
%%
%% @returns 时间戳（秒）
%%
-spec timestamp_seconds() -> pos_integer().
timestamp_seconds() ->
    erlang:system_time(second).

%%====================================================================
%% Map 操作函数
%%====================================================================

%% @doc 安全获取 Map 中的值
%%
%% 当键不存在时返回默认值，避免抛出异常。
%%
%% @param Map 目标 Map
%% @param Key 键
%% @param Default 默认值
%% @returns Map 中的值或默认值
%%
-spec safe_get(map(), term(), term()) -> term().
safe_get(Map, Key, Default) when is_map(Map) ->
    maps:get(Key, Map, Default).

%% @doc 安全合并两个 Map
%%
%% 只合并第一个 Map 中不存在的键，避免覆盖已有值。
%%
%% @param Target 目标 Map
%% @param Source 源 Map
%% @returns 合并后的 Map
%%
-spec safe_merge(map(), map()) -> map().
safe_merge(Target, Source) when is_map(Target), is_map(Source) ->
    maps:fold(fun(K, V, Acc) ->
        case maps:is_key(K, Acc) of
            true -> Acc;
            false -> maps:put(K, V, Acc)
        end
    end, Target, Source).

%%====================================================================
%% 列表操作函数
%%====================================================================

%% @doc 列表分页
%%
%% 从列表中提取指定范围的元素。
%%
%% @param List 源列表
%% @param Offset 偏移量
%% @param Limit 限制数量
%% @returns 分页后的列表
%%
-spec paginate(list(), non_neg_integer(), pos_integer()) -> list().
paginate(List, Offset, Limit) when is_list(List), is_integer(Offset), is_integer(Limit) ->
    Len = length(List),
    Start = min(Offset, Len) + 1,
    End = min(Offset + Limit, Len),
    case Start =< Len of
        true -> lists:sublist(List, Start, End - Start + 1);
        false -> []
    end.

%% @doc 按时间范围过滤
%%
%% 从包含 timestamp 字段的 Map 列表中过滤出指定时间范围内的元素。
%%
%% @param Items Map 列表
%% @param FromTs 起始时间戳
%% @param ToTs 结束时间戳
%% @returns 过滤后的列表
%%
-spec filter_by_time([map()], timestamp(), timestamp()) -> [map()].
filter_by_time(Items, FromTs, ToTs) when is_list(Items) ->
    lists:filter(fun(Item) ->
        Ts = maps:get(timestamp, Item, 0),
        Ts >= FromTs andalso Ts =< ToTs
    end, Items).

%%====================================================================
%% 验证函数
%%====================================================================

%% @doc 验证是否为有效的 Binary
%%
%% @param Term 待验证的项
%% @returns true 如果是有效的非空 binary
%%
-spec validate_binary(term()) -> boolean().
validate_binary(Term) when is_binary(Term) ->
    byte_size(Term) > 0;
validate_binary(_) ->
    false.

%% @doc 验证是否为有效的 Map
%%
%% @param Term 待验证的项
%% @returns true 如果是有效的非空 map
%%
-spec validate_map(term()) -> boolean().
validate_map(Term) when is_map(Term) ->
    map_size(Term) > 0;
validate_map(_) ->
    false.

%%====================================================================
%% 字符串操作函数
%%====================================================================

%% @doc 连接 Binary 列表
%%
%% 使用指定的分隔符连接 binary 列表。
%%
%% @param Separator 分隔符
%% @param Parts Binary 列表
%% @returns 连接后的 Binary
%%
-spec binary_join(binary(), [binary()]) -> binary().
binary_join(_Separator, []) ->
    <<>>;
binary_join(_Separator, [Single]) ->
    Single;
binary_join(Separator, [Head | Tail]) ->
    lists:foldl(fun(Part, Acc) ->
        <<Acc/binary, Separator/binary, Part/binary>>
    end, Head, Tail).

%% @doc 转换为 Binary
%%
%% 将各种类型的数据转换为 binary。
%%
%% @param Term 待转换的数据
%% @returns Binary 格式的数据
%%
-spec to_binary(term()) -> binary().
to_binary(Term) when is_binary(Term) ->
    Term;
to_binary(Term) when is_list(Term) ->
    case io_lib:printable_unicode_list(Term) of
        true -> list_to_binary(Term);
        false -> list_to_binary(Term)
    end;
to_binary(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
to_binary(Term) when is_integer(Term) ->
    list_to_binary(integer_to_list(Term));
to_binary(Term) when is_float(Term) ->
    float_to_binary(Term, [{decimals, 10}, compact]);
to_binary(Term) when is_map(Term) ->
    jsx:encode(Term);
to_binary(Term) ->
    iolist_to_binary(io_lib:format("~p", [Term])).

%% @doc 确保值为 Binary（宽松版本）
%%
%% 与 to_binary 不同：对 null/undefined 返回空 binary。
%%
%% @param Term 待转换的数据
%% @returns Binary 格式的数据
%%
-spec ensure_binary(term()) -> binary().
ensure_binary(null) -> <<>>;
ensure_binary(undefined) -> <<>>;
ensure_binary(V) -> to_binary(V).

%%====================================================================
%% JSON 解析函数
%%====================================================================

%% @doc 安全解析 JSON
%%
%% 解析失败时返回空 map，不抛异常。
%%
%% @param Input Binary 或 Map
%% @returns 解析后的 Map
%%
-spec parse_json(binary() | map()) -> map().
parse_json(Map) when is_map(Map) -> Map;
parse_json(Bin) when is_binary(Bin), byte_size(Bin) > 0 ->
    try jsx:decode(Bin, [return_maps]) of
        Result when is_map(Result) -> Result;
        _ -> #{}
    catch _:_ -> #{}
    end;
parse_json(_) -> #{}.

%%====================================================================
%% HTTP 辅助函数
%%====================================================================

%% @doc 编码 HTTP 请求体为二进制
%%
%% binary 直接返回，map/list 编码为 JSON，其他类型转二进制。
%%
%% @param Body 请求体
%% @returns 编码后的二进制
%%
-spec encode_body(binary() | map() | list() | term()) -> binary().
encode_body(Body) when is_binary(Body) -> Body;
encode_body(Body) when is_map(Body) -> jsx:encode(Body);
encode_body(Body) when is_list(Body) -> jsx:encode(Body);
encode_body(Body) -> to_binary(Body).

%% @doc 尝试将 HTTP 响应体解码为 JSON
%%
%% 如果是合法 JSON 则解码为 map，否则原样返回。
%%
%% @param Body 响应体
%% @returns 解码后的 map 或原始值
%%
-spec decode_json_response(binary() | term()) -> map() | binary() | term().
decode_json_response(Body) when is_binary(Body), byte_size(Body) > 0 ->
    case jsx:is_json(Body) of
        true ->
            try jsx:decode(Body, [return_maps])
            catch _:_ -> Body
            end;
        false -> Body
    end;
decode_json_response(Body) ->
    Body.

%%====================================================================
%% 安全执行函数
%%====================================================================

%% @doc 安全执行无参函数
%%
%% 捕获异常，返回 {ok, Result} 或 {error, {Class, Reason}}。
%%
%% @param Fun 待执行的函数
%% @returns {ok, Result} | {error, {Class, Reason}}
-spec safe_execute(fun(() -> T)) -> {ok, T} | {error, {atom(), term()}}.
safe_execute(Fun) when is_function(Fun, 0) ->
    try
        {ok, Fun()}
    catch
        Class:Reason:_ ->
            {error, {Class, Reason}}
    end.

%% @doc 安全执行单参函数
%%
%% @param Fun 待执行的函数
%% @param Arg 参数
%% @returns {ok, Result} | {error, {Class, Reason}}
-spec safe_execute(fun((A) -> T), A) -> {ok, T} | {error, {atom(), term()}}.
safe_execute(Fun, Arg) when is_function(Fun, 1) ->
    try
        {ok, Fun(Arg)}
    catch
        Class:Reason:_ ->
            {error, {Class, Reason}}
    end.

%%====================================================================
%% 错误格式化函数
%%====================================================================

%% @doc 格式化错误为 Binary
%%
%% @param Reason 错误原因
%% @returns 格式化后的错误信息
-spec format_error(term()) -> binary().
format_error(Reason) ->
    iolist_to_binary(io_lib:format("Error: ~p", [Reason])).

%% @doc 格式化错误为 Binary（带前缀）
%%
%% @param Prefix 前缀
%% @param Reason 错误原因
%% @returns 格式化后的错误信息
-spec format_error(binary(), term()) -> binary().
format_error(Prefix, Reason) ->
    iolist_to_binary(io_lib:format("~s: ~p", [Prefix, Reason])).

%%====================================================================
%% 内部函数
%%====================================================================
