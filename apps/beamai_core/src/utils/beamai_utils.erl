%%%-------------------------------------------------------------------
%%% @doc Agent 公共工具函数模块
%%%
%%% 提供各种 Agent 实现中常用的工具函数，避免代码重复。
%%% 所有函数都是纯函数，无副作用。
%%%
%%% 功能分类：
%%%   - 字符串/Binary：连接、类型转换
%%%   - JSON：请求体编码、响应解码、解析
%%%
%%% 注意：ID 生成已迁移至 beamai_id 模块。时间戳、Map 安全访问、列表分页、
%%% 数据验证、safe_execute、format_error 等一批全仓零引用的 stdlib 薄包装
%%% 已删除（多为 erlang:system_time/1、maps:get/3 的改名）——需要时直接用
%%% stdlib 即可。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_utils).

%% 导出公共 API
-export([binary_join/2, to_binary/1, ensure_binary/1]).
-export([encode_body/1, decode_json_response/1]).
-export([parse_json/1, encode_json/1]).

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
    case unicode:characters_to_binary(Term) of
        Bin when is_binary(Bin) -> Bin;
        _ -> iolist_to_binary(io_lib:format("~p", [Term]))
    end;
to_binary(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
to_binary(Term) when is_integer(Term) ->
    list_to_binary(integer_to_list(Term));
to_binary(Term) when is_float(Term) ->
    float_to_binary(Term, [{decimals, 10}, compact]);
to_binary(Term) when is_map(Term) ->
    encode_json(Term);
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
    try json:decode(Bin) of
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
encode_body(Body) when is_map(Body) -> encode_json(Body);
encode_body(Body) when is_list(Body) -> encode_json(Body);
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
    %% 旧的 jsx 实现先 is_json/1 验证再解码，等于解析两遍；OTP 的 json 模块也没有
    %% is_json 对应物。直接试解码、失败回落原值，语义等价且只解析一遍。
    try json:decode(Body)
    catch _:_ -> Body
    end;
decode_json_response(Body) ->
    Body.

%%====================================================================
%% JSON 编码函数
%%====================================================================

%% @doc JSON 编码为 binary（OTP `json' 模块的薄适配层）
%%
%% 解码请直接用 `json:decode/1'——它无需适配。编码则有两处差异需要抹平：
%%   1. `json:encode/1' 返回 iodata，jsx 返回 binary——本模块对外契约是 binary。
%%   2. jsx 把 proplist 编码成 JSON 对象且**保持键的插入顺序**；`json:encode/1'
%%      只认 map，遇到 proplist 抛 `{unsupported_type, {K, V}}'。encode_body/1
%%      的 list 子句是公开 API（beamai_http:post/3 等的 Body 可以是 proplist），
%%      故用自定义 encoder 接住：`json:encode_key_value_list/2' 按序编码，
%%      与 jsx 逐字节一致（转成 map 会丢顺序）。
-spec encode_json(term()) -> binary().
encode_json(Term) ->
    iolist_to_binary(json:encode(Term, fun json_encoder/2)).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 自定义 encoder：proplist 走有序的 kv 编码，其余交回默认实现
json_encoder([{_, _} | _] = List, Encode) ->
    case is_proplist(List) of
        true -> json:encode_key_value_list(List, Encode);
        false -> json:encode_value(List, Encode)
    end;
json_encoder(Term, Encode) ->
    json:encode_value(Term, Encode).

%% @private proplist 判定：键须为 atom 或 binary（与 jsx 接受的对象键一致）
-spec is_proplist(list()) -> boolean().
is_proplist(List) ->
    lists:all(fun({K, _}) -> is_atom(K) orelse is_binary(K);
                 (_) -> false
              end, List).
