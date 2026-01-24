%%%-------------------------------------------------------------------
%%% @doc 统一 ID 生成与解析模块
%%%
%%% 提供加密安全的、带时间戳的唯一标识符生成和解析功能。
%%%
%%% ID 格式：{prefix}_{timestamp_hex}_{random_hex}
%%%   - prefix: 类型前缀，用于快速识别 ID 类型
%%%   - timestamp: 48位毫秒时间戳（十六进制，12字符）
%%%   - random: 80位加密随机数（十六进制，20字符）
%%%
%%% 设计特点：
%%%   - 时间有序：ID 按创建时间自然排序
%%%   - 加密安全：使用 crypto:strong_rand_bytes 生成随机部分
%%%   - 可解析：可从 ID 提取前缀和创建时间
%%%   - 分布式友好：无需中央协调即可生成唯一 ID
%%%
%%% 使用示例：
%%% ```
%%% 1> beamai_id:gen_id(<<"agent">>).
%%% <<"agent_018e4a2f3cb8_0a1b2c3d4e5f6a7b8c9d">>
%%%
%%% 2> beamai_id:parse_id(<<"agent_018e4a2f3cb8_0a1b2c3d4e5f6a7b8c9d">>).
%%% {ok, #{prefix => <<"agent">>,
%%%        timestamp => 1705123456789,
%%%        random => <<"0a1b2c3d4e5f6a7b8c9d">>}}
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_id).

%%====================================================================
%% API 导出
%%====================================================================

-export([
    gen_id/1,
    parse_id/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type id() :: binary().
%% 生成的 ID，格式为 {prefix}_{timestamp_hex}_{random_hex}

-type prefix() :: binary().
%% ID 前缀，用于标识 ID 类型

-type parsed_id() :: #{
    prefix := binary(),
    timestamp := non_neg_integer(),
    random := binary()
}.
%% 解析后的 ID 结构

-export_type([id/0, prefix/0, parsed_id/0]).

%%====================================================================
%% 常量定义
%%====================================================================

%% 时间戳位数（48位，足够使用到公元 10889 年）
-define(TIMESTAMP_BITS, 48).
%% 时间戳十六进制字符数
-define(TIMESTAMP_HEX_LEN, 12).

%% 随机数字节数（10字节 = 80位）
-define(RANDOM_BYTES, 10).
%% 随机数十六进制字符数
-define(RANDOM_HEX_LEN, 20).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 生成带前缀的唯一 ID
%%
%% 使用毫秒时间戳 + 加密随机数生成唯一标识符。
%% 时间戳在前保证 ID 可按时间排序。
%%
%% @param Prefix 类型前缀（如 <<"agent">>, <<"task">>, <<"msg">>）
%% @returns 格式为 {prefix}_{timestamp_hex}_{random_hex} 的二进制 ID
%%
%% 示例：
%% ```
%% gen_id(<<"agent">>) -> <<"agent_018e4a2f3cb8_0a1b2c3d4e5f6a7b8c9d">>
%% gen_id(<<"task">>)  -> <<"task_018e4a2f3cb9_1f2e3d4c5b6a79808182">>
%% ```
-spec gen_id(prefix()) -> id().
gen_id(Prefix) when is_binary(Prefix), byte_size(Prefix) > 0 ->
    Timestamp = erlang:system_time(millisecond),
    RandomBytes = crypto:strong_rand_bytes(?RANDOM_BYTES),
    format_id(Prefix, Timestamp, RandomBytes).

%% @doc 解析 ID 获取其组成部分
%%
%% 从 ID 中提取前缀、时间戳和随机数部分。
%%
%% @param Id 要解析的 ID
%% @returns {ok, ParsedId} 成功时返回解析结果
%%          {error, Reason} 解析失败时返回错误原因
%%
%% 示例：
%% ```
%% parse_id(<<"agent_018e4a2f3cb8_0a1b2c3d4e5f6a7b8c9d">>)
%% -> {ok, #{prefix => <<"agent">>,
%%           timestamp => 1705123456789,
%%           random => <<"0a1b2c3d4e5f6a7b8c9d">>}}
%%
%% parse_id(<<"invalid">>)
%% -> {error, invalid_format}
%% ```
-spec parse_id(id()) -> {ok, parsed_id()} | {error, term()}.
parse_id(Id) when is_binary(Id) ->
    case split_id(Id) of
        {ok, Prefix, TimestampHex, RandomHex} ->
            parse_components(Prefix, TimestampHex, RandomHex);
        {error, _} = Error ->
            Error
    end;
parse_id(_) ->
    {error, invalid_type}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 格式化 ID
-spec format_id(binary(), non_neg_integer(), binary()) -> binary().
format_id(Prefix, Timestamp, RandomBytes) ->
    TimestampHex = encode_timestamp(Timestamp),
    RandomHex = encode_random(RandomBytes),
    <<Prefix/binary, "_", TimestampHex/binary, "_", RandomHex/binary>>.

%% @private 编码时间戳为十六进制
-spec encode_timestamp(non_neg_integer()) -> binary().
encode_timestamp(Timestamp) ->
    Hex = integer_to_binary(Timestamp, 16),
    pad_left(Hex, ?TIMESTAMP_HEX_LEN).

%% @private 编码随机字节为十六进制
-spec encode_random(binary()) -> binary().
encode_random(Bytes) ->
    Hex = binary:encode_hex(Bytes, lowercase),
    Hex.

%% @private 左填充零
-spec pad_left(binary(), non_neg_integer()) -> binary().
pad_left(Bin, Len) when byte_size(Bin) >= Len ->
    Bin;
pad_left(Bin, Len) ->
    PadLen = Len - byte_size(Bin),
    Padding = binary:copy(<<"0">>, PadLen),
    <<Padding/binary, Bin/binary>>.

%% @private 分割 ID 为各部分
-spec split_id(binary()) -> {ok, binary(), binary(), binary()} | {error, term()}.
split_id(Id) ->
    case binary:split(Id, <<"_">>, [global]) of
        Parts when length(Parts) >= 3 ->
            %% 前缀可能包含下划线，所以取最后两部分作为时间戳和随机数
            {PrefixParts, [TimestampHex, RandomHex]} = lists:split(length(Parts) - 2, Parts),
            Prefix = beamai_utils:binary_join(<<"_">>, PrefixParts),
            {ok, Prefix, TimestampHex, RandomHex};
        _ ->
            {error, invalid_format}
    end.

%% @private 解析各组件
-spec parse_components(binary(), binary(), binary()) -> {ok, parsed_id()} | {error, term()}.
parse_components(Prefix, TimestampHex, RandomHex) ->
    case {parse_timestamp(TimestampHex), validate_random(RandomHex)} of
        {{ok, Timestamp}, ok} ->
            {ok, #{
                prefix => Prefix,
                timestamp => Timestamp,
                random => RandomHex
            }};
        {{error, _} = Error, _} ->
            Error;
        {_, {error, _} = Error} ->
            Error
    end.

%% @private 解析时间戳
-spec parse_timestamp(binary()) -> {ok, non_neg_integer()} | {error, term()}.
parse_timestamp(Hex) when byte_size(Hex) =:= ?TIMESTAMP_HEX_LEN ->
    try
        {ok, binary_to_integer(Hex, 16)}
    catch
        error:badarg -> {error, invalid_timestamp}
    end;
parse_timestamp(_) ->
    {error, invalid_timestamp_length}.

%% @private 验证随机数部分
-spec validate_random(binary()) -> ok | {error, term()}.
validate_random(Hex) when byte_size(Hex) =:= ?RANDOM_HEX_LEN ->
    case is_hex_string(Hex) of
        true -> ok;
        false -> {error, invalid_random}
    end;
validate_random(_) ->
    {error, invalid_random_length}.

%% @private 检查是否为有效的十六进制字符串
-spec is_hex_string(binary()) -> boolean().
is_hex_string(Bin) ->
    try
        _ = binary:decode_hex(Bin),
        true
    catch
        _:_ -> false
    end.

