%%%-------------------------------------------------------------------
%%% @doc A2A 公共工具函数模块
%%%
%%% 提供 A2A 模块共用的工具函数，减少代码重复。
%%%
%%% == 主要功能 ==
%%%
%%% 1. 错误格式化
%%%    - format_error/1: 将各种错误类型转换为 binary
%%%
%%% 2. ID 生成
%%%    - generate_id/0: 生成唯一 ID
%%%    - generate_id/1: 生成带前缀的唯一 ID
%%%
%%% 3. 时间戳
%%%    - timestamp/0: 获取当前毫秒时间戳
%%%    - timestamp_iso8601/0: 获取 ISO 8601 格式时间戳
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 格式化错误
%%% ErrorBin = beamai_a2a_utils:format_error(some_error),
%%% %% => <<"some_error">>
%%%
%%% ErrorBin2 = beamai_a2a_utils:format_error({http_error, 500, "Internal Error"}),
%%% %% => <<"{http_error,500,\"Internal Error\"}">>
%%%
%%% %% 生成 ID
%%% TaskId = beamai_a2a_utils:generate_id(<<"task_">>),
%%% %% => <<"task_abc123...">>
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_utils).

%% API 导出
-export([
    %% 错误格式化
    format_error/1,

    %% ID 生成
    generate_id/0,
    generate_id/1,

    %% 时间戳
    timestamp/0,
    timestamp_iso8601/0
]).

%%====================================================================
%% 错误格式化
%%====================================================================

%% @doc 将错误转换为 binary 格式
%%
%% 支持多种输入类型：
%% - binary: 直接返回
%% - atom: 转换为 binary
%% - 其他: 使用 io_lib:format 格式化
%%
%% 此函数用于将内部错误原因转换为可在 JSON-RPC 响应中使用的字符串。
%%
%% @param Reason 错误原因（任意类型）
%% @returns 错误描述的 binary 字符串
-spec format_error(term()) -> binary().
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) when is_atom(Reason) ->
    atom_to_binary(Reason, utf8);
format_error(Reason) when is_list(Reason) ->
    %% 尝试作为字符串处理
    try
        unicode:characters_to_binary(Reason)
    catch
        _:_ ->
            iolist_to_binary(io_lib:format("~p", [Reason]))
    end;
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%%====================================================================
%% ID 生成
%%====================================================================

%% @doc 生成唯一 ID
%%
%% 使用 UUID v4 格式生成唯一标识符。
%%
%% @returns 唯一 ID（binary）
-spec generate_id() -> binary().
generate_id() ->
    %% 生成 16 字节随机数据
    Bytes = crypto:strong_rand_bytes(16),
    %% 设置 UUID v4 版本位
    <<A:32, B:16, _:4, C:12, _:2, D:62>> = Bytes,
    %% 格式化为标准 UUID 字符串
    list_to_binary(io_lib:format(
        "~8.16.0b-~4.16.0b-4~3.16.0b-~1.16.0b~3.16.0b-~12.16.0b",
        [A, B, C, 8 + (D bsr 60), (D bsr 48) band 16#fff, D band 16#ffffffffffff]
    )).

%% @doc 生成带前缀的唯一 ID
%%
%% @param Prefix ID 前缀（binary）
%% @returns 带前缀的唯一 ID（binary）
-spec generate_id(binary()) -> binary().
generate_id(Prefix) when is_binary(Prefix) ->
    Id = generate_id(),
    <<Prefix/binary, Id/binary>>.

%%====================================================================
%% 时间戳
%%====================================================================

%% @doc 获取当前毫秒时间戳
%%
%% @returns Unix 毫秒时间戳（integer）
-spec timestamp() -> non_neg_integer().
timestamp() ->
    erlang:system_time(millisecond).

%% @doc 获取 ISO 8601 格式时间戳
%%
%% 返回格式：YYYY-MM-DDTHH:MM:SS.sssZ
%%
%% @returns ISO 8601 格式时间戳（binary）
-spec timestamp_iso8601() -> binary().
timestamp_iso8601() ->
    {{Y, M, D}, {H, Mi, S}} = calendar:universal_time(),
    Ms = erlang:system_time(millisecond) rem 1000,
    list_to_binary(io_lib:format(
        "~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0B.~3..0BZ",
        [Y, M, D, H, Mi, S, Ms]
    )).
