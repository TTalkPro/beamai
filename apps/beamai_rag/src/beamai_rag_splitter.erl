%%%-------------------------------------------------------------------
%%% @doc RAG 文本分割模块
%%%
%%% 负责将长文本分割为可嵌入的片段（chunk）：
%%% - 按分隔符切分：默认使用换行符
%%% - 尺寸控制：确保每个片段不超过指定大小
%%% - 重叠处理：相邻片段保持部分重叠以保持上下文
%%%
%%% 分割算法：
%%% 1. 按分隔符（如换行）将文本切分为段落
%%% 2. 将段落合并为不超过 chunk_size 的块
%%% 3. 块之间保留 chunk_overlap 字节的重叠
%%%
%%% 设计原则：
%%% - 纯函数：无副作用
%%% - 保持语义：尽量不在句子中间切分
%%% - 可配置：支持自定义分割参数
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rag_splitter).

%% 导入工具函数
-import(beamai_rag_utils, [generate_uuid/0, zip_with_index/1]).

%%====================================================================
%% 导出 API
%%====================================================================

%% 分割器创建
-export([
    new/0,
    new/1
]).

%% 分割操作
-export([
    split/2
]).

%% 配置访问
-export([
    get_chunk_size/1,
    get_overlap/1,
    get_separator/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type splitter() :: #{
    chunk_size := pos_integer(),
    chunk_overlap := non_neg_integer(),
    separator := binary()
}.

-type chunk() :: #{
    content := binary(),
    chunk_id := binary(),
    chunk_index := non_neg_integer()
}.

-export_type([splitter/0, chunk/0]).

%%====================================================================
%% 常量定义
%%====================================================================

-define(DEFAULT_CHUNK_SIZE, 1000).
-define(DEFAULT_CHUNK_OVERLAP, 200).
-define(DEFAULT_SEPARATOR, <<"\n">>).

%%====================================================================
%% 分割器创建 API
%%====================================================================

%% @doc 创建默认分割器
-spec new() -> splitter().
new() ->
    new(#{}).

%% @doc 创建自定义分割器
%%
%% 选项：
%% - chunk_size: 每个块的最大字节数（默认 1000）
%% - chunk_overlap: 相邻块的重叠字节数（默认 200）
%% - separator: 切分文本的分隔符（默认 "\n"）
-spec new(map()) -> splitter().
new(Opts) ->
    #{
        chunk_size => maps:get(chunk_size, Opts, ?DEFAULT_CHUNK_SIZE),
        chunk_overlap => maps:get(chunk_overlap, Opts, ?DEFAULT_CHUNK_OVERLAP),
        separator => maps:get(separator, Opts, ?DEFAULT_SEPARATOR)
    }.

%%====================================================================
%% 分割操作 API
%%====================================================================

%% @doc 分割文本为块列表
%%
%% 返回包含内容、ID 和索引的块记录列表。
-spec split(splitter(), binary()) -> [chunk()].
split(Splitter, Text) ->
    #{chunk_size := Size, chunk_overlap := Overlap, separator := Sep} = Splitter,
    Segments = binary:split(Text, Sep, [global, trim_all]),
    ChunkBins = merge_segments(Segments, Size, Overlap, Sep),
    to_chunk_records(ChunkBins).

%%====================================================================
%% 配置访问 API
%%====================================================================

%% @doc 获取块大小配置
-spec get_chunk_size(splitter()) -> pos_integer().
get_chunk_size(#{chunk_size := Size}) -> Size.

%% @doc 获取重叠配置
-spec get_overlap(splitter()) -> non_neg_integer().
get_overlap(#{chunk_overlap := Overlap}) -> Overlap.

%% @doc 获取分隔符配置
-spec get_separator(splitter()) -> binary().
get_separator(#{separator := Sep}) -> Sep.

%%====================================================================
%% 私有函数 - 段落合并
%%====================================================================

%% @private 合并段落为块
%%
%% 将按分隔符切分的段落合并为不超过指定大小的块。
-spec merge_segments([binary()], pos_integer(), non_neg_integer(), binary()) -> [binary()].
merge_segments(Segments, ChunkSize, Overlap, Sep) ->
    merge_loop(Segments, ChunkSize, Overlap, Sep, [], <<>>).

%% @private 合并循环
%%
%% 累积段落直到超过块大小，然后开始新块。
%% 新块从上一块的重叠部分开始。
-spec merge_loop([binary()], pos_integer(), non_neg_integer(), binary(), [binary()], binary()) -> [binary()].
merge_loop([], _Size, _Overlap, _Sep, Acc, <<>>) ->
    lists:reverse(Acc);
merge_loop([], _Size, _Overlap, _Sep, Acc, Current) ->
    lists:reverse([Current | Acc]);
merge_loop([Seg | Rest], Size, Overlap, Sep, Acc, Current) ->
    NewCurrent = append_segment(Current, Seg, Sep),
    case byte_size(NewCurrent) >= Size of
        true ->
            OverlapText = extract_overlap(NewCurrent, Overlap),
            merge_loop(Rest, Size, Overlap, Sep, [NewCurrent | Acc], OverlapText);
        false ->
            merge_loop(Rest, Size, Overlap, Sep, Acc, NewCurrent)
    end.

%% @private 追加段落到当前块
-spec append_segment(binary(), binary(), binary()) -> binary().
append_segment(<<>>, Segment, _Sep) -> Segment;
append_segment(Current, Segment, Sep) -> <<Current/binary, Sep/binary, Segment/binary>>.

%% @private 提取重叠文本
%%
%% 从块末尾提取指定长度的文本作为下一块的起始。
-spec extract_overlap(binary(), non_neg_integer()) -> binary().
extract_overlap(Text, Overlap) ->
    Size = byte_size(Text),
    case Size > Overlap of
        true -> binary:part(Text, Size - Overlap, Overlap);
        false -> Text
    end.

%%====================================================================
%% 私有函数 - 块记录构建
%%====================================================================

%% @private 将二进制块列表转换为块记录列表
-spec to_chunk_records([binary()]) -> [chunk()].
to_chunk_records(ChunkBins) ->
    [#{content => C, chunk_id => generate_uuid(), chunk_index => I}
     || {I, C} <- zip_with_index(ChunkBins)].
