%%%-------------------------------------------------------------------
%%% @doc 工具索引 Behaviour：大工具集的检索式收窄
%%%
%%% 对标 Spring AI 2.0 `ToolSearchToolCallingAdvisor`：工具数量一多，
%%% 把全量 tool schema 塞进每轮请求既烧 token 又拉低模型选择精度。
%%% 索引先按查询把工具集收窄到相关的少数几个，再对 LLM 广播
%%% （Spring 实测 28 个工具时省 34%~64% token）。
%%%
%%% 句柄约定 `{Module, State}`（与 beamai_chat_memory 的 `{Module, Ref}` 一致）：
%%% - Module 实现本 Behaviour
%%% - State 为 build/2 产出的实现私有索引状态（不透明，调用方勿窥探）
%%%
%%% 调度 API（new/3、search/3）对句柄解包后转发到对应实现模块，
%%% 调用方无需硬编码后端模块名。内置实现：
%%%
%%%   - beamai_tool_index_regex   —— 正则/子串匹配，零依赖、可预期
%%%   - beamai_tool_index_keyword —— BM25 词频打分，识别 camelCase/snake_case
%%%
%%% **查询来自模型**：search 的 Query 是 LLM 生成的自由文本，实现必须对
%%% 任意输入健壮（不得因非法正则、非法编码等崩溃）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_index).

%% 调度 API
-export([new/3, search/3]).

%% API - 工具集指纹
-export([fingerprint/1]).

%% Types
-export_type([handle/0]).

-opaque handle() :: {module(), State :: term()}.

%%====================================================================
%% Behaviour 回调
%%====================================================================

%% @doc 构建索引状态
%%
%% 在工具集变化时调用一次，产出的 State 供后续 search/3 反复使用。
%% 文档频率、平均文档长度等全局统计量应在此预计算。
-callback build(Tools :: [beamai_tool:tool_spec()], Opts :: map()) -> State :: term().

%% @doc 按查询检索工具，返回**工具名**列表
%%
%% 按相关度降序（最佳匹配在前），最多 MaxResults 个。无匹配返回 []。
%% Query 由 LLM 生成，实现不得对其内容做任何假设。
-callback search(Query :: binary(), MaxResults :: pos_integer(), State :: term()) ->
    [binary()].

%%====================================================================
%% 调度 API
%%====================================================================

%% @doc 构建索引句柄
%%
%% @param Module 实现本 Behaviour 的后端模块
%% @param Tools 待索引的工具集
%% @param Opts 后端私有选项（各实现自行约定，未知键忽略）
%% @returns 索引句柄，供 search/3 使用
-spec new(module(), [beamai_tool:tool_spec()], map()) -> handle().
new(Module, Tools, Opts) when is_atom(Module), is_list(Tools), is_map(Opts) ->
    {Module, Module:build(Tools, Opts)}.

%% @doc 按查询检索工具名（最佳匹配在前，至多 MaxResults 个）
-spec search(handle(), binary(), pos_integer()) -> [binary()].
search({Module, State}, Query, MaxResults)
  when is_binary(Query), is_integer(MaxResults), MaxResults > 0 ->
    Module:search(Query, MaxResults, State).

%%====================================================================
%% API - 工具集指纹
%%====================================================================

%% @doc 计算工具集指纹（SHA-256），用于跳过无变化时的重建索引
%%
%% 语义：**同一批工具无论列表顺序如何，指纹恒相同**；任一工具的
%% name 或 description 变化则指纹变化。调用方缓存上次指纹，比对相等
%% 即可复用旧句柄（索引重建对 keyword 后端是全量扫描，值得省）。
%%
%% 编码（对齐 Spring 的做法）：按 name 排序后逐条喂入
%% `Name, 0, Desc, 1`——字段间 `0`、记录间 `1` 作分隔符。字节 0/1 不会
%% 出现在合法 UTF-8 文本中，故 `{<<"ab">>, <<"c">>}` 与 `{<<"a">>, <<"bc">>}`
%% 不会碰撞（若直接拼接则两者同为 <<"abc">>）。
%%
%% @param Tools 工具集
%% @returns 32 字节 SHA-256 摘要
-spec fingerprint([beamai_tool:tool_spec()]) -> binary().
fingerprint(Tools) ->
    Sorted = lists:sort(fun(A, B) -> beamai_tool:get_name(A) =< beamai_tool:get_name(B) end,
                        Tools),
    IoData = [[beamai_tool:get_name(T), 0, description(T), 1] || T <- Sorted],
    crypto:hash(sha256, IoData).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 取工具描述（缺省空串）
-spec description(beamai_tool:tool_spec()) -> binary().
description(ToolSpec) ->
    maps:get(description, ToolSpec, <<>>).
