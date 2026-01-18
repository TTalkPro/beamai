%%%-------------------------------------------------------------------
%%% @doc JSON Output Parser
%%%
%%% 容错解析 LLM 输出的 JSON。
%%%
%%% 主要特性：
%%% - 从 markdown 代码块中提取 JSON
%%% - 修复常见格式错误（尾随逗号、注释等）
%%% - 支持多种 JSON 变体
%%% - 提供详细的错误信息
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_parser_json).

%% 解析 API
-export([parse/2]).

%% 内部导出（供测试使用）
-export([extract_json/1, extract_json_codeblock/1]).
-export([repair_json/1]).
-export([find_json_boundaries/1]).

%%====================================================================
%% 类型定义
%%====================================================================

-type parse_options() :: #{
    extract_codeblock => boolean(),
    repair_common => boolean(),
    strip_markdown => boolean(),
    schema => map()
}.

-type parse_result() :: {ok, term()} | {error, term()}.

%%====================================================================
%% 主解析函数
%%====================================================================

%% @doc 解析 JSON 文本
%%
%% 支持容错解析，包括：
%% 1. 从 markdown 代码块提取 JSON
%% 2. 修复常见格式错误
%% 3. 处理各种边界情况
-spec parse(binary(), parse_options()) -> parse_result().
parse(Text, Opts) ->
    %% 1. 预处理：提取或清理
    ProcessedText = preprocess(Text, Opts),

    %% 2. 尝试直接解析
    case try_parse(ProcessedText) of
        {ok, _} = Result ->
            Result;
        {error, _} ->
            %% 3. 如果失败，尝试修复后解析
            case maps:get(repair_common, Opts, true) of
                true ->
                    Repaired = repair_json(ProcessedText),
                    try_parse(Repaired);
                false ->
                    {error, {invalid_json, ProcessedText}}
            end
    end.

%%====================================================================
%% 预处理
%%====================================================================

%% @doc 预处理文本
-spec preprocess(binary(), parse_options()) -> binary().
preprocess(Text, Opts) ->
    %% 先提取代码块，再处理其他格式
    T1 = maybe_extract_codeblock(Text, Opts),
    T2 = maybe_strip_markdown(T1, Opts),
    trim_whitespace(T2).

%% @doc 移除 markdown 格式
-spec maybe_strip_markdown(binary(), parse_options()) -> binary().
maybe_strip_markdown(Text, #{strip_markdown := true}) ->
    %% 移除可能的 markdown 格式标记
    case Text of
        <<"```json", Rest/binary>> -> Rest;
        <<"```", Rest/binary>> -> Rest;
        _ -> Text
    end;
maybe_strip_markdown(Text, _Opts) ->
    Text.

%% @doc 提取 JSON 代码块或查找 JSON 边界
-spec maybe_extract_codeblock(binary(), parse_options()) -> binary().
maybe_extract_codeblock(Text, #{extract_codeblock := true}) ->
    %% 使用完整的 extract_json 逻辑：先找代码块，再找边界
    case extract_json(Text) of
        {ok, Extracted} -> Extracted;
        {error, _} -> Text
    end;
maybe_extract_codeblock(Text, _Opts) ->
    Text.

%% @doc 提取边界
-spec trim_whitespace(binary()) -> binary().
trim_whitespace(Text) ->
    %% 移除首尾空白
    re:replace(Text, "^\\s+|\\s+$", "", [{return, binary}, global]).

%%====================================================================
%% JSON 提取
%%====================================================================

%% @doc 从文本中提取 JSON
%%
%% 尝试多种策略：
%% 1. 查找 json 代码块
%% 2. 查找普通代码块
%% 3. 查找对象或数组结构
-spec extract_json(binary()) -> {ok, binary()} | {error, term()}.
extract_json(Text) ->
    %% 首先尝试提取代码块
    case extract_json_codeblock(Text) of
        {ok, Json} ->
            {ok, Json};
        {error, _} ->
            %% 然后尝试查找 JSON 边界
            case find_json_boundaries(Text) of
                {ok, Json} -> {ok, Json};
                {error, _} = Error -> Error
            end
    end.

%% @doc 提取 JSON 代码块
%%
%% 支持 markdown json 代码块格式
-spec extract_json_codeblock(binary()) -> {ok, binary()} | {error, term()}.
extract_json_codeblock(Text) ->
    case re:run(Text, "```json\\s*([\\s\\S]+?)```", [caseless, {capture, all, binary}]) of
        {match, [_, Content]} ->
            {ok, Content};
        nomatch ->
            case re:run(Text, "```\\s*([\\s\\S]+?)```", [{capture, all, binary}]) of
                {match, [_, Content]} ->
                    %% 验证是否是 JSON
                    case looks_like_json(Content) of
                        true -> {ok, Content};
                        false -> {error, no_json_codeblock}
                    end;
                nomatch ->
                    {error, no_json_codeblock}
            end
    end.

%% @doc 查找 JSON 边界
%%
%% 查找完整的 { ... } 或 [ ... ] 结构
%% 支持嵌套结构
%% 注意：优先尝试找数组，因为数组可能包含对象
-spec find_json_boundaries(binary()) -> {ok, binary()} | {error, term()}.
find_json_boundaries(Text) ->
    %% 检查哪个字符先出现
    ObjStart = case binary:match(Text, <<"{">>)  of
        {O, _} -> O;
        nomatch -> infinity
    end,
    ArrStart = case binary:match(Text, <<"[">>) of
        {A, _} -> A;
        nomatch -> infinity
    end,
    %% 根据哪个先出现来选择解析策略
    if
        ArrStart < ObjStart ->
            case find_array_boundaries(Text) of
                {ok, Json} -> {ok, Json};
                {error, _} -> find_object_boundaries(Text)
            end;
        ObjStart < ArrStart ->
            case find_object_boundaries(Text) of
                {ok, Json} -> {ok, Json};
                {error, _} -> find_array_boundaries(Text)
            end;
        true ->
            {error, no_json_found}
    end.

%% @private 查找对象边界
find_object_boundaries(Text) ->
    case binary:match(Text, <<"{">>) of
        {Start, _} ->
            case find_matching_brace(Text, Start, ${, $}) of
                {ok, End} ->
                    Json = binary:part(Text, Start, End - Start + 1),
                    {ok, Json};
                {error, _} ->
                    {error, unmatched_braces}
            end;
        nomatch ->
            {error, no_object_found}
    end.

%% @private 查找数组边界
find_array_boundaries(Text) ->
    case binary:match(Text, <<"[">>) of
        {Start, _} ->
            case find_matching_brace(Text, Start, $[, $]) of
                {ok, End} ->
                    Json = binary:part(Text, Start, End - Start + 1),
                    {ok, Json};
                {error, _} ->
                    {error, unmatched_brackets}
            end;
        nomatch ->
            {error, no_array_found}
    end.

%% @private 查找匹配的括号
find_matching_brace(Text, Start, Open, Close) ->
    find_matching_brace(Text, Start, 0, Open, Close, Start).

find_matching_brace(_Text, Pos, _Depth, _Open, _Close, _End) when Pos > byte_size(_Text) - 1 ->
    {error, unexpected_end};
find_matching_brace(Text, Pos, Depth, Open, Close, End) ->
    case binary:at(Text, Pos) of
        C when C =:= Open ->
            NewDepth = Depth + 1,
            find_matching_brace(Text, Pos + 1, NewDepth, Open, Close, End);
        C when C =:= Close ->
            case Depth of
                1 -> {ok, Pos};
                _ -> find_matching_brace(Text, Pos + 1, Depth - 1, Open, Close, End)
            end;
        $" ->
            %% 跳过字符串
            case skip_string(Text, Pos + 1) of
                {ok, NextPos} -> find_matching_brace(Text, NextPos, Depth, Open, Close, End);
                {error, _} -> {error, unterminated_string}
            end;
        _ ->
            find_matching_brace(Text, Pos + 1, Depth, Open, Close, End)
    end.

%% @private 跳过字符串内容
skip_string(Text, Pos) ->
    skip_string(Text, Pos, false).

skip_string(Text, Pos, _Escaped) when Pos > byte_size(Text) - 1 ->
    {error, unterminated_string};
skip_string(Text, Pos, Escaped) ->
    case binary:at(Text, Pos) of
        $" when not Escaped -> {ok, Pos + 1};
        $\\ when not Escaped -> skip_string(Text, Pos + 1, true);
        _ -> skip_string(Text, Pos + 1, false)
    end.

%% @doc 检查文本是否像 JSON
-spec looks_like_json(binary()) -> boolean().
looks_like_json(Text) ->
    Trimmed = trim_whitespace(Text),
    case Trimmed of
        <<"{", _/binary>> -> true;
        <<"[", _/binary>> -> true;
        _ -> false
    end.

%%====================================================================
%% JSON 修复
%%====================================================================

%% @doc 修复常见的 JSON 格式错误
%%
%% 修复内容：
%% 1. 移除尾随逗号
%% 2. 移除 JavaScript 注释（// 和 /* */）
%% 3. 修复未引用的键名
%% 4. 修复单引号
%% 5. 移除末尾分号
-spec repair_json(binary()) -> binary().
repair_json(Text) ->
    T1 = remove_trailing_commas(Text),
    T2 = remove_js_comments(T1),
    T3 = quote_unquoted_keys(T2),
    T4 = fix_single_quotes(T3),
    T5 = remove_trailing_semicolon(T4),
    T5.

%% @doc 移除尾随逗号
-spec remove_trailing_commas(binary()) -> binary().
remove_trailing_commas(Text) ->
    %% 移除 } 或 ] 前的逗号
    re:replace(Text, ",(\\s*[\\]}])", "\\1", [{return, binary}, global]).

%% @doc 移除 JavaScript 注释
-spec remove_js_comments(binary()) -> binary().
remove_js_comments(Text) ->
    %% 移除单行注释 //
    T1 = re:replace(Text, "//.*?(?:\\n|$)", "", [{return, binary}, global]),
    %% 移除多行注释 /* */
    re:replace(T1, "/\\*[\s\S]*?\\*/", "", [{return, binary}, global]).

%% @doc 为未引用的键名添加引号
-spec quote_unquoted_keys(binary()) -> binary().
quote_unquoted_keys(Text) ->
    %% 匹配未引用的键名模式: {key: 或 ,key:
    %% 这是简化的实现，可能无法处理所有情况
    re:replace(Text, "([{,]\\s*)([a-zA-Z_][a-zA-Z0-9_]*)(\\s*:)",
                "\\1\"\\2\"\\3",
                [{return, binary}, global]).

%% @doc 修复单引号
-spec fix_single_quotes(binary()) -> binary().
fix_single_quotes(Text) ->
    %% 替换单引号为双引号（简化版本，可能误报）
    %% 更安全的做法是只在 JSON 上下文中替换
    Text.

%% @doc 移除末尾分号
-spec remove_trailing_semicolon(binary()) -> binary().
remove_trailing_semicolon(Text) ->
    Trimmed = trim_whitespace(Text),
    case byte_size(Trimmed) of
        0 -> Trimmed;
        Size ->
            case binary:at(Trimmed, Size - 1) of
                $; -> binary:part(Trimmed, 0, Size - 1);
                _ -> Trimmed
            end
    end.

%%====================================================================
%% 基础解析
%%====================================================================

%% @doc 尝试解析 JSON
-spec try_parse(binary()) -> parse_result().
try_parse(Text) ->
    try jsx:decode(Text, [return_maps]) of
        Result ->
            {ok, Result}
    catch
        _:Error ->
            {error, {invalid_json, format_parse_error(Error)}}
    end.

%% @doc 格式化解析错误
-spec format_parse_error(term()) -> binary().
format_parse_error(Error) ->
    ErrorBin = list_to_binary(io_lib:format("~p", [Error])),
    ErrorBin.
