%%%-------------------------------------------------------------------
%%% @doc Output Parser 统一入口
%%%
%%% 提供类似 LangChain 的 Output Parser 功能，用于解析 LLM 的文本输出。
%%%
%%% 主要功能：
%%% - JSON Parser: 容错解析 LLM 输出的 JSON
%%% - 格式指令生成: 自动生成格式说明
%%% - 解析重试: 失败时自动重试
%%%
%%% 使用方法：
%%% - JSON 解析: beamai_output_parser:parse(json(), Text)
%%% - 获取格式指令: beamai_output_parser:get_instructions(json)
%%% - 重试解析: beamai_output_parser:parse_with_retry(Parser, Text, 3)
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_output_parser).

%%====================================================================
%% 导出 API
%%====================================================================

%% Parser 创建
-export([new/2, json/0, json/1, xml/0, csv/0]).

%% 解析操作
-export([parse/2, parse/3]).
-export([parse_with_retry/3, parse_with_retry/4]).

%% 格式指令
-export([get_instructions/1, get_instructions/2]).

%% 错误处理
-export([is_retryable_error/1]).

%% 类型导出
-export_type([parser/0, parse_result/0, format/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type format() :: json | xml | csv | raw.

-type parser() :: #{
    type := format(),
    options := map()
}.

-type parse_result() :: {ok, term()} | {error, parse_error()}.

-type parse_error() ::
    {invalid_json, binary()} |
    {invalid_xml, binary()} |
    {invalid_csv, binary()} |
    {extract_failed, binary()} |
    {max_retries_exceeded, [parse_error()]}.

%%====================================================================
%% Parser 创建 API
%%====================================================================

%% @doc 创建通用 Parser
-spec new(format(), map()) -> parser().
new(Type, Options) ->
    #{type => Type, options => Options}.

%% @doc 创建 JSON Parser（默认选项）
-spec json() -> parser().
json() ->
    json(#{}).

%% @doc 创建 JSON Parser（带选项）
%%
%% 选项：
%% - schema: JSON Schema 用于验证（可选）
%% - extract_codeblock: 是否提取 json 代码块（默认 true）
%% - repair_common: 是否修复常见格式错误（默认 true）
%% - strip_markdown: 是否移除 markdown 格式（默认 true）
-spec json(map()) -> parser().
json(Options) ->
    DefaultOpts = #{
        extract_codeblock => true,
        repair_common => true,
        strip_markdown => true
    },
    #{type => json, options => maps:merge(DefaultOpts, Options)}.

%% @doc 创建 XML Parser
-spec xml() -> parser().
xml() ->
    #{type => xml, options => #{}}.

%% @doc 创建 CSV Parser
-spec csv() -> parser().
csv() ->
    #{type => csv, options => #{}}.

%%====================================================================
%% 解析 API
%%====================================================================

%% @doc 解析文本（使用默认选项）
-spec parse(parser(), binary()) -> parse_result().
parse(#{type := Type, options := Opts}, Text) ->
    do_parse(Type, Text, Opts).

%% @doc 解析文本（带覆盖选项）
-spec parse(parser(), binary(), map()) -> parse_result().
parse(#{type := Type, options := BaseOpts}, Text, OverrideOpts) ->
    Opts = maps:merge(BaseOpts, OverrideOpts),
    do_parse(Type, Text, Opts).

%% @private 分派到具体解析器
do_parse(json, Text, Opts) ->
    beamai_parser_json:parse(Text, Opts);
do_parse(xml, _Text, _Opts) ->
    {error, {not_implemented, xml}};
do_parse(csv, _Text, _Opts) ->
    {error, {not_implemented, csv}};
do_parse(raw, Text, _Opts) ->
    {ok, Text}.

%%====================================================================
%% 重试解析 API
%%====================================================================

%% @doc 带重试的解析（默认最多 3 次）
-spec parse_with_retry(parser(), binary(), non_neg_integer()) -> parse_result().
parse_with_retry(Parser, Text, MaxRetries) ->
    parse_with_retry(Parser, Text, MaxRetries, #{}).

%% @doc 带重试的解析（完整选项）
%%
%% 选项：
%% - on_retry: 每次重试前的回调 fun(Error, Attempt) -> ok
-spec parse_with_retry(parser(), binary(), non_neg_integer(), map()) -> parse_result().
parse_with_retry(#{type := _Type} = Parser, Text, MaxRetries, Opts) ->
    beamai_parser_retry:parse(Parser, Text, MaxRetries, Opts).

%%====================================================================
%% 格式指令 API
%%====================================================================

%% @doc 获取格式指令
-spec get_instructions(format()) -> binary().
get_instructions(Format) ->
    get_instructions(Format, #{}).

%% @doc 获取格式指令（带选项）
%%
%% JSON 选项：
%% - schema: JSON Schema，会包含在指令中
%% - examples: 示例列表
-spec get_instructions(format(), map()) -> binary().
get_instructions(json, Opts) ->
    beamai_parser_instructions:json(Opts);
get_instructions(xml, _Opts) ->
    beamai_parser_instructions:xml();
get_instructions(csv, _Opts) ->
    beamai_parser_instructions:csv();
get_instructions(raw, _Opts) ->
    <<>>.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @doc 判断是否需要重试
-spec is_retryable_error(parse_error()) -> boolean().
is_retryable_error({invalid_json, _}) -> true;
is_retryable_error({extract_failed, _}) -> true;
is_retryable_error({max_retries_exceeded, _}) -> false;
is_retryable_error({not_implemented, _}) -> false;
is_retryable_error({invalid_xml, _}) -> true;
is_retryable_error({invalid_csv, _}) -> true.
