%%%-------------------------------------------------------------------
%%% @doc LLM 统一响应数据结构（beamai_core 核心模块）
%%%
%%% 提供统一的 LLM 响应格式，抽象不同 Provider 的差异，
%%% 同时保留原始数据以便获取 Provider 特有信息。
%%%
%%% 该模块位于 beamai_core 是因为：
%%% - 作为核心数据结构，被 LLM 层生成、被 Kernel 层消费
%%% - beamai_kernel 的工具调用循环需要可靠访问 tool_calls 和 content
%%% - 使用访问器函数比直接 map 模式匹配更健壮
%%%
%%% == 设计原则 ==
%%%
%%% 1. 统一访问：常用字段通过统一接口访问
%%% 2. 保留原始：原始响应完整保存，支持 Provider 特有字段
%%% 3. 类型安全：统一的类型定义，避免运行时错误
%%% 4. 可扩展：易于添加新 Provider 支持
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 通过 new/1 构造
%%% Resp = beamai_llm_response:new(#{id => <<"123">>, content => <<"Hello">>}),
%%%
%%% %% 统一接口访问
%%% Content = beamai_llm_response:content(Resp),
%%% ToolCalls = beamai_llm_response:tool_calls(Resp),
%%% Usage = beamai_llm_response:usage(Resp),
%%%
%%% %% 获取 Provider 特有信息
%%% Raw = beamai_llm_response:raw(Resp),
%%% CacheTokens = maps:get(<<"cache_creation_input_tokens">>,
%%%                        maps:get(<<"usage">>, Raw, #{}), 0).
%%% ```
%%%
%%% Provider 特定的响应解析（from_openai/1, from_anthropic/1 等）
%%% 已迁移至 beamai_llm 的 beamai_llm_response_parser 模块。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_response).

%% 构造函数
-export([new/1]).

%% 统一访问接口
-export([id/1, model/1, provider/1]).
-export([content/1, content_blocks/1, thinking/1, reasoning_content/1]).
-export([tool_calls/1, has_tool_calls/1]).
-export([finish_reason/1, is_complete/1, needs_tool_call/1]).
-export([usage/1, input_tokens/1, output_tokens/1, total_tokens/1]).

%% 原始数据访问
-export([raw/1, raw_get/2, raw_get/3]).

%% 元数据
-export([metadata/1, set_metadata/3]).

%% 序列化
-export([to_map/1]).

%% 类型定义
-type response() :: #{
    '__struct__' := ?MODULE,
    id := binary(),
    model := binary(),
    provider := provider(),
    content := binary() | null,
    content_blocks := [content_block()],
    tool_calls := [tool_call()],
    finish_reason := finish_reason(),
    usage := usage(),
    raw := map(),
    metadata := map()
}.

-type provider() :: openai | anthropic | deepseek | zhipu | ollama | dashscope | unknown.

-type content_block() ::
    #{type := text, text := binary()} |
    #{type := thinking, thinking := binary(), signature := binary()} |
    #{type := redacted_thinking, data := binary()} |
    #{type := tool_use, id := binary(), name := binary(), input := map()}.

-type tool_call() :: #{
    id := binary(),
    name := binary(),
    arguments := map(),
    raw_arguments := binary()
}.

-type finish_reason() ::
    complete |           % 正常完成
    tool_use |           % 需要工具调用
    length_limit |       % 达到长度限制
    content_filtered |   % 内容过滤
    stop_sequence |      % 停止序列触发
    pause_turn |         % 长运行暂停（Anthropic）
    refusal |            % 内容策略拒绝（Anthropic）
    error |              % 错误
    unknown.             % 未知

-type usage() :: #{
    input_tokens := non_neg_integer(),
    output_tokens := non_neg_integer(),
    total_tokens := non_neg_integer(),
    details => map()  % Provider 特有的详细统计
}.

-export_type([response/0, provider/0, content_block/0, tool_call/0, finish_reason/0, usage/0]).

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 从字段 map 构造标准 response() 结构
-spec new(map()) -> response().
new(Fields) ->
    #{
        '__struct__' => ?MODULE,
        id => maps:get(id, Fields, <<>>),
        model => maps:get(model, Fields, <<>>),
        provider => maps:get(provider, Fields, unknown),
        content => maps:get(content, Fields, null),
        content_blocks => maps:get(content_blocks, Fields, []),
        tool_calls => maps:get(tool_calls, Fields, []),
        finish_reason => maps:get(finish_reason, Fields, unknown),
        usage => maps:get(usage, Fields, #{input_tokens => 0, output_tokens => 0, total_tokens => 0}),
        raw => maps:get(raw, Fields, #{}),
        metadata => maps:get(metadata, Fields, #{})
    }.

%%====================================================================
%% 统一访问接口
%%====================================================================

%% @doc 获取响应 ID
-spec id(response()) -> binary().
id(#{id := Id}) -> Id.

%% @doc 获取模型名称
-spec model(response()) -> binary().
model(#{model := Model}) -> Model.

%% @doc 获取 Provider 类型
-spec provider(response()) -> provider().
provider(#{provider := Provider}) -> Provider.

%% @doc 获取文本内容（合并后的）
-spec content(response()) -> binary() | null.
content(#{content := Content}) -> Content;
content(_) -> null.

%% @doc 获取原始内容块列表
-spec content_blocks(response()) -> [content_block()].
content_blocks(#{content_blocks := Blocks}) -> Blocks;
content_blocks(_) -> [].

%% @doc 获取 thinking 内容（Anthropic extended thinking）
%% 从 content_blocks 中提取 thinking 块的文本，合并返回
-spec thinking(response()) -> binary() | null.
thinking(#{content_blocks := Blocks}) ->
    ThinkingTexts = [T || #{type := thinking, thinking := T} <- Blocks],
    case ThinkingTexts of
        [] -> null;
        _ -> iolist_to_binary(ThinkingTexts)
    end;
thinking(_) -> null.

%% @doc 获取推理内容（智谱 GLM-4.6+ 特有）
%% 返回 reasoning_content，如果不存在则返回 null
-spec reasoning_content(response()) -> binary() | null.
reasoning_content(#{metadata := #{reasoning_content := RC}}) -> RC;
reasoning_content(_) -> null.

%% @doc 获取工具调用列表
-spec tool_calls(response()) -> [tool_call()].
tool_calls(#{tool_calls := Calls}) -> Calls;
tool_calls(_) -> [].

%% @doc 是否有工具调用
-spec has_tool_calls(response()) -> boolean().
has_tool_calls(#{tool_calls := []}) -> false;
has_tool_calls(#{tool_calls := Calls}) when is_list(Calls), Calls =/= [] -> true;
has_tool_calls(_) -> false.

%% @doc 获取统一的结束原因
-spec finish_reason(response()) -> finish_reason().
finish_reason(#{finish_reason := Reason}) -> Reason;
finish_reason(_) -> unknown.

%% @doc 是否正常完成（无需进一步操作）
-spec is_complete(response()) -> boolean().
is_complete(#{finish_reason := complete}) -> true;
is_complete(_) -> false.

%% @doc 是否需要执行工具调用
-spec needs_tool_call(response()) -> boolean().
needs_tool_call(#{finish_reason := tool_use}) -> true;
needs_tool_call(#{tool_calls := Calls}) when length(Calls) > 0 -> true;
needs_tool_call(_) -> false.

%% @doc 获取 Token 使用统计
-spec usage(response()) -> usage().
usage(#{usage := Usage}) -> Usage;
usage(_) -> #{}.

%% @doc 获取输入 Token 数
-spec input_tokens(response()) -> non_neg_integer().
input_tokens(#{usage := #{input_tokens := N}}) -> N;
input_tokens(_) -> 0.

%% @doc 获取输出 Token 数
-spec output_tokens(response()) -> non_neg_integer().
output_tokens(#{usage := #{output_tokens := N}}) -> N;
output_tokens(_) -> 0.

%% @doc 获取总 Token 数
-spec total_tokens(response()) -> non_neg_integer().
total_tokens(#{usage := #{total_tokens := N}}) -> N;
total_tokens(_) -> 0.

%%====================================================================
%% 原始数据访问
%%====================================================================

%% @doc 获取原始响应数据
-spec raw(response()) -> map().
raw(#{raw := Raw}) -> Raw.

%% @doc 从原始数据中获取指定路径的值
-spec raw_get(response(), [binary()] | binary()) -> term() | undefined.
raw_get(Resp, Key) ->
    raw_get(Resp, Key, undefined).

%% @doc 从原始数据中获取指定路径的值，带默认值
-spec raw_get(response(), [binary()] | binary(), term()) -> term().
raw_get(#{raw := Raw}, Path, Default) when is_list(Path) ->
    get_nested(Raw, Path, Default);
raw_get(#{raw := Raw}, Key, Default) when is_binary(Key) ->
    maps:get(Key, Raw, Default).

%%====================================================================
%% 元数据
%%====================================================================

%% @doc 获取元数据
-spec metadata(response()) -> map().
metadata(#{metadata := Meta}) -> Meta.

%% @doc 设置元数据字段
-spec set_metadata(response(), term(), term()) -> response().
set_metadata(#{metadata := Meta} = Resp, Key, Value) ->
    Resp#{metadata => Meta#{Key => Value}}.

%%====================================================================
%% 序列化
%%====================================================================

%% @doc 转换为普通 map（不含 __struct__）
-spec to_map(response()) -> map().
to_map(Resp) ->
    maps:remove('__struct__', Resp).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 获取嵌套值
get_nested(Map, [], _Default) -> Map;
get_nested(Map, [Key | Rest], Default) when is_map(Map) ->
    case maps:get(Key, Map, undefined) of
        undefined -> Default;
        Value -> get_nested(Value, Rest, Default)
    end;
get_nested(_, _, Default) -> Default.
