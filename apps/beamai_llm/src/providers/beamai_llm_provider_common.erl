%%%-------------------------------------------------------------------
%%% @doc LLM Provider 公共函数模块
%%%
%%% 抽取多个 Provider 共用的函数，减少代码重复。
%%% 遵循 DRY 原则，提供以下公共功能：
%%%
%%% == URL 构建 ==
%%% - build_url/3: 构建请求 URL（base_url + endpoint）
%%%
%%% == 请求头构建 ==
%%% - build_bearer_auth_headers/1: 构建 Bearer Token 认证头
%%%
%%% == 请求体构建辅助 ==
%%% - maybe_add_stream/2: 添加流式标志
%%% - maybe_add_tools/2: 添加 OpenAI 格式工具定义
%%% - maybe_add_tool_choice/2: 添加 OpenAI 格式 tool_choice
%%% - maybe_add_top_p/2: 添加 top_p 参数
%%% - maybe_add_params/3: 按映射表批量添加可选参数
%%%
%%% == 流式响应累加 ==
%%% - accumulate_openai_event/2: OpenAI 格式事件累加器
%%%   （支持 content / reasoning_content / 分片 tool_calls / usage）
%%% - finalize_openai_stream/2: 将累加结果重建为 OpenAI 原始响应
%%%   并经 beamai_llm_response_parser 解析为统一响应
%%% - accumulate_anthropic_event/2: Anthropic 格式事件累加器
%%%   （message_start / content_block_* / message_delta / error）
%%% - finalize_anthropic_stream/1: 将累加结果重建为 Anthropic 原始
%%%   响应并解析为统一响应
%%%
%%% == 响应解析 ==
%%% - parse_tool_calls/1: 解析 OpenAI 格式工具调用
%%% - parse_single_tool_call/1: 解析单个工具调用
%%% - parse_usage/1: 解析 OpenAI 格式使用统计
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 在 Provider 模块中使用
%%% -module(llm_provider_xxx).
%%%
%%% build_url(Config, DefaultEndpoint) ->
%%%     beamai_llm_provider_common:build_url(Config, DefaultEndpoint, ?XXX_BASE_URL).
%%%
%%% build_headers(Config) ->
%%%     beamai_llm_provider_common:build_bearer_auth_headers(Config).
%%%
%%% build_request_body(Config, Request) ->
%%%     Base = #{...},
%%%     ?BUILD_BODY_PIPELINE(Base, [
%%%         fun(B) -> beamai_llm_provider_common:maybe_add_stream(B, Request) end,
%%%         fun(B) -> beamai_llm_provider_common:maybe_add_tools(B, Request) end
%%%     ]).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_provider_common).

%% API 导出
-export([
    %% URL 构建
    build_url/3,

    %% 请求头构建
    build_bearer_auth_headers/1,

    %% 请求体辅助函数
    maybe_add_stream/2,
    maybe_add_tools/2,
    maybe_add_tool_choice/2,
    maybe_add_top_p/2,
    maybe_add_params/3,

    %% 流式响应累加
    accumulate_openai_event/2,
    finalize_openai_stream/2,
    accumulate_anthropic_event/2,
    finalize_anthropic_stream/1,
    accumulate_completions_event/2,
    finalize_completions_stream/2,

    %% 响应解析
    parse_tool_calls/1,
    parse_single_tool_call/1,
    parse_usage/1,

    %% 响应头
    rate_limit_metadata/1,
    retry_after_ms/1
]).

%%====================================================================
%% URL 构建
%%====================================================================

%% @doc 构建请求 URL
%%
%% 将 base_url 和 endpoint 拼接成完整 URL。
%% 支持通过 Config 覆盖默认值，便于第三方 API 代理兼容
%% （如 one-api、new-api 等）。
%%
%% @param Config Provider 配置 map，可包含 base_url 和 endpoint
%% @param DefaultEndpoint 默认端点路径
%% @param DefaultBaseUrl 默认基础 URL
%% @returns 完整请求 URL（binary）
-spec build_url(map(), binary(), binary()) -> binary().
build_url(Config, DefaultEndpoint, DefaultBaseUrl) ->
    BaseUrl = maps:get(base_url, Config, DefaultBaseUrl),
    Endpoint = maps:get(endpoint, Config, DefaultEndpoint),
    <<BaseUrl/binary, Endpoint/binary>>.

%%====================================================================
%% 请求头构建
%%====================================================================

%% @doc 构建 Bearer Token 认证请求头
%%
%% 生成标准的 Bearer Token 认证头，用于 OpenAI 兼容 API。
%% 包含 Authorization 和 Content-Type 头。
%%
%% @param Config Provider 配置 map，必须包含 api_key
%% @returns 请求头列表 [{Name, Value}]
-spec build_bearer_auth_headers(map()) -> [{binary(), binary()}].
build_bearer_auth_headers(#{api_key := ApiKey}) ->
    [
        {<<"Authorization">>, <<"Bearer ", ApiKey/binary>>},
        {<<"Content-Type">>, <<"application/json">>}
    ].

%%====================================================================
%% 请求体辅助函数
%%====================================================================

%% @doc 根据请求参数添加流式标志
%%
%% 如果请求中包含 stream => true，则在请求体中添加流式标志。
%%
%% @param Body 当前请求体 map
%% @param Request 原始请求参数 map
%% @returns 更新后的请求体 map
-spec maybe_add_stream(map(), map()) -> map().
maybe_add_stream(Body, #{stream := true}) -> Body#{<<"stream">> => true};
maybe_add_stream(Body, _) -> Body.

%% @doc 根据请求参数添加工具定义
%%
%% 如果请求中包含非空的 tools 列表，则添加 OpenAI 格式的工具定义。
%% 同时设置 tool_choice 为 "auto"（如果未指定）。
%%
%% @param Body 当前请求体 map
%% @param Request 原始请求参数 map
%% @returns 更新后的请求体 map
-spec maybe_add_tools(map(), map()) -> map().
maybe_add_tools(Body, #{tools := Tools}) when Tools =/= [] ->
    FormattedTools = beamai_llm_tool_adapter:to_openai(Tools),
    ToolChoice = maps:get(tool_choice, Body, <<"auto">>),
    Body#{<<"tools">> => FormattedTools, <<"tool_choice">> => ToolChoice};
maybe_add_tools(Body, _) ->
    Body.

%% @doc 根据配置添加 top_p 参数
%%
%% 如果配置中包含 top_p 且为有效数值，则添加到请求体。
%%
%% @param Body 当前请求体 map
%% @param Config Provider 配置 map
%% @returns 更新后的请求体 map
-spec maybe_add_top_p(map(), map()) -> map().
maybe_add_top_p(Body, #{top_p := TopP}) when is_number(TopP) ->
    Body#{<<"top_p">> => TopP};
maybe_add_top_p(Body, _) ->
    Body.

%% @doc 添加 OpenAI 格式的 tool_choice
%%
%% 支持以下取值（与 OpenAI API 对应）：
%%   - auto | none | required: 字符串形式
%%   - {tool, Name}: 指定函数 #{type => function, function => #{name}}
%%   - binary: 直接传入（"auto"/"none"/"required"）
%%   - map: 高级用法，直接透传
%%
%% @param Body 当前请求体 map
%% @param Request 原始请求参数 map
%% @returns 更新后的请求体 map
-spec maybe_add_tool_choice(map(), map()) -> map().
maybe_add_tool_choice(Body, #{tool_choice := auto}) ->
    Body#{<<"tool_choice">> => <<"auto">>};
maybe_add_tool_choice(Body, #{tool_choice := none}) ->
    Body#{<<"tool_choice">> => <<"none">>};
maybe_add_tool_choice(Body, #{tool_choice := required}) ->
    Body#{<<"tool_choice">> => <<"required">>};
maybe_add_tool_choice(Body, #{tool_choice := {tool, Name}}) when is_binary(Name) ->
    Body#{<<"tool_choice">> => #{
        <<"type">> => <<"function">>,
        <<"function">> => #{<<"name">> => Name}
    }};
maybe_add_tool_choice(Body, #{tool_choice := Choice}) when is_binary(Choice); is_map(Choice) ->
    Body#{<<"tool_choice">> => Choice};
maybe_add_tool_choice(Body, _) ->
    Body.

%% @doc 按映射表批量添加可选参数
%%
%% 遍历 {SourceKey, JsonKey} 映射表，将 Source 中存在的参数
%% 复制到请求体。值为 undefined 时跳过。
%%
%% ```erlang
%% maybe_add_params(Body, Config, [
%%     {frequency_penalty, <<"frequency_penalty">>},
%%     {seed, <<"seed">>}
%% ])
%% ```
%%
%% @param Body 当前请求体 map
%% @param Source 参数来源 map（Config 或 Request）
%% @param Specs 参数映射表 [{atom(), binary()}]
%% @returns 更新后的请求体 map
-spec maybe_add_params(map(), map(), [{atom(), binary()}]) -> map().
maybe_add_params(Body, Source, Specs) ->
    lists:foldl(fun({Key, JsonKey}, B) ->
        case maps:get(Key, Source, undefined) of
            undefined -> B;
            Value -> B#{JsonKey => Value}
        end
    end, Body, Specs).

%%====================================================================
%% 流式响应累加
%%====================================================================

%% @doc OpenAI 格式流式事件累加器
%%
%% 累加 OpenAI 兼容 API 的 SSE 事件，参考 Spring AI 的 ChunkMerger：
%%   - content / reasoning_content: 逐片段拼接
%%   - tool_calls: 按 index 分组累加（id/name 出现在首个分片，
%%     function.arguments 跨分片拼接）
%%   - usage: 捕获末尾 usage chunk（stream_options.include_usage）
%%   - id / model / finish_reason: 取最新非空值
%%
%% 此函数作为 beamai_llm_http_client:stream_request 的 accumulator 参数使用。
%%
%% @param Event 解析后的 SSE 事件 map
%% @param Acc 当前累加器 map（需包含 content 字段）
%% @returns 更新后的累加器 map
-spec accumulate_openai_event(map(), map()) -> map().
accumulate_openai_event(#{<<"choices">> := [Choice | _]} = Event, Acc) when is_map(Choice) ->
    Delta = maps:get(<<"delta">>, Choice, #{}),
    Acc1 = merge_openai_event_meta(Event, Acc),
    Acc2 = append_delta_text(content, maps:get(<<"content">>, Delta, <<>>), Acc1),
    Acc3 = append_delta_text(reasoning_content, maps:get(<<"reasoning_content">>, Delta, <<>>), Acc2),
    Acc4 = accumulate_delta_tool_calls(maps:get(<<"tool_calls">>, Delta, []), Acc3),
    Acc5 = update_finish_reason(maps:get(<<"finish_reason">>, Choice, null), Acc4),
    capture_raw_usage(Event, Acc5);
accumulate_openai_event(#{<<"usage">> := Usage} = Event, Acc) when is_map(Usage) ->
    %% include_usage 模式：末尾 chunk 的 choices 为空，仅携带 usage
    capture_raw_usage(Event, merge_openai_event_meta(Event, Acc));
accumulate_openai_event(_, Acc) ->
    Acc.

%% @doc 将 OpenAI 流式累加结果转换为统一响应
%%
%% 重建为 OpenAI 原始响应格式（choices[0].message），
%% 再交给 beamai_llm_response_parser 解析，使流式响应与
%% 同步响应结构完全一致（含 tool_calls / usage / finish_reason）。
%%
%% 此函数可作为 beamai_llm_http_client:stream_request 的 finalizer 使用：
%% ```erlang
%% Opts = #{finalizer => fun(Acc) ->
%%     beamai_llm_provider_common:finalize_openai_stream(Acc, deepseek)
%% end}
%% ```
%%
%% @param Acc 流式累加器
%% @param Provider Provider 标识（决定使用哪个解析器）
%% @returns {ok, 统一响应} | {error, Reason}
-spec finalize_openai_stream(map(), atom()) -> {ok, map()} | {error, term()}.
finalize_openai_stream(Acc, Provider) ->
    Raw = build_openai_raw_response(Acc),
    beamai_llm_response_parser:from_provider(Raw, Provider).

%% @private 合并事件级元信息（id/model）
merge_openai_event_meta(Event, Acc) ->
    Acc#{
        id => first_nonempty(maps:get(<<"id">>, Event, <<>>), maps:get(id, Acc, <<>>)),
        model => first_nonempty(maps:get(<<"model">>, Event, <<>>), maps:get(model, Acc, <<>>))
    }.

%% @private 取首个非空 binary
first_nonempty(New, _Old) when is_binary(New), New =/= <<>> -> New;
first_nonempty(_, Old) -> Old.

%% @private 追加 delta 文本片段（content / reasoning_content）
append_delta_text(_Key, Fragment, Acc) when Fragment =:= <<>>; Fragment =:= null ->
    Acc;
append_delta_text(Key, Fragment, Acc) when is_binary(Fragment) ->
    Acc#{Key => <<(maps:get(Key, Acc, <<>>))/binary, Fragment/binary>>};
append_delta_text(_Key, _Fragment, Acc) ->
    Acc.

%% @private 按 index 累加分片 tool_calls
%% OpenAI 流式工具调用：首个分片携带 id 和 function.name，
%% 后续分片仅携带 function.arguments 片段，按 index 关联。
accumulate_delta_tool_calls(Deltas, Acc) when is_list(Deltas), Deltas =/= [] ->
    Map0 = maps:get(tool_call_acc, Acc, #{}),
    Map1 = lists:foldl(fun merge_tool_call_delta/2, Map0, Deltas),
    Acc#{tool_call_acc => Map1};
accumulate_delta_tool_calls(_, Acc) ->
    Acc.

%% @private 合并单个 tool_call 分片
merge_tool_call_delta(TC, Map) when is_map(TC) ->
    Index = maps:get(<<"index">>, TC, 0),
    Entry0 = maps:get(Index, Map, #{id => <<>>, name => <<>>, arguments => <<>>}),
    Func = maps:get(<<"function">>, TC, #{}),
    Entry1 = Entry0#{
        id => first_nonempty(maps:get(<<"id">>, TC, <<>>), maps:get(id, Entry0)),
        name => first_nonempty(maps:get(<<"name">>, Func, <<>>), maps:get(name, Entry0))
    },
    Entry2 = case maps:get(<<"arguments">>, Func, <<>>) of
        Frag when is_binary(Frag), Frag =/= <<>> ->
            Entry1#{arguments => <<(maps:get(arguments, Entry1))/binary, Frag/binary>>};
        _ ->
            Entry1
    end,
    Map#{Index => Entry2};
merge_tool_call_delta(_, Map) ->
    Map.

%% @private 更新完成原因（保留最新非空值）
update_finish_reason(FR, Acc) when is_binary(FR), FR =/= <<>> ->
    Acc#{finish_reason => FR};
update_finish_reason(_, Acc) ->
    Acc.

%% @private 捕获原始 usage（binary key 格式，供解析器复用）
capture_raw_usage(#{<<"usage">> := Usage}, Acc) when is_map(Usage), map_size(Usage) > 0 ->
    Acc#{usage_raw => maps:merge(maps:get(usage_raw, Acc, #{}), Usage)};
capture_raw_usage(_, Acc) ->
    Acc.

%% @private 从累加器重建 OpenAI 原始响应
build_openai_raw_response(Acc) ->
    Message = build_openai_raw_message(Acc),
    FinishReason = case maps:get(finish_reason, Acc, <<>>) of
        <<>> -> <<"stop">>;
        FR when is_binary(FR) -> FR;
        _ -> <<"stop">>
    end,
    #{
        <<"id">> => maps:get(id, Acc, <<>>),
        <<"model">> => maps:get(model, Acc, <<>>),
        <<"object">> => <<"chat.completion">>,
        <<"choices">> => [#{
            <<"index">> => 0,
            <<"message">> => Message,
            <<"finish_reason">> => FinishReason
        }],
        <<"usage">> => maps:get(usage_raw, Acc, #{})
    }.

%% @private 重建 assistant 消息（含累加的 tool_calls / reasoning_content）
build_openai_raw_message(Acc) ->
    Content = case maps:get(content, Acc, <<>>) of
        <<>> -> null;
        C -> C
    end,
    Base = #{<<"role">> => <<"assistant">>, <<"content">> => Content},
    Base1 = case build_raw_tool_calls(maps:get(tool_call_acc, Acc, #{})) of
        [] -> Base;
        Calls -> Base#{<<"tool_calls">> => Calls}
    end,
    case maps:get(reasoning_content, Acc, <<>>) of
        <<>> -> Base1;
        RC -> Base1#{<<"reasoning_content">> => RC}
    end.

%% @private 将 index 分组的工具调用转为 OpenAI 原始格式（按 index 排序）
build_raw_tool_calls(ToolCallMap) ->
    Indexes = lists:sort(maps:keys(ToolCallMap)),
    [begin
         #{id := Id, name := Name, arguments := Args} = maps:get(I, ToolCallMap),
         #{
             <<"id">> => Id,
             <<"type">> => <<"function">>,
             <<"function">> => #{<<"name">> => Name, <<"arguments">> => Args}
         }
     end || I <- Indexes].

%%====================================================================
%% Completions（文本补全）流式响应累加
%%====================================================================

%% @doc OpenAI 兼容 Completions 格式流式事件累加器
%%
%% 用于文本补全类接口（如 DeepSeek FIM /beta/completions）。
%% 与 Chat 格式的区别：文本片段直接在 choices[0].text，没有 delta 包装。
%%
%% @param Event 解析后的 SSE 事件 map
%% @param Acc 当前累加器 map
%% @returns 更新后的累加器 map
-spec accumulate_completions_event(map(), map()) -> map().
accumulate_completions_event(#{<<"choices">> := [Choice | _]} = Event, Acc) when is_map(Choice) ->
    Acc1 = merge_openai_event_meta(Event, Acc),
    Acc2 = append_delta_text(content, maps:get(<<"text">>, Choice, <<>>), Acc1),
    Acc3 = update_finish_reason(maps:get(<<"finish_reason">>, Choice, null), Acc2),
    capture_raw_usage(Event, Acc3);
accumulate_completions_event(#{<<"usage">> := Usage} = Event, Acc) when is_map(Usage) ->
    capture_raw_usage(Event, merge_openai_event_meta(Event, Acc));
accumulate_completions_event(_, Acc) ->
    Acc.

%% @doc 将 Completions 流式累加结果转换为统一响应
%%
%% 重建为 Completions 原始响应格式（choices[0].text），
%% 再交给传入的解析器函数解析为统一响应。
%%
%% @param Acc 流式累加器
%% @param Parser 响应解析器函数（如 beamai_llm_response_parser:parser_deepseek_fim()）
%% @returns {ok, 统一响应} | {error, Reason}
-spec finalize_completions_stream(map(), fun((map()) -> {ok, map()} | {error, term()})) ->
    {ok, map()} | {error, term()}.
finalize_completions_stream(Acc, Parser) ->
    FinishReason = case maps:get(finish_reason, Acc, <<>>) of
        <<>> -> <<"stop">>;
        FR when is_binary(FR) -> FR;
        _ -> <<"stop">>
    end,
    Raw = #{
        <<"id">> => maps:get(id, Acc, <<>>),
        <<"model">> => maps:get(model, Acc, <<>>),
        <<"object">> => <<"text_completion">>,
        <<"choices">> => [#{
            <<"index">> => 0,
            <<"text">> => maps:get(content, Acc, <<>>),
            <<"finish_reason">> => FinishReason
        }],
        <<"usage">> => maps:get(usage_raw, Acc, #{})
    },
    Parser(Raw).

%%====================================================================
%% Anthropic 流式响应累加
%%====================================================================

%% @doc Anthropic 格式流式事件累加器
%%
%% 按 Anthropic Messages API 的事件流协议累加，参考 Spring AI
%% AnthropicChatModel 的 StreamingState：
%%   - message_start: 提取 id / model / usage.input_tokens
%%   - content_block_start: 按 index 初始化内容块
%%     （text / tool_use / thinking / redacted_thinking）
%%   - content_block_delta: 按 delta 类型累加
%%     （text_delta / input_json_delta / thinking_delta / signature_delta）
%%   - message_delta: 提取 stop_reason / stop_sequence / usage.output_tokens
%%   - error: 记录错误，finalize 时返回 {error, ...}
%%   - ping / content_block_stop: 无操作
%%
%% @param Event 解析后的 SSE 事件 map
%% @param Acc 当前累加器 map
%% @returns 更新后的累加器 map
-spec accumulate_anthropic_event(map(), map()) -> map().
accumulate_anthropic_event(#{<<"type">> := <<"message_start">>, <<"message">> := Msg}, Acc) ->
    Acc1 = Acc#{
        id => maps:get(<<"id">>, Msg, <<>>),
        model => maps:get(<<"model">>, Msg, <<>>)
    },
    capture_raw_usage(Msg, Acc1);
accumulate_anthropic_event(#{<<"type">> := <<"content_block_start">>,
                             <<"index">> := Index,
                             <<"content_block">> := Block}, Acc) ->
    Blocks = maps:get(blocks, Acc, #{}),
    Acc#{blocks => Blocks#{Index => init_anthropic_block(Block)}};
accumulate_anthropic_event(#{<<"type">> := <<"content_block_delta">>,
                             <<"index">> := Index,
                             <<"delta">> := Delta}, Acc) ->
    Blocks = maps:get(blocks, Acc, #{}),
    case maps:get(Index, Blocks, undefined) of
        undefined -> Acc;
        Block ->
            Acc#{blocks => Blocks#{Index => apply_anthropic_delta(Delta, Block)}}
    end;
accumulate_anthropic_event(#{<<"type">> := <<"message_delta">>} = Event, Acc) ->
    Delta = maps:get(<<"delta">>, Event, #{}),
    Acc1 = update_finish_reason(maps:get(<<"stop_reason">>, Delta, null), Acc),
    Acc2 = case maps:get(<<"stop_sequence">>, Delta, null) of
        null -> Acc1;
        Seq -> Acc1#{stop_sequence => Seq}
    end,
    capture_raw_usage(Event, Acc2);
accumulate_anthropic_event(#{<<"type">> := <<"error">>, <<"error">> := Error}, Acc) ->
    Acc#{error => Error};
accumulate_anthropic_event(_, Acc) ->
    %% ping / content_block_stop / message_stop 等无需处理
    Acc.

%% @doc 将 Anthropic 流式累加结果转换为统一响应
%%
%% 重建为 Anthropic 原始响应格式（content 块列表），
%% 再交给 beamai_llm_response_parser:from_anthropic 解析，
%% 使流式响应与同步响应结构完全一致
%% （含 tool_calls / thinking 块 / usage / finish_reason）。
%%
%% @param Acc 流式累加器
%% @returns {ok, 统一响应} | {error, Reason}
-spec finalize_anthropic_stream(map()) -> {ok, map()} | {error, term()}.
finalize_anthropic_stream(#{error := Error}) ->
    {error, {api_error, Error}};
finalize_anthropic_stream(Acc) ->
    Raw = #{
        <<"id">> => maps:get(id, Acc, <<>>),
        <<"model">> => maps:get(model, Acc, <<>>),
        <<"type">> => <<"message">>,
        <<"role">> => <<"assistant">>,
        <<"content">> => build_anthropic_raw_blocks(maps:get(blocks, Acc, #{})),
        <<"stop_reason">> => maps:get(finish_reason, Acc, null),
        <<"stop_sequence">> => maps:get(stop_sequence, Acc, null),
        <<"usage">> => maps:get(usage_raw, Acc, #{})
    },
    beamai_llm_response_parser:from_anthropic(Raw).

%% @private 初始化内容块（保留原始 binary key 结构）
%% tool_use 块的 input 由后续 input_json_delta 分片拼接，
%% 用 partial_json 缓冲区暂存。
init_anthropic_block(#{<<"type">> := <<"tool_use">>} = Block) ->
    Block#{<<"partial_json">> => <<>>};
init_anthropic_block(#{<<"type">> := <<"thinking">>} = Block) ->
    %% 确保 thinking / signature 字段存在，便于后续累加与解析
    Block#{
        <<"thinking">> => maps:get(<<"thinking">>, Block, <<>>),
        <<"signature">> => maps:get(<<"signature">>, Block, <<>>)
    };
init_anthropic_block(Block) ->
    Block.

%% @private 按 delta 类型更新内容块
apply_anthropic_delta(#{<<"type">> := <<"text_delta">>, <<"text">> := T}, Block) ->
    Block#{<<"text">> => <<(maps:get(<<"text">>, Block, <<>>))/binary, T/binary>>};
apply_anthropic_delta(#{<<"type">> := <<"input_json_delta">>, <<"partial_json">> := J}, Block) ->
    Block#{<<"partial_json">> => <<(maps:get(<<"partial_json">>, Block, <<>>))/binary, J/binary>>};
apply_anthropic_delta(#{<<"type">> := <<"thinking_delta">>, <<"thinking">> := T}, Block) ->
    Block#{<<"thinking">> => <<(maps:get(<<"thinking">>, Block, <<>>))/binary, T/binary>>};
apply_anthropic_delta(#{<<"type">> := <<"signature_delta">>, <<"signature">> := Sig}, Block) ->
    Block#{<<"signature">> => Sig};
apply_anthropic_delta(_, Block) ->
    Block.

%% @private 将 index 分组的内容块转为 Anthropic 原始格式（按 index 排序）
build_anthropic_raw_blocks(BlockMap) ->
    Indexes = lists:sort(maps:keys(BlockMap)),
    [finalize_anthropic_block(maps:get(I, BlockMap)) || I <- Indexes].

%% @private 完成单个内容块（tool_use 解析累加的 partial_json 为 input）
finalize_anthropic_block(#{<<"type">> := <<"tool_use">>} = Block) ->
    {PartialJson, Block1} = case maps:take(<<"partial_json">>, Block) of
        {J, B} -> {J, B};
        error -> {<<>>, Block}
    end,
    Input = case PartialJson of
        <<>> -> maps:get(<<"input">>, Block1, #{});
        _ -> decode_json_or_empty(PartialJson)
    end,
    Block1#{<<"input">> => Input};
finalize_anthropic_block(Block) ->
    Block.

%% @private 解析 JSON，失败时返回空 map
decode_json_or_empty(Bin) ->
    try jsx:decode(Bin, [return_maps])
    catch _:_ -> #{}
    end.

%%====================================================================
%% 响应解析
%%====================================================================

%% @doc 解析 OpenAI 格式的工具调用列表
%%
%% 从消息 map 中提取 tool_calls 字段并解析为统一格式。
%%
%% @param Message 消息 map，可能包含 tool_calls 字段
%% @returns 工具调用列表 [#{id, name, arguments}]
-spec parse_tool_calls(map()) -> [map()].
parse_tool_calls(#{<<"tool_calls">> := Calls}) when is_list(Calls) ->
    [parse_single_tool_call(C) || C <- Calls];
parse_tool_calls(_) ->
    [].

%% @doc 解析单个 OpenAI 格式的工具调用
%%
%% @param Call 工具调用 map
%% @returns 标准化的工具调用 map #{id, name, arguments}
-spec parse_single_tool_call(map()) -> map().
parse_single_tool_call(#{<<"id">> := Id, <<"function">> := Func}) ->
    #{
        id => Id,
        name => maps:get(<<"name">>, Func, <<>>),
        arguments => maps:get(<<"arguments">>, Func, <<>>)
    };
parse_single_tool_call(#{<<"function">> := Func}) ->
    %% 某些 Provider 可能不返回 id
    #{
        id => generate_tool_call_id(),
        name => maps:get(<<"name">>, Func, <<>>),
        arguments => maps:get(<<"arguments">>, Func, <<>>)
    };
parse_single_tool_call(_) ->
    #{id => <<>>, name => <<>>, arguments => <<>>}.

%% @doc 解析 OpenAI 格式的使用统计
%%
%% @param Usage 使用统计 map
%% @returns 标准化的使用统计 map #{prompt_tokens, completion_tokens, total_tokens}
-spec parse_usage(map()) -> map().
parse_usage(Usage) ->
    #{
        prompt_tokens => maps:get(<<"prompt_tokens">>, Usage, 0),
        completion_tokens => maps:get(<<"completion_tokens">>, Usage, 0),
        total_tokens => maps:get(<<"total_tokens">>, Usage, 0)
    }.

%%====================================================================
%% 响应头：速率限制
%%====================================================================

%% @doc 从响应头提取速率限制信息，封装为可合并进 metadata 的 map
%%
%% 识别以下响应头（大小写不敏感）：
%%   - anthropic-ratelimit-*（如 -requests-limit / -tokens-remaining / -*-reset）
%%   - x-ratelimit-*（OpenAI / DeepSeek 等 OpenAI 兼容服务）
%%   - retry-after
%%
%% 命中时返回 #{rate_limit => #{<<"requests-limit">> => V, ...}}，
%% 无命中返回 #{}（上层据此决定是否注入）。可直接作为
%% beamai_llm_http_client 的 on_headers 处理器使用。
%%
%% @param Headers 响应头列表 [{Name, Value}]
%% @returns #{rate_limit => map()} | #{}
-spec rate_limit_metadata([{binary(), binary()}]) -> map().
rate_limit_metadata(Headers) when is_list(Headers) ->
    RL = lists:foldl(fun({K, V}, Acc) ->
        case rate_limit_key(string:lowercase(to_header_binary(K))) of
            skip -> Acc;
            NormKey -> Acc#{NormKey => V}
        end
    end, #{}, Headers),
    case map_size(RL) of
        0 -> #{};
        _ -> #{rate_limit => RL}
    end;
rate_limit_metadata(_) ->
    #{}.

%% @doc 从响应头解析 Retry-After，返回毫秒数（无或不可解析时返回 undefined）
%%
%% Retry-After 通常为整数秒（也可能是 HTTP-date，此处仅支持整数秒，
%% 其它格式回退 undefined 由上层走默认退避）。用于 429/503 时按服务端
%% 建议的等待时间退避。
-spec retry_after_ms([{binary(), binary()}]) -> non_neg_integer() | undefined.
retry_after_ms(Headers) when is_list(Headers) ->
    case find_header(<<"retry-after">>, Headers) of
        undefined -> undefined;
        Value ->
            case string:to_integer(string:trim(to_header_binary(Value))) of
                {Int, <<>>} when Int >= 0 -> Int * 1000;
                _ -> undefined
            end
    end;
retry_after_ms(_) ->
    undefined.

%% @private 大小写不敏感查找响应头值
find_header(Name, Headers) ->
    LName = string:lowercase(Name),
    case lists:search(fun({K, _}) -> string:lowercase(to_header_binary(K)) =:= LName end, Headers) of
        {value, {_, V}} -> V;
        false -> undefined
    end.

%% @private 归一化速率限制响应头名（去掉厂商前缀，保留语义后缀）
rate_limit_key(<<"anthropic-ratelimit-", Rest/binary>>) -> Rest;
rate_limit_key(<<"x-ratelimit-", Rest/binary>>) -> Rest;
rate_limit_key(<<"retry-after">>) -> <<"retry-after">>;
rate_limit_key(_) -> skip.

%% @private 响应头名统一为 binary
to_header_binary(K) when is_binary(K) -> K;
to_header_binary(K) when is_list(K) -> list_to_binary(K);
to_header_binary(K) when is_atom(K) -> atom_to_binary(K, utf8).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 生成工具调用 ID
-spec generate_tool_call_id() -> binary().
generate_tool_call_id() ->
    Rand = integer_to_binary(rand:uniform(1000000000)),
    <<"call_", Rand/binary>>.
