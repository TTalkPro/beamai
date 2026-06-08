%%%-------------------------------------------------------------------
%%% @doc LLM 响应解析器模块
%%%
%%% 将不同 Provider 的原始 API 响应解析为统一的 beamai_llm_response 结构。
%%% 每个 Provider 有独立的解析逻辑，通过 parser_*/0 返回解析器函数，
%%% 可直接传给 beamai_llm_http_client:request/5。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_response_parser).

%% 构造函数
-export([from_provider/2, from_openai/1, from_anthropic/1]).
-export([from_ollama/1, from_dashscope/1, from_zhipu/1, from_deepseek/1, from_deepseek_fim/1]).

%% HTTP Client 解析器（可直接传给 beamai_llm_http_client:request/5）
-export([parser_openai/0, parser_anthropic/0, parser/1]).
-export([parser_ollama/0, parser_dashscope/0, parser_zhipu/0, parser_deepseek/0, parser_deepseek_fim/0]).

%%====================================================================
%% HTTP Client 解析器
%%====================================================================

%% @doc 返回 OpenAI 格式的解析器函数
%% 可直接用于 beamai_llm_http_client:request/5
-spec parser_openai() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
parser_openai() ->
    fun from_openai/1.

%% @doc 返回 Anthropic 格式的解析器函数
-spec parser_anthropic() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
parser_anthropic() ->
    fun from_anthropic/1.

%% @doc 返回指定 Provider 的解析器函数
-spec parser(beamai_llm_response:provider()) -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
parser(openai) -> parser_openai();
parser(anthropic) -> parser_anthropic();
parser(deepseek) -> parser_deepseek();
parser(zhipu) -> parser_zhipu();
parser(ollama) -> parser_ollama();
parser(bailian) -> parser_dashscope();
parser(_) -> parser_openai().

%% @doc 返回 Ollama 格式的解析器函数
-spec parser_ollama() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
parser_ollama() ->
    fun from_ollama/1.

%% @doc 返回 DashScope 格式的解析器函数
-spec parser_dashscope() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
parser_dashscope() ->
    fun from_dashscope/1.

%% @doc 返回智谱格式的解析器函数（OpenAI兼容 + reasoning_content）
-spec parser_zhipu() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
parser_zhipu() ->
    fun from_zhipu/1.

%% @doc 返回 DeepSeek 格式的解析器函数（OpenAI兼容 + reasoning_content）
-spec parser_deepseek() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
parser_deepseek() ->
    fun from_deepseek/1.

%% @doc 返回 DeepSeek FIM（填空补全）格式的解析器函数
-spec parser_deepseek_fim() -> fun((map()) -> {ok, beamai_llm_response:response()} | {error, term()}).
parser_deepseek_fim() ->
    fun from_deepseek_fim/1.

%%====================================================================
%% 构造函数
%%====================================================================

%% @doc 从指定 Provider 的原始响应创建统一响应
-spec from_provider(map(), beamai_llm_response:provider()) -> {ok, beamai_llm_response:response()} | {error, term()}.
from_provider(Raw, openai) -> from_openai(Raw);
from_provider(Raw, anthropic) -> from_anthropic(Raw);
from_provider(Raw, deepseek) -> from_deepseek(Raw); % DeepSeek 使用 OpenAI 格式 + reasoning_content
from_provider(Raw, zhipu) -> from_zhipu(Raw);      % 智谱使用 OpenAI 格式 + reasoning_content
from_provider(Raw, ollama) -> from_ollama(Raw);    % Ollama 支持原生和 OpenAI 格式
from_provider(Raw, bailian) -> from_dashscope(Raw); % 百炼使用 DashScope 格式
from_provider(Raw, Provider) ->
    %% 默认尝试 OpenAI 格式
    case from_openai(Raw) of
        {ok, Resp} -> {ok, Resp#{provider => Provider}};
        Error -> Error
    end.

%% @doc 从 OpenAI 格式响应创建
-spec from_openai(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
from_openai(#{<<"choices">> := [Choice | _]} = Raw) ->
    Message = maps:get(<<"message">>, Choice, #{}),
    ToolCalls = parse_tool_calls_openai(Message),
    {ok, beamai_llm_response:new(#{
        id => maps:get(<<"id">>, Raw, <<>>),
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => openai,
        content => maps:get(<<"content">>, Message, null),
        content_blocks => build_content_blocks_openai(Message),
        tool_calls => ToolCalls,
        finish_reason => normalize_finish_reason_openai(maps:get(<<"finish_reason">>, Choice, <<>>)),
        usage => parse_usage_openai(maps:get(<<"usage">>, Raw, #{}), Raw),
        raw => Raw,
        metadata => #{
            created => maps:get(<<"created">>, Raw, undefined),
            system_fingerprint => maps:get(<<"system_fingerprint">>, Raw, undefined),
            object => maps:get(<<"object">>, Raw, undefined)
        }
    })};
from_openai(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
from_openai(Raw) ->
    {error, {invalid_response, Raw}}.

%% @doc 从 Anthropic 格式响应创建
-spec from_anthropic(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
from_anthropic(#{<<"content">> := ContentBlocks} = Raw) when is_list(ContentBlocks) ->
    {Content, ToolCalls, Blocks} = extract_anthropic_content(ContentBlocks),
    Metadata0 = #{
        type => maps:get(<<"type">>, Raw, undefined),
        role => maps:get(<<"role">>, Raw, undefined),
        stop_sequence => maps:get(<<"stop_sequence">>, Raw, undefined)
    },
    Metadata = maybe_add_anthropic_citations(Metadata0, ContentBlocks),
    {ok, beamai_llm_response:new(#{
        id => maps:get(<<"id">>, Raw, <<>>),
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => anthropic,
        content => Content,
        content_blocks => Blocks,
        tool_calls => ToolCalls,
        finish_reason => normalize_finish_reason_anthropic(maps:get(<<"stop_reason">>, Raw, <<>>)),
        usage => parse_usage_anthropic(maps:get(<<"usage">>, Raw, #{}), Raw),
        raw => Raw,
        metadata => Metadata
    })};
from_anthropic(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
from_anthropic(Raw) ->
    {error, {invalid_response, Raw}}.

%% @doc 从智谱 AI 格式响应创建
%% 智谱使用 OpenAI 兼容格式，但有 reasoning_content 字段
-spec from_zhipu(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
from_zhipu(#{<<"choices">> := [Choice | _]} = Raw) ->
    Message = maps:get(<<"message">>, Choice, #{}),
    ToolCalls = parse_tool_calls_openai(Message),
    ReasoningContent = maps:get(<<"reasoning_content">>, Message, null),
    %% 内容处理：如果 content 为空但有 reasoning_content，使用 reasoning_content
    Content = case maps:get(<<"content">>, Message, null) of
        null -> ReasoningContent;
        <<>> -> case ReasoningContent of null -> null; _ -> ReasoningContent end;
        C -> C
    end,
    {ok, beamai_llm_response:new(#{
        id => maps:get(<<"id">>, Raw, <<>>),
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => zhipu,
        content => Content,
        content_blocks => build_content_blocks_openai(Message),
        tool_calls => ToolCalls,
        finish_reason => normalize_finish_reason_openai(maps:get(<<"finish_reason">>, Choice, <<>>)),
        usage => parse_usage_openai(maps:get(<<"usage">>, Raw, #{}), Raw),
        raw => Raw,
        metadata => #{
            created => maps:get(<<"created">>, Raw, undefined),
            reasoning_content => ReasoningContent
        }
    })};
from_zhipu(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
from_zhipu(Raw) ->
    {error, {invalid_response, Raw}}.

%% @doc 从 DeepSeek 格式响应创建
%% DeepSeek 使用 OpenAI 兼容格式，deepseek-reasoner 模型额外返回
%% reasoning_content 字段（思维链内容，与最终回答 content 并存）。
-spec from_deepseek(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
from_deepseek(#{<<"choices">> := [Choice | _]} = Raw) ->
    Message = maps:get(<<"message">>, Choice, #{}),
    ToolCalls = parse_tool_calls_openai(Message),
    ReasoningContent = maps:get(<<"reasoning_content">>, Message, null),
    {ok, beamai_llm_response:new(#{
        id => maps:get(<<"id">>, Raw, <<>>),
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => deepseek,
        content => maps:get(<<"content">>, Message, null),
        content_blocks => build_content_blocks_openai(Message),
        tool_calls => ToolCalls,
        finish_reason => normalize_finish_reason_openai(maps:get(<<"finish_reason">>, Choice, <<>>)),
        usage => parse_usage_deepseek(maps:get(<<"usage">>, Raw, #{}), Raw),
        raw => Raw,
        metadata => #{
            created => maps:get(<<"created">>, Raw, undefined),
            system_fingerprint => maps:get(<<"system_fingerprint">>, Raw, undefined),
            reasoning_content => ReasoningContent
        }
    })};
from_deepseek(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
from_deepseek(Raw) ->
    {error, {invalid_response, Raw}}.

%% @doc 从 DeepSeek FIM（填空补全）格式响应创建
%% FIM 接口（/beta/completions）使用文本补全格式：
%% 补全文本在 choices[0].text，没有 message 包装。
-spec from_deepseek_fim(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
from_deepseek_fim(#{<<"choices">> := [Choice | _]} = Raw) ->
    Text = case maps:get(<<"text">>, Choice, <<>>) of
        T when is_binary(T) -> T;
        _ -> <<>>
    end,
    {ok, beamai_llm_response:new(#{
        id => maps:get(<<"id">>, Raw, <<>>),
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => deepseek,
        content => normalize_content(Text),
        content_blocks => case Text of <<>> -> []; _ -> [#{type => text, text => Text}] end,
        tool_calls => [],
        finish_reason => normalize_finish_reason_openai(maps:get(<<"finish_reason">>, Choice, <<>>)),
        usage => parse_usage_deepseek(maps:get(<<"usage">>, Raw, #{}), Raw),
        raw => Raw,
        metadata => #{
            created => maps:get(<<"created">>, Raw, undefined),
            object => maps:get(<<"object">>, Raw, undefined),
            logprobs => maps:get(<<"logprobs">>, Choice, undefined)
        }
    })};
from_deepseek_fim(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
from_deepseek_fim(Raw) ->
    {error, {invalid_response, Raw}}.

%% @doc 从 Ollama 格式响应创建
%% 支持 Ollama 原生格式和 OpenAI 兼容格式
-spec from_ollama(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
from_ollama(#{<<"message">> := Message} = Raw) ->
    %% Ollama 原生格式
    ToolCalls = parse_tool_calls_openai(Message),
    Content = maps:get(<<"content">>, Message, <<>>),
    {ok, beamai_llm_response:new(#{
        id => maps:get(<<"created_at">>, Raw, <<>>),
        model => maps:get(<<"model">>, Raw, <<>>),
        provider => ollama,
        content => normalize_content(Content),
        content_blocks => case Content of <<>> -> []; _ -> [#{type => text, text => Content}] end,
        tool_calls => ToolCalls,
        finish_reason => normalize_finish_reason_ollama(maps:get(<<"done_reason">>, Raw, <<"stop">>)),
        usage => parse_usage_ollama(Raw),
        raw => Raw,
        metadata => #{
            done => maps:get(<<"done">>, Raw, false),
            total_duration => maps:get(<<"total_duration">>, Raw, undefined),
            load_duration => maps:get(<<"load_duration">>, Raw, undefined),
            prompt_eval_duration => maps:get(<<"prompt_eval_duration">>, Raw, undefined),
            eval_duration => maps:get(<<"eval_duration">>, Raw, undefined)
        }
    })};
from_ollama(#{<<"choices">> := _} = Raw) ->
    %% OpenAI 兼容格式，直接委托
    case from_openai(Raw) of
        {ok, Resp} -> {ok, Resp#{provider => ollama}};
        Error -> Error
    end;
from_ollama(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
from_ollama(Raw) ->
    {error, {invalid_response, Raw}}.

%% @doc 从阿里云 DashScope 格式响应创建
%% DashScope 使用 {output: {choices: [...]}, usage: {...}} 格式
-spec from_dashscope(map()) -> {ok, beamai_llm_response:response()} | {error, term()}.
from_dashscope(#{<<"output">> := Output} = Raw) ->
    parse_dashscope_output(Output, Raw);
from_dashscope(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
from_dashscope(#{<<"code">> := Code, <<"message">> := Message}) ->
    {error, {api_error, #{code => Code, message => Message}}};
from_dashscope(Raw) ->
    {error, {invalid_response, Raw}}.

%%====================================================================
%% OpenAI 内部函数
%%====================================================================

%% @private 解析 OpenAI 格式的工具调用
parse_tool_calls_openai(#{<<"tool_calls">> := Calls}) when is_list(Calls) ->
    [parse_single_tool_call_openai(C) || C <- Calls];
parse_tool_calls_openai(_) ->
    [].

parse_single_tool_call_openai(#{<<"id">> := Id, <<"function">> := Func}) ->
    RawArgs = maps:get(<<"arguments">>, Func, <<"{}">>),
    #{
        id => Id,
        name => maps:get(<<"name">>, Func, <<>>),
        arguments => safe_decode_json(RawArgs),
        raw_arguments => RawArgs
    };
parse_single_tool_call_openai(_) ->
    #{id => <<>>, name => <<>>, arguments => #{}, raw_arguments => <<"{}">>}.

%% @private 构建 OpenAI 内容块
build_content_blocks_openai(#{<<"content">> := Content, <<"tool_calls">> := ToolCalls})
  when Content =/= null, is_list(ToolCalls), length(ToolCalls) > 0 ->
    TextBlock = #{type => text, text => Content},
    ToolBlocks = [#{
        type => tool_use,
        id => maps:get(<<"id">>, TC, <<>>),
        name => maps:get(<<"name">>, maps:get(<<"function">>, TC, #{}), <<>>),
        input => safe_decode_json(maps:get(<<"arguments">>, maps:get(<<"function">>, TC, #{}), <<"{}">>))
    } || TC <- ToolCalls],
    [TextBlock | ToolBlocks];
build_content_blocks_openai(#{<<"content">> := Content}) when Content =/= null ->
    [#{type => text, text => Content}];
build_content_blocks_openai(#{<<"tool_calls">> := ToolCalls}) when is_list(ToolCalls), length(ToolCalls) > 0 ->
    [#{
        type => tool_use,
        id => maps:get(<<"id">>, TC, <<>>),
        name => maps:get(<<"name">>, maps:get(<<"function">>, TC, #{}), <<>>),
        input => safe_decode_json(maps:get(<<"arguments">>, maps:get(<<"function">>, TC, #{}), <<"{}">>))
    } || TC <- ToolCalls];
build_content_blocks_openai(_) ->
    [].

%% @private 解析 OpenAI usage
parse_usage_openai(Usage, Raw) ->
    InputTokens = maps:get(<<"prompt_tokens">>, Usage, 0),
    OutputTokens = maps:get(<<"completion_tokens">>, Usage, 0),
    Base = #{
        input_tokens => InputTokens,
        output_tokens => OutputTokens,
        total_tokens => maps:get(<<"total_tokens">>, Usage, InputTokens + OutputTokens)
    },
    %% 提取 OpenAI 特有的详细统计
    Details = extract_openai_usage_details(Usage, Raw),
    case maps:size(Details) of
        0 -> Base;
        _ -> Base#{details => Details}
    end.

%% @private 提取 OpenAI 特有的 usage 详情
extract_openai_usage_details(Usage, _Raw) ->
    Details = #{},
    %% completion_tokens_details (如 reasoning_tokens for o1)
    Details1 = case maps:get(<<"completion_tokens_details">>, Usage, undefined) of
        undefined -> Details;
        CompDetails -> Details#{completion_details => CompDetails}
    end,
    %% prompt_tokens_details (如 cached_tokens)
    case maps:get(<<"prompt_tokens_details">>, Usage, undefined) of
        undefined -> Details1;
        PromptDetails -> Details1#{prompt_details => PromptDetails}
    end.

%% @private 解析 DeepSeek usage（OpenAI 格式 + 硬盘缓存统计）
%% DeepSeek 上下文缓存自动启用，usage 顶层额外返回
%% prompt_cache_hit_tokens / prompt_cache_miss_tokens，提取到 details。
parse_usage_deepseek(Usage, Raw) ->
    Base = parse_usage_openai(Usage, Raw),
    CacheFields = [
        {<<"prompt_cache_hit_tokens">>, prompt_cache_hit_tokens},
        {<<"prompt_cache_miss_tokens">>, prompt_cache_miss_tokens}
    ],
    Extra = lists:foldl(fun({JsonKey, AtomKey}, Acc) ->
        case maps:get(JsonKey, Usage, undefined) of
            undefined -> Acc;
            Value -> Acc#{AtomKey => Value}
        end
    end, #{}, CacheFields),
    case maps:size(Extra) of
        0 -> Base;
        _ -> Base#{details => maps:merge(maps:get(details, Base, #{}), Extra)}
    end.

%% @private 标准化 OpenAI finish_reason
normalize_finish_reason_openai(<<"stop">>) -> complete;
normalize_finish_reason_openai(<<"tool_calls">>) -> tool_use;
normalize_finish_reason_openai(<<"length">>) -> length_limit;
normalize_finish_reason_openai(<<"content_filter">>) -> content_filtered;
normalize_finish_reason_openai(<<>>) -> unknown;
normalize_finish_reason_openai(null) -> unknown;
normalize_finish_reason_openai(_) -> unknown.

%%====================================================================
%% Anthropic 内部函数
%%====================================================================

%% @private 提取 Anthropic 内容和工具调用
extract_anthropic_content(Blocks) ->
    extract_anthropic_content(Blocks, <<>>, [], []).

extract_anthropic_content([], Content, ToolCalls, ContentBlocks) ->
    {normalize_content(Content), lists:reverse(ToolCalls), lists:reverse(ContentBlocks)};
extract_anthropic_content([#{<<"type">> := <<"text">>, <<"text">> := T} | Rest], Content, ToolCalls, Blocks) ->
    Block = #{type => text, text => T},
    NewContent = <<Content/binary, T/binary>>,
    extract_anthropic_content(Rest, NewContent, ToolCalls, [Block | Blocks]);
extract_anthropic_content([#{<<"type">> := <<"thinking">>, <<"thinking">> := T, <<"signature">> := Sig} | Rest], Content, ToolCalls, Blocks) ->
    Block = #{type => thinking, thinking => T, signature => Sig},
    extract_anthropic_content(Rest, Content, ToolCalls, [Block | Blocks]);
extract_anthropic_content([#{<<"type">> := <<"redacted_thinking">>, <<"data">> := Data} | Rest], Content, ToolCalls, Blocks) ->
    Block = #{type => redacted_thinking, data => Data},
    extract_anthropic_content(Rest, Content, ToolCalls, [Block | Blocks]);
extract_anthropic_content([#{<<"type">> := <<"tool_use">>} = B | Rest], Content, ToolCalls, Blocks) ->
    Id = maps:get(<<"id">>, B, <<>>),
    Name = maps:get(<<"name">>, B, <<>>),
    Input = maps:get(<<"input">>, B, #{}),
    ToolCall = #{
        id => Id,
        name => Name,
        arguments => Input,
        raw_arguments => jsx:encode(Input)
    },
    Block = #{type => tool_use, id => Id, name => Name, input => Input},
    extract_anthropic_content(Rest, Content, [ToolCall | ToolCalls], [Block | Blocks]);
extract_anthropic_content([_ | Rest], Content, ToolCalls, Blocks) ->
    extract_anthropic_content(Rest, Content, ToolCalls, Blocks).

%% @private 从 text block 提取 citations 注入 metadata
%% Claude 启用引用后，text block 会携带 citations 数组（char/page/content_block
%% location 等），汇总到 metadata.citations 便于上层使用。
maybe_add_anthropic_citations(Metadata, ContentBlocks) ->
    Citations = lists:flatten(
        [maps:get(<<"citations">>, B, [])
         || B <- ContentBlocks,
            maps:get(<<"type">>, B, <<>>) =:= <<"text">>,
            is_list(maps:get(<<"citations">>, B, undefined))]),
    case Citations of
        [] -> Metadata;
        _ -> Metadata#{citations => Citations}
    end.

%% @private 解析 Anthropic usage
parse_usage_anthropic(Usage, _Raw) ->
    InputTokens = maps:get(<<"input_tokens">>, Usage, 0),
    OutputTokens = maps:get(<<"output_tokens">>, Usage, 0),
    Base = #{
        input_tokens => InputTokens,
        output_tokens => OutputTokens,
        total_tokens => InputTokens + OutputTokens
    },
    %% 提取 Anthropic 特有的详细统计
    Details = extract_anthropic_usage_details(Usage),
    case maps:size(Details) of
        0 -> Base;
        _ -> Base#{details => Details}
    end.

%% @private 提取 Anthropic 特有的 usage 详情
extract_anthropic_usage_details(Usage) ->
    Fields = [
        {<<"cache_creation_input_tokens">>, cache_creation_input_tokens},
        {<<"cache_read_input_tokens">>, cache_read_input_tokens},
        {<<"cache_creation">>, cache_creation},
        {<<"server_tool_use">>, server_tool_use},
        {<<"service_tier">>, service_tier},
        {<<"inference_geo">>, inference_geo}
    ],
    lists:foldl(fun({JsonKey, AtomKey}, Acc) ->
        case maps:get(JsonKey, Usage, undefined) of
            undefined -> Acc;
            Value -> Acc#{AtomKey => Value}
        end
    end, #{}, Fields).

%% @private 标准化 Anthropic stop_reason
normalize_finish_reason_anthropic(<<"end_turn">>) -> complete;
normalize_finish_reason_anthropic(<<"tool_use">>) -> tool_use;
normalize_finish_reason_anthropic(<<"max_tokens">>) -> length_limit;
normalize_finish_reason_anthropic(<<"stop_sequence">>) -> stop_sequence;
normalize_finish_reason_anthropic(<<"pause_turn">>) -> pause_turn;
normalize_finish_reason_anthropic(<<"refusal">>) -> refusal;
normalize_finish_reason_anthropic(<<>>) -> unknown;
normalize_finish_reason_anthropic(null) -> unknown;
normalize_finish_reason_anthropic(_) -> unknown.

%%====================================================================
%% Ollama 内部函数
%%====================================================================

%% @private 解析 Ollama 原生格式的 usage
parse_usage_ollama(Resp) ->
    PromptTokens = maps:get(<<"prompt_eval_count">>, Resp, 0),
    CompletionTokens = maps:get(<<"eval_count">>, Resp, 0),
    #{
        input_tokens => PromptTokens,
        output_tokens => CompletionTokens,
        total_tokens => PromptTokens + CompletionTokens
    }.

%% @private 标准化 Ollama done_reason
normalize_finish_reason_ollama(<<"stop">>) -> complete;
normalize_finish_reason_ollama(<<"length">>) -> length_limit;
normalize_finish_reason_ollama(<<>>) -> complete;  % Ollama 默认为完成
normalize_finish_reason_ollama(null) -> complete;
normalize_finish_reason_ollama(_) -> unknown.

%%====================================================================
%% DashScope 内部函数
%%====================================================================

%% @private 解析 DashScope output
parse_dashscope_output(#{<<"choices">> := [Choice | _]}, Raw) ->
    Message = maps:get(<<"message">>, Choice, #{}),
    ToolCalls = parse_tool_calls_openai(Message),
    Content = maps:get(<<"content">>, Message, null),
    {ok, beamai_llm_response:new(#{
        id => maps:get(<<"request_id">>, Raw, <<>>),
        model => <<>>,  %% DashScope 响应不包含 model
        provider => bailian,
        content => Content,
        content_blocks => case Content of null -> []; _ -> [#{type => text, text => Content}] end,
        tool_calls => ToolCalls,
        finish_reason => normalize_finish_reason_dashscope(maps:get(<<"finish_reason">>, Choice, <<>>)),
        usage => parse_usage_dashscope(maps:get(<<"usage">>, Raw, #{})),
        raw => Raw,
        metadata => #{
            request_id => maps:get(<<"request_id">>, Raw, undefined)
        }
    })};
%% 兼容旧格式：text + finish_reason 直接在 output 下
parse_dashscope_output(#{<<"text">> := Text, <<"finish_reason">> := FinishReason}, Raw) ->
    {ok, beamai_llm_response:new(#{
        id => maps:get(<<"request_id">>, Raw, <<>>),
        model => <<>>,
        provider => bailian,
        content => Text,
        content_blocks => [#{type => text, text => Text}],
        tool_calls => [],
        finish_reason => normalize_finish_reason_dashscope(FinishReason),
        usage => parse_usage_dashscope(maps:get(<<"usage">>, Raw, #{})),
        raw => Raw,
        metadata => #{
            request_id => maps:get(<<"request_id">>, Raw, undefined)
        }
    })};
parse_dashscope_output(Output, _Raw) ->
    {error, {invalid_output, Output}}.

%% @private 解析 DashScope usage（使用 input_tokens/output_tokens）
parse_usage_dashscope(Usage) ->
    InputTokens = maps:get(<<"input_tokens">>, Usage, 0),
    OutputTokens = maps:get(<<"output_tokens">>, Usage, 0),
    #{
        input_tokens => InputTokens,
        output_tokens => OutputTokens,
        total_tokens => maps:get(<<"total_tokens">>, Usage, InputTokens + OutputTokens)
    }.

%% @private 标准化 DashScope finish_reason
normalize_finish_reason_dashscope(<<"stop">>) -> complete;
normalize_finish_reason_dashscope(<<"tool_calls">>) -> tool_use;
normalize_finish_reason_dashscope(<<"length">>) -> length_limit;
normalize_finish_reason_dashscope(<<>>) -> unknown;
normalize_finish_reason_dashscope(null) -> complete;
normalize_finish_reason_dashscope(_) -> unknown.

%%====================================================================
%% 通用内部函数
%%====================================================================

%% @private 安全解码 JSON
safe_decode_json(Bin) when is_binary(Bin) ->
    try jsx:decode(Bin, [return_maps])
    catch _:_ -> #{}
    end;
safe_decode_json(_) -> #{}.

%% @private 标准化内容（空字符串变为 null）
normalize_content(<<>>) -> null;
normalize_content(Content) -> Content.
