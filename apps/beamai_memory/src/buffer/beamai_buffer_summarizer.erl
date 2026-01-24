%%%-------------------------------------------------------------------
%%% @doc Agent Buffer Summarizer - 对话摘要生成器
%%%
%%% 提供基于 LLM 的对话摘要生成功能。
%%% 可与 beamai_conversation_buffer 配合使用。
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建 LLM 摘要器
%%% SummarizeFn = beamai_buffer_summarizer:create_llm_summarizer(#{
%%%     provider => openai,
%%%     model => <<"gpt-3.5-turbo">>,
%%%     max_tokens => 500
%%% }),
%%%
%%% %% 配置到 Buffer
%%% BufferConfig = beamai_conversation_buffer:new(#{
%%%     window_size => 20,
%%%     summarize => true,
%%%     summarize_fn => SummarizeFn
%%% }),
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_buffer_summarizer).

-type message() :: beamai_conversation_buffer:message().
-type summarize_fn() :: fun(([message()]) -> {ok, binary()} | {error, term()}).

%% API 导出
-export([
    create_llm_summarizer/1,
    create_llm_summarizer/2,
    create_simple_summarizer/0,
    create_simple_summarizer/1,
    summarize_with_llm/3,
    default_prompt/0
]).

%%====================================================================
%% 默认值
%%====================================================================

-define(DEFAULT_MAX_TOKENS, 500).
-define(DEFAULT_TEMPERATURE, 0.3).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 创建 LLM 摘要函数
%%
%% Opts 支持：
%% - provider: LLM 提供商 (openai | anthropic | zhipu)
%% - model: 模型名称
%% - max_tokens: 摘要最大 Token 数
%% - temperature: 温度参数
%% - prompt: 自定义 Prompt 模板
-spec create_llm_summarizer(map()) -> summarize_fn().
create_llm_summarizer(Opts) ->
    create_llm_summarizer(Opts, default_prompt()).

%% @doc 创建 LLM 摘要函数（自定义 Prompt）
-spec create_llm_summarizer(map(), binary()) -> summarize_fn().
create_llm_summarizer(Opts, PromptTemplate) ->
    fun(Messages) ->
        summarize_with_llm(Opts, PromptTemplate, Messages)
    end.

%% @doc 创建简单摘要函数（不依赖 LLM）
-spec create_simple_summarizer() -> summarize_fn().
create_simple_summarizer() ->
    create_simple_summarizer(#{}).

%% @doc 创建简单摘要函数（带配置）
%%
%% Opts 支持：
%% - max_messages: 摘要中包含的最大消息数（默认 5）
%% - max_content_length: 每条消息内容的最大长度（默认 100）
-spec create_simple_summarizer(map()) -> summarize_fn().
create_simple_summarizer(Opts) ->
    MaxMessages = maps:get(max_messages, Opts, 5),
    MaxContentLength = maps:get(max_content_length, Opts, 100),

    fun(Messages) ->
        simple_summarize(Messages, MaxMessages, MaxContentLength)
    end.

%% @doc 使用 LLM 生成摘要
-spec summarize_with_llm(map(), binary(), [message()]) ->
    {ok, binary()} | {error, term()}.
summarize_with_llm(_Opts, _PromptTemplate, []) ->
    {ok, <<>>};
summarize_with_llm(Opts, PromptTemplate, Messages) ->
    %% 构建对话文本
    ConversationText = format_messages_for_summary(Messages),

    %% 构建完整 Prompt
    Prompt = build_summary_prompt(PromptTemplate, ConversationText, length(Messages)),

    %% 调用 LLM
    Provider = maps:get(provider, Opts, openai),
    Model = maps:get(model, Opts, <<"gpt-3.5-turbo">>),
    MaxTokens = maps:get(max_tokens, Opts, ?DEFAULT_MAX_TOKENS),
    Temperature = maps:get(temperature, Opts, ?DEFAULT_TEMPERATURE),

    LLMOpts = #{
        model => Model,
        max_tokens => MaxTokens,
        temperature => Temperature
    },

    LLMMessages = [
        #{role => <<"system">>, content => <<"你是一个对话摘要助手，请简洁准确地总结对话内容。">>},
        #{role => <<"user">>, content => Prompt}
    ],

    case call_llm(Provider, LLMMessages, LLMOpts) of
        {ok, Response} ->
            extract_summary_from_response(Response);
        {error, _} = Error ->
            Error
    end.

%% @doc 获取默认 Prompt 模板
-spec default_prompt() -> binary().
default_prompt() ->
    <<"请总结以下对话的主要内容，包括：
1. 讨论的主题
2. 关键决定或结论
3. 重要的问题和答案
4. 未解决的事项（如有）

对话内容（共 ~B 条消息）：
~s

请用简洁的语言总结，控制在 200 字以内。">>.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 简单摘要实现
-spec simple_summarize([message()], pos_integer(), pos_integer()) ->
    {ok, binary()}.
simple_summarize([], _, _) ->
    {ok, <<>>};
simple_summarize(Messages, MaxMessages, MaxContentLength) ->
    MsgCount = length(Messages),

    %% 提取关键消息
    SampleMsgs = case MsgCount =< MaxMessages of
        true -> Messages;
        false ->
            %% 取首尾各一半
            Half = MaxMessages div 2,
            FirstPart = lists:sublist(Messages, Half),
            LastPart = lists:nthtail(MsgCount - Half, Messages),
            FirstPart ++ LastPart
    end,

    %% 格式化摘要
    Summaries = lists:map(fun(#{role := Role, content := Content}) ->
        ShortContent = truncate_content(Content, MaxContentLength),
        <<Role/binary, ": ", ShortContent/binary>>
    end, SampleMsgs),

    SummaryText = iolist_to_binary([
        <<"[对话摘要] 共 ">>, integer_to_binary(MsgCount), <<" 条消息\n">>,
        lists:join(<<"\n">>, Summaries)
    ]),

    {ok, SummaryText}.

%% @private 截断内容
-spec truncate_content(binary(), pos_integer()) -> binary().
truncate_content(Content, MaxLength) when byte_size(Content) =< MaxLength ->
    Content;
truncate_content(Content, MaxLength) ->
    <<(binary:part(Content, 0, MaxLength))/binary, "...">>.

%% @private 格式化消息用于摘要
-spec format_messages_for_summary([message()]) -> binary().
format_messages_for_summary(Messages) ->
    FormattedLines = lists:map(fun(#{role := Role, content := Content}) ->
        RoleLabel = role_to_label(Role),
        %% 限制每条消息长度
        ShortContent = truncate_content(Content, 500),
        <<RoleLabel/binary, ": ", ShortContent/binary>>
    end, Messages),
    iolist_to_binary(lists:join(<<"\n\n">>, FormattedLines)).

%% @private 角色标签转换
-spec role_to_label(binary()) -> binary().
role_to_label(<<"user">>) -> <<"用户">>;
role_to_label(<<"assistant">>) -> <<"助手">>;
role_to_label(<<"system">>) -> <<"系统">>;
role_to_label(Role) -> Role.

%% @private 构建摘要 Prompt
-spec build_summary_prompt(binary(), binary(), non_neg_integer()) -> binary().
build_summary_prompt(Template, ConversationText, MsgCount) ->
    %% 简单的模板替换
    T1 = binary:replace(Template, <<"~B">>, integer_to_binary(MsgCount)),
    binary:replace(T1, <<"~s">>, ConversationText).

%% @private 调用 LLM
-spec call_llm(atom(), [message()], map()) -> {ok, map()} | {error, term()}.
call_llm(Provider, Messages, Opts) ->
    %% 检查 agent_llm 是否可用
    case code:ensure_loaded(beamai_chat_completion) of
        {module, beamai_chat_completion} ->
            try
                beamai_chat_completion:chat(Provider, Messages, Opts)
            catch
                _:Reason ->
                    {error, {llm_call_failed, Reason}}
            end;
        {error, _} ->
            {error, {module_not_available, beamai_chat_completion}}
    end.

%% @private 从 LLM 响应中提取摘要
-spec extract_summary_from_response(map()) -> {ok, binary()} | {error, term()}.
extract_summary_from_response(#{<<"choices">> := [Choice | _]}) ->
    %% OpenAI 格式
    case Choice of
        #{<<"message">> := #{<<"content">> := Content}} ->
            {ok, Content};
        _ ->
            {error, invalid_response_format}
    end;
extract_summary_from_response(#{<<"content">> := [Block | _]}) ->
    %% Anthropic 格式
    case Block of
        #{<<"text">> := Text} ->
            {ok, Text};
        _ ->
            {error, invalid_response_format}
    end;
extract_summary_from_response(#{content := Content}) when is_binary(Content) ->
    %% 简化格式
    {ok, Content};
extract_summary_from_response(_) ->
    {error, invalid_response_format}.
