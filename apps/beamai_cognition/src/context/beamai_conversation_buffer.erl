%%%-------------------------------------------------------------------
%%% @doc Agent Conversation Buffer - 对话缓冲管理模块
%%%
%%% 提供对话消息的智能管理，包括：
%%% - 滑动窗口：保留最近 N 条消息原文
%%% - 摘要压缩：将旧消息压缩为摘要
%%% - Token 管理：确保上下文不超过 Token 限制
%%%
%%% == 设计理念 ==
%%%
%%% Buffer 是 Checkpointer 的上层抽象：
%%% - Checkpointer 负责存储完整历史（支持回溯）
%%% - Buffer 负责构建 LLM 上下文（控制大小）
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建 Buffer 配置
%%% Config = beamai_conversation_buffer:new(#{
%%%     window_size => 20,
%%%     max_tokens => 4000,
%%%     summarize => true
%%% }),
%%%
%%% %% 从完整消息列表构建上下文
%%% Messages = [...],  % 完整历史
%%% {ok, Context} = beamai_conversation_buffer:build_context(Config, Messages),
%%%
%%% %% 或直接从 Memory 加载并构建
%%% {ok, Context} = beamai_conversation_buffer:load_context(Config, Memory, ThreadConfig),
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_conversation_buffer).


%%====================================================================
%% 类型定义
%%====================================================================

-type buffer_config() :: #{
    %% 滑动窗口大小（保留最近 N 条原文）
    window_size := pos_integer(),

    %% 最大 Token 数量
    max_tokens := pos_integer(),

    %% 是否启用摘要压缩
    summarize := boolean(),

    %% 摘要函数（需要外部提供，默认使用简单截断）
    summarize_fn => summarize_fn(),

    %% Token 计数函数（默认使用简单估算）
    token_count_fn => token_count_fn(),

    %% 摘要最大 Token 数
    summary_max_tokens => pos_integer(),

    %% 系统消息是否始终保留
    preserve_system => boolean()
}.

-type summarize_fn() :: fun(([message()]) -> {ok, binary()} | {error, term()}).
-type token_count_fn() :: fun((binary() | message()) -> non_neg_integer()).
-type message() :: #{role := atom() | binary(), content := binary(), atom() => term()}.

-type context() :: #{
    %% 用于 LLM 的消息列表
    messages := [message()],

    %% 摘要（如果有）
    summary => binary(),

    %% 统计信息
    stats := context_stats()
}.

-type context_stats() :: #{
    %% 原始消息总数
    total_messages := non_neg_integer(),

    %% 上下文中的消息数
    context_messages := non_neg_integer(),

    %% 被压缩的消息数
    summarized_messages := non_neg_integer(),

    %% 估算的 Token 数
    estimated_tokens := non_neg_integer()
}.

-export_type([buffer_config/0, context/0, message/0, context_stats/0]).

%%====================================================================
%% API 导出
%%====================================================================

%% 配置
-export([
    new/0,
    new/1,
    default_config/0
]).

%% 核心功能
-export([
    build_context/2,
    build_context/3,
    load_context/3,
    add_message/2,
    trim_to_tokens/2
]).

%% 窗口操作
-export([
    apply_window/2,
    split_by_window/2
]).

%% 摘要操作
-export([
    summarize_messages/2,
    set_summarize_fn/2
]).

%% Token 操作
-export([
    count_tokens/2,
    estimate_tokens/1,
    set_token_count_fn/2
]).

%% 工具函数
-export([
    get_system_messages/1,
    get_non_system_messages/1,
    message_to_text/1
]).

%%====================================================================
%% 默认值
%%====================================================================

-define(DEFAULT_WINDOW_SIZE, 20).
-define(DEFAULT_MAX_TOKENS, 4000).
-define(DEFAULT_SUMMARY_MAX_TOKENS, 500).
-define(CHARS_PER_TOKEN, 4).  % 简单估算：4 字符 ≈ 1 token

%%====================================================================
%% 配置函数
%%====================================================================

%% @doc 创建默认配置的 Buffer
-spec new() -> buffer_config().
new() ->
    new(#{}).

%% @doc 创建 Buffer 配置
%%
%% Opts 支持：
%% - window_size: 滑动窗口大小（默认 20）
%% - max_tokens: 最大 Token 数（默认 4000）
%% - summarize: 是否启用摘要（默认 false）
%% - summarize_fn: 自定义摘要函数
%% - token_count_fn: 自定义 Token 计数函数
%% - preserve_system: 是否保留系统消息（默认 true）
-spec new(map()) -> buffer_config().
new(Opts) ->
    Default = default_config(),
    maps:merge(Default, Opts).

%% @doc 获取默认配置
-spec default_config() -> buffer_config().
default_config() ->
    #{
        window_size => ?DEFAULT_WINDOW_SIZE,
        max_tokens => ?DEFAULT_MAX_TOKENS,
        summarize => false,
        summarize_fn => fun default_summarize/1,
        token_count_fn => fun estimate_tokens/1,
        summary_max_tokens => ?DEFAULT_SUMMARY_MAX_TOKENS,
        preserve_system => true
    }.

%%====================================================================
%% 核心功能
%%====================================================================

%% @doc 从消息列表构建上下文
%%
%% 应用滑动窗口和摘要压缩策略，构建适合 LLM 的上下文。
-spec build_context(buffer_config(), [message()]) -> {ok, context()}.
build_context(Config, Messages) ->
    build_context(Config, Messages, #{}).

%% @doc 从消息列表构建上下文（带选项）
%%
%% ExtraOpts 支持：
%% - force_summarize: 强制生成摘要
%% - include_summary: 是否在消息中包含摘要
-spec build_context(buffer_config(), [message()], map()) -> {ok, context()}.
build_context(Config, Messages, ExtraOpts) ->
    WindowSize = maps:get(window_size, Config),
    MaxTokens = maps:get(max_tokens, Config),
    PreserveSystem = maps:get(preserve_system, Config, true),

    %% 1. 分离系统消息
    {SystemMsgs, OtherMsgs} = case PreserveSystem of
        true -> partition_system_messages(Messages);
        false -> {[], Messages}
    end,

    %% 2. 应用滑动窗口
    {RecentMsgs, OlderMsgs} = split_by_window(OtherMsgs, WindowSize),

    %% 3. 处理旧消息（摘要或丢弃）
    {Summary, SummarizedCount} = maybe_summarize(Config, OlderMsgs, ExtraOpts),

    %% 4. 组装消息列表
    ContextMsgs0 = SystemMsgs ++ maybe_add_summary_message(Summary, ExtraOpts) ++ RecentMsgs,

    %% 5. 确保不超过 Token 限制
    {ContextMsgs, TrimmedTokens} = trim_to_tokens(Config, ContextMsgs0, MaxTokens),

    %% 6. 构建统计信息
    TokenCountFn = maps:get(token_count_fn, Config, fun estimate_tokens/1),
    EstimatedTokens = lists:sum([TokenCountFn(M) || M <- ContextMsgs]),

    Stats = #{
        total_messages => length(Messages),
        context_messages => length(ContextMsgs),
        summarized_messages => SummarizedCount,
        estimated_tokens => EstimatedTokens,
        trimmed_tokens => TrimmedTokens
    },

    Context = #{
        messages => ContextMsgs,
        summary => Summary,
        stats => Stats
    },

    {ok, Context}.

%% @doc 从 Process Snapshot Manager 加载并构建上下文
-spec load_context(buffer_config(), beamai_process_snapshot:manager(), map()) ->
    {ok, context()} | {error, term()}.
load_context(Config, Mgr, ThreadConfig) ->
    ThreadId = maps:get(thread_id, ThreadConfig),
    case beamai_process_snapshot:get_latest(Mgr, ThreadId) of
        {ok, Snapshot} ->
            StateMap = beamai_process_snapshot:entry_state(Snapshot),
            Messages = maps:get(messages, StateMap, []),
            build_context(Config, Messages);
        {error, _} = Error ->
            Error
    end.

%% @doc 添加消息并返回更新后的上下文
-spec add_message(context(), message()) -> context().
add_message(#{messages := Messages, stats := Stats} = Context, Message) ->
    NewMessages = Messages ++ [Message],
    NewStats = Stats#{
        total_messages := maps:get(total_messages, Stats, 0) + 1,
        context_messages := length(NewMessages)
    },
    Context#{messages := NewMessages, stats := NewStats}.

%% @doc 将消息列表裁剪到指定 Token 数量内
-spec trim_to_tokens(buffer_config(), [message()]) -> [message()].
trim_to_tokens(Config, Messages) ->
    MaxTokens = maps:get(max_tokens, Config),
    {Trimmed, _} = trim_to_tokens(Config, Messages, MaxTokens),
    Trimmed.

%%====================================================================
%% 窗口操作
%%====================================================================

%% @doc 应用滑动窗口，返回最近 N 条消息
-spec apply_window([message()], pos_integer()) -> [message()].
apply_window(Messages, WindowSize) ->
    Len = length(Messages),
    if
        Len =< WindowSize -> Messages;
        true -> lists:nthtail(Len - WindowSize, Messages)
    end.

%% @doc 按窗口大小分割消息
%%
%% 返回 {RecentMessages, OlderMessages}
-spec split_by_window([message()], pos_integer()) -> {[message()], [message()]}.
split_by_window(Messages, WindowSize) ->
    Len = length(Messages),
    if
        Len =< WindowSize ->
            {Messages, []};
        true ->
            SplitPoint = Len - WindowSize,
            {lists:nthtail(SplitPoint, Messages),
             lists:sublist(Messages, SplitPoint)}
    end.

%%====================================================================
%% 摘要操作
%%====================================================================

%% @doc 对消息列表生成摘要
-spec summarize_messages(buffer_config(), [message()]) ->
    {ok, binary()} | {error, term()}.
summarize_messages(_Config, []) ->
    {ok, <<>>};
summarize_messages(Config, Messages) ->
    SummarizeFn = maps:get(summarize_fn, Config, fun default_summarize/1),
    SummarizeFn(Messages).

%% @doc 设置摘要函数
-spec set_summarize_fn(buffer_config(), summarize_fn()) -> buffer_config().
set_summarize_fn(Config, Fn) ->
    Config#{summarize_fn => Fn}.

%%====================================================================
%% Token 操作
%%====================================================================

%% @doc 计算消息的 Token 数量
-spec count_tokens(buffer_config(), message() | [message()]) -> non_neg_integer().
count_tokens(Config, Messages) when is_list(Messages) ->
    TokenCountFn = maps:get(token_count_fn, Config, fun estimate_tokens/1),
    lists:sum([TokenCountFn(M) || M <- Messages]);
count_tokens(Config, Message) ->
    TokenCountFn = maps:get(token_count_fn, Config, fun estimate_tokens/1),
    TokenCountFn(Message).

%% @doc 估算 Token 数量（简单实现）
%%
%% 使用字符数 / 4 的简单估算，适用于英文。
%% 中文建议使用更精确的 tokenizer。
-spec estimate_tokens(message() | binary()) -> non_neg_integer().
estimate_tokens(#{content := Content}) when is_binary(Content) ->
    %% 消息还有 role 等开销，加上一些余量
    byte_size(Content) div ?CHARS_PER_TOKEN + 4;
estimate_tokens(#{content := Content}) when is_list(Content) ->
    %% 多模态消息，估算文本部分
    TextParts = [Text || #{type := <<"text">>, text := Text} <- Content],
    TotalSize = lists:sum([byte_size(T) || T <- TextParts]),
    TotalSize div ?CHARS_PER_TOKEN + 4;
estimate_tokens(Text) when is_binary(Text) ->
    byte_size(Text) div ?CHARS_PER_TOKEN.

%% @doc 设置 Token 计数函数
-spec set_token_count_fn(buffer_config(), token_count_fn()) -> buffer_config().
set_token_count_fn(Config, Fn) ->
    Config#{token_count_fn => Fn}.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 获取系统消息
-spec get_system_messages([message()]) -> [message()].
get_system_messages(Messages) ->
    [M || M = #{role := Role} <- Messages, Role =:= system orelse Role =:= <<"system">>].

%% @doc 获取非系统消息
-spec get_non_system_messages([message()]) -> [message()].
get_non_system_messages(Messages) ->
    [M || M = #{role := Role} <- Messages, Role =/= system andalso Role =/= <<"system">>].

%% @doc 将消息转换为文本
-spec message_to_text(message()) -> binary().
message_to_text(#{role := Role, content := Content}) when is_binary(Content) ->
    RoleBin = role_to_binary(Role),
    <<RoleBin/binary, ": ", Content/binary>>;
message_to_text(#{role := Role, content := Content}) when is_list(Content) ->
    %% 多模态消息
    RoleBin = role_to_binary(Role),
    TextParts = [Text || #{type := <<"text">>, text := Text} <- Content],
    CombinedText = iolist_to_binary(lists:join(<<" ">>, TextParts)),
    <<RoleBin/binary, ": ", CombinedText/binary>>.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 分离系统消息和其他消息
-spec partition_system_messages([message()]) -> {[message()], [message()]}.
partition_system_messages(Messages) ->
    lists:partition(fun(#{role := Role}) ->
        Role =:= system orelse Role =:= <<"system">>
    end, Messages).

%% @private 根据配置决定是否生成摘要
-spec maybe_summarize(buffer_config(), [message()], map()) ->
    {binary() | undefined, non_neg_integer()}.
maybe_summarize(_Config, [], _Opts) ->
    {undefined, 0};
maybe_summarize(Config, OlderMsgs, Opts) ->
    Summarize = maps:get(summarize, Config, false),
    ForceSummarize = maps:get(force_summarize, Opts, false),

    case Summarize orelse ForceSummarize of
        true ->
            case summarize_messages(Config, OlderMsgs) of
                {ok, Summary} when byte_size(Summary) > 0 ->
                    {Summary, length(OlderMsgs)};
                _ ->
                    {undefined, 0}
            end;
        false ->
            {undefined, 0}
    end.

%% @private 可能添加摘要消息到上下文
-spec maybe_add_summary_message(binary() | undefined, map()) -> [message()].
maybe_add_summary_message(undefined, _Opts) ->
    [];
maybe_add_summary_message(<<>>, _Opts) ->
    [];
maybe_add_summary_message(Summary, Opts) ->
    IncludeSummary = maps:get(include_summary, Opts, true),
    case IncludeSummary of
        true ->
            [beamai_message:system(<<"[对话历史摘要]\n", Summary/binary>>)];
        false ->
            []
    end.

%% @private 裁剪消息到指定 Token 数量内
-spec trim_to_tokens(buffer_config(), [message()], pos_integer()) ->
    {[message()], non_neg_integer()}.
trim_to_tokens(Config, Messages, MaxTokens) ->
    TokenCountFn = maps:get(token_count_fn, Config, fun estimate_tokens/1),
    trim_messages_loop(Messages, MaxTokens, TokenCountFn, [], 0).

%% @private 裁剪循环（从前向后添加，累加 Token 数）
-spec trim_messages_loop([message()], pos_integer(), token_count_fn(),
                         [message()], non_neg_integer()) ->
    {[message()], non_neg_integer()}.
trim_messages_loop([], _MaxTokens, _TokenCountFn, Acc, _UsedTokens) ->
    {lists:reverse(Acc), 0};
trim_messages_loop([Msg | Rest], MaxTokens, TokenCountFn, Acc, UsedTokens) ->
    MsgTokens = TokenCountFn(Msg),
    case UsedTokens + MsgTokens =< MaxTokens of
        true ->
            trim_messages_loop(Rest, MaxTokens, TokenCountFn,
                               [Msg | Acc], UsedTokens + MsgTokens);
        false ->
            %% 超出限制，计算剩余被裁剪的 Token 数
            TrimmedTokens = MsgTokens + lists:sum([TokenCountFn(M) || M <- Rest]),
            {lists:reverse(Acc), TrimmedTokens}
    end.

%% @private 默认摘要函数（简单截断）
%%
%% 生产环境应替换为 LLM 摘要
-spec default_summarize([message()]) -> {ok, binary()}.
default_summarize([]) ->
    {ok, <<>>};
default_summarize(Messages) ->
    %% 简单实现：提取关键信息
    MsgCount = length(Messages),

    %% 提取每条消息的简短描述
    Summaries = lists:map(fun(#{role := Role, content := Content}) ->
        %% 截取前 100 个字符
        ShortContent = case byte_size(Content) > 100 of
            true -> <<(binary:part(Content, 0, 100))/binary, "...">>;
            false -> Content
        end,
        RoleBin = role_to_binary(Role),
        <<RoleBin/binary, ": ", ShortContent/binary>>
    end, lists:sublist(Messages, 5)),  % 最多取 5 条

    SummaryText = iolist_to_binary([
        <<"共 ">>, integer_to_binary(MsgCount), <<" 条历史消息。\n">>,
        <<"部分内容：\n">>,
        lists:join(<<"\n">>, Summaries)
    ]),

    {ok, SummaryText}.

%% @private 将 role 转为 binary（兼容 atom 和 binary）
-spec role_to_binary(atom() | binary()) -> binary().
role_to_binary(Role) when is_atom(Role) -> atom_to_binary(Role, utf8);
role_to_binary(Role) when is_binary(Role) -> Role.
