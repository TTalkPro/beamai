%%%-------------------------------------------------------------------
%%% @doc Summarization Middleware - 对话历史摘要压缩
%%%
%%% 在模型调用前检查消息历史长度，自动进行摘要压缩：
%%% - 滑动窗口：保留最近 N 条消息原文
%%% - Token 限制：确保上下文不超过限制
%%% - 智能摘要：将旧消息压缩为摘要（可选）
%%%
%%% == 配置选项 ==
%%%
%%% ```erlang
%%% {middleware_summarization, #{
%%%     window_size => 20,        %% 滑动窗口大小（默认 20）
%%%     max_tokens => 4000,       %% 最大 Token 数（默认 4000）
%%%     summarize => true,        %% 是否启用摘要（默认 false）
%%%     summarize_fn => fun(...), %% 自定义摘要函数
%%%     preserve_system => true   %% 保留系统消息（默认 true）
%%% }}
%%% ```
%%%
%%% == 工作流程 ==
%%%
%%% 1. before_model 钩子触发
%%% 2. 检查当前消息数量和 Token 估算
%%% 3. 如果超过阈值：
%%%    a. 分离系统消息
%%%    b. 应用滑动窗口
%%%    c. 对旧消息生成摘要（如果启用）
%%%    d. 更新图状态中的 messages
%%% 4. 继续正常的 LLM 调用
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_summarization).

-behaviour(beamai_middleware).

%% Middleware 回调
-export([init/1, before_model/2]).

%% 内部使用
-export([should_compress/2, compress_messages/2]).

%%====================================================================
%% Middleware 回调
%%====================================================================

%% @doc 初始化 Middleware 状态
-spec init(map()) -> map().
init(Opts) ->
    %% 创建 buffer 配置
    BufferConfig = beamai_conversation_buffer:new(#{
        window_size => maps:get(window_size, Opts, 20),
        max_tokens => maps:get(max_tokens, Opts, 4000),
        summarize => maps:get(summarize, Opts, false),
        summarize_fn => maps:get(summarize_fn, Opts, undefined),
        preserve_system => maps:get(preserve_system, Opts, true)
    }),

    #{
        buffer_config => BufferConfig,
        %% 压缩阈值：当消息数量超过此值时触发检查
        compress_threshold => maps:get(compress_threshold, Opts, 30),
        %% Token 阈值：当估算 Token 超过此值时强制压缩
        token_threshold => maps:get(token_threshold, Opts, 3000),
        %% 是否在 after_agent 时也压缩（用于持久化）
        compress_on_finish => maps:get(compress_on_finish, Opts, false),
        %% 调试模式
        debug => maps:get(debug, Opts, false)
    }.

%% @doc 模型调用前检查并压缩消息
-spec before_model(map(), map()) -> beamai_middleware:middleware_result().
before_model(State, MwState) ->
    Messages = graph:get(State, messages, []),

    case should_compress(Messages, MwState) of
        false ->
            %% 不需要压缩
            ok;
        true ->
            %% 执行压缩
            case compress_messages(Messages, MwState) of
                {ok, CompressedMessages, Summary} ->
                    maybe_log_compression(Messages, CompressedMessages, Summary, MwState),
                    Updates = build_updates(CompressedMessages, Summary, State),
                    {update, Updates};
                {error, Reason} ->
                    logger:warning("消息压缩失败: ~p，使用原始消息", [Reason]),
                    ok
            end
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 判断是否需要压缩
-spec should_compress([map()], map()) -> boolean().
should_compress(Messages, MwState) ->
    #{buffer_config := BufferConfig,
      compress_threshold := CompressThreshold,
      token_threshold := TokenThreshold} = MwState,

    MsgCount = length(Messages),
    EstimatedTokens = beamai_conversation_buffer:count_tokens(BufferConfig, Messages),

    %% 消息数量或 Token 数量超过阈值时压缩
    MsgCount > CompressThreshold orelse EstimatedTokens > TokenThreshold.

%% @doc 执行消息压缩
-spec compress_messages([map()], map()) ->
    {ok, [map()], binary() | undefined} | {error, term()}.
compress_messages(Messages, #{buffer_config := BufferConfig}) ->
    case beamai_conversation_buffer:build_context(BufferConfig, Messages) of
        {ok, #{messages := CompressedMsgs, summary := Summary}} ->
            {ok, CompressedMsgs, Summary};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private 构建状态更新
-spec build_updates([map()], binary() | undefined, map()) -> map().
build_updates(CompressedMessages, Summary, State) ->
    %% 保存原始完整消息到 full_messages（用于持久化）
    OriginalMessages = graph:get(State, messages, []),
    FullMessages = graph:get(State, full_messages, OriginalMessages),

    Updates = #{
        messages => CompressedMessages,
        full_messages => FullMessages,
        compression_applied => true
    },

    case Summary of
        undefined -> Updates;
        <<>> -> Updates;
        _ -> Updates#{conversation_summary => Summary}
    end.

%% @private 记录压缩日志（调试模式）
-spec maybe_log_compression([map()], [map()], binary() | undefined, map()) -> ok.
maybe_log_compression(Original, Compressed, Summary, #{debug := true}) ->
    logger:info("消息压缩: ~p -> ~p 条, 摘要: ~s",
                [length(Original), length(Compressed),
                 case Summary of undefined -> "无"; _ -> "有" end]);
maybe_log_compression(_, _, _, _) ->
    ok.
