%%%-------------------------------------------------------------------
%%% @doc Context Editing Middleware - 上下文编辑
%%%
%%% 管理对话上下文，清理旧的工具输出以减少 token 消耗。
%%%
%%% == 功能特性 ==
%%%
%%% - 自动清理旧的工具调用结果
%%% - 保留最近的工具输出
%%% - 可配置保留策略
%%% - 支持选择性保留重要内容
%%% - token 使用优化
%%%
%%% == 配置示例 ==
%%%
%%% ```erlang
%%% {middleware_context_editing, #{
%%%     %% 保留最近的工具结果数量
%%%     keep_recent_tool_results => 3,
%%%
%%%     %% 清理旧工具结果的替换文本
%%%     placeholder => <<"[Tool output cleared]">>,
%%%
%%%     %% 触发清理的消息数量阈值
%%%     trigger_message_count => 10,
%%%
%%%     %% 触发清理的 token 阈值（估算）
%%%     trigger_token_count => 4000,
%%%
%%%     %% 保留的工具类型（这些工具的输出不会被清理）
%%%     preserve_tools => [<<"search">>, <<"read_file">>],
%%%
%%%     %% 是否清理 assistant 消息中的工具调用
%%%     clear_tool_calls => false,
%%%
%%%     %% 调试模式
%%%     debug => false
%%% }}
%%% ```
%%%
%%% == 工作流程 ==
%%%
%%% 1. before_model: 检查上下文大小
%%% 2. 如果超过阈值，执行清理
%%% 3. 替换旧的工具结果为占位符
%%% 4. 保留最近的工具结果
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_context_editing).

-behaviour(beamai_middleware).

%% Middleware 回调
-export([init/1, before_model/2]).

%% 工具函数
-export([
    clear_old_tool_results/2,
    estimate_tokens/1,
    get_editing_stats/1
]).

%%====================================================================
%% Middleware 回调
%%====================================================================

%% @doc 初始化 Middleware 状态
-spec init(map()) -> map().
init(Opts) ->
    #{
        keep_recent_tool_results => maps:get(keep_recent_tool_results, Opts, 3),
        placeholder => maps:get(placeholder, Opts, <<"[Previous tool output cleared to save context]">>),
        trigger_message_count => maps:get(trigger_message_count, Opts, 10),
        trigger_token_count => maps:get(trigger_token_count, Opts, 4000),
        preserve_tools => maps:get(preserve_tools, Opts, []),
        clear_tool_calls => maps:get(clear_tool_calls, Opts, false),
        debug => maps:get(debug, Opts, false)
    }.

%% @doc 模型调用前检查并清理上下文
-spec before_model(map(), map()) -> beamai_middleware:middleware_result().
before_model(State, MwState) ->
    #{trigger_message_count := TriggerMsgCount,
      trigger_token_count := TriggerTokenCount,
      debug := Debug} = MwState,

    Messages = graph:get(State, messages, []),
    MessageCount = length(Messages),

    %% 检查是否需要清理
    ShouldClean = case MessageCount >= TriggerMsgCount of
        true ->
            true;
        false ->
            %% 估算 token 数量
            EstimatedTokens = estimate_tokens(Messages),
            EstimatedTokens >= TriggerTokenCount
    end,

    case ShouldClean of
        false ->
            ok;
        true ->
            %% 执行清理
            {CleanedMessages, CleanedCount} = clear_old_tool_results(Messages, MwState),

            case CleanedCount > 0 of
                true ->
                    case Debug of
                        true ->
                            OriginalTokens = estimate_tokens(Messages),
                            NewTokens = estimate_tokens(CleanedMessages),
                            logger:info("[ContextEditing] 清理了 ~p 个工具结果，token: ~p -> ~p",
                                       [CleanedCount, OriginalTokens, NewTokens]);
                        false ->
                            ok
                    end,

                    %% 更新统计
                    TotalCleaned = graph:get(State, mw_context_cleaned_count, 0) + CleanedCount,
                    {update, #{
                        messages => CleanedMessages,
                        mw_context_cleaned_count => TotalCleaned,
                        mw_last_context_edit => erlang:system_time(millisecond)
                    }};
                false ->
                    ok
            end
    end.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 清理旧的工具结果
-spec clear_old_tool_results([map()], map()) -> {[map()], non_neg_integer()}.
clear_old_tool_results(Messages, MwState) ->
    #{keep_recent_tool_results := KeepRecent,
      placeholder := Placeholder,
      preserve_tools := PreserveTools,
      clear_tool_calls := ClearToolCalls} = MwState,

    %% 找出所有工具结果消息的索引
    ToolResultIndices = find_tool_result_indices(Messages),

    %% 确定要清理的索引（保留最近的）
    ToClean = case length(ToolResultIndices) > KeepRecent of
        true ->
            lists:sublist(ToolResultIndices, length(ToolResultIndices) - KeepRecent);
        false ->
            []
    end,

    %% 执行清理
    {CleanedMessages, CleanedCount} = clean_messages(Messages, ToClean, Placeholder,
                                                      PreserveTools, ClearToolCalls, 0),

    {CleanedMessages, CleanedCount}.

%% @doc 估算消息的 token 数量
%%
%% 使用简单的启发式方法：
%% - 英文：约 4 字符 = 1 token
%% - 中文：约 2 字符 = 1 token
%% - 平均：约 3 字符 = 1 token
-spec estimate_tokens([map()]) -> non_neg_integer().
estimate_tokens(Messages) ->
    TotalChars = lists:sum([estimate_message_chars(M) || M <- Messages]),
    TotalChars div 3.

%% @doc 获取编辑统计
-spec get_editing_stats(map()) -> map().
get_editing_stats(State) ->
    #{
        total_cleaned => graph:get(State, mw_context_cleaned_count, 0),
        last_edit_time => graph:get(State, mw_last_context_edit, undefined)
    }.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 找出工具结果消息的索引
-spec find_tool_result_indices([map()]) -> [pos_integer()].
find_tool_result_indices(Messages) ->
    {Indices, _} = lists:foldl(fun(Msg, {Acc, Idx}) ->
        case is_tool_result_message(Msg) of
            true -> {[Idx | Acc], Idx + 1};
            false -> {Acc, Idx + 1}
        end
    end, {[], 1}, Messages),
    lists:reverse(Indices).

%% @private 判断是否为工具结果消息
-spec is_tool_result_message(map()) -> boolean().
is_tool_result_message(#{<<"role">> := <<"tool">>}) -> true;
is_tool_result_message(#{role := tool}) -> true;
is_tool_result_message(#{<<"role">> := <<"function">>}) -> true;
is_tool_result_message(#{role := function}) -> true;
is_tool_result_message(_) -> false.

%% @private 清理消息
-spec clean_messages([map()], [pos_integer()], binary(), [binary()],
                     boolean(), non_neg_integer()) ->
    {[map()], non_neg_integer()}.
clean_messages(Messages, ToClean, Placeholder, PreserveTools, ClearToolCalls, CleanedCount) ->
    {CleanedList, FinalCount} = lists:foldl(fun({Idx, Msg}, {Acc, Count}) ->
        case lists:member(Idx, ToClean) of
            true ->
                %% 检查是否应该保留
                case should_preserve(Msg, PreserveTools) of
                    true ->
                        {[Msg | Acc], Count};
                    false ->
                        %% 清理工具结果
                        CleanedMsg = clear_tool_content(Msg, Placeholder),
                        {[CleanedMsg | Acc], Count + 1}
                end;
            false ->
                %% 可选：清理 assistant 消息中的工具调用
                case ClearToolCalls andalso is_assistant_with_tool_calls(Msg) of
                    true ->
                        CleanedMsg = clear_assistant_tool_calls(Msg),
                        {[CleanedMsg | Acc], Count};
                    false ->
                        {[Msg | Acc], Count}
                end
        end
    end, {[], CleanedCount}, lists:zip(lists:seq(1, length(Messages)), Messages)),

    {lists:reverse(CleanedList), FinalCount}.

%% @private 检查是否应该保留
-spec should_preserve(map(), [binary()]) -> boolean().
should_preserve(Msg, PreserveTools) ->
    ToolName = get_tool_name_from_message(Msg),
    lists:member(ToolName, PreserveTools).

%% @private 从消息获取工具名称
-spec get_tool_name_from_message(map()) -> binary().
get_tool_name_from_message(#{<<"name">> := Name}) -> Name;
get_tool_name_from_message(#{name := Name}) when is_binary(Name) -> Name;
get_tool_name_from_message(#{<<"tool_call_id">> := _} = Msg) ->
    %% 尝试从关联的工具调用获取名称
    maps:get(<<"name">>, Msg, maps:get(name, Msg, <<>>));
get_tool_name_from_message(_) -> <<>>.

%% @private 清理工具内容
-spec clear_tool_content(map(), binary()) -> map().
clear_tool_content(#{<<"content">> := _} = Msg, Placeholder) ->
    Msg#{<<"content">> => Placeholder};
clear_tool_content(#{content := _} = Msg, Placeholder) ->
    Msg#{content => Placeholder};
clear_tool_content(Msg, _) ->
    Msg.

%% @private 判断是否为带工具调用的 assistant 消息
-spec is_assistant_with_tool_calls(map()) -> boolean().
is_assistant_with_tool_calls(#{<<"role">> := <<"assistant">>, <<"tool_calls">> := TC})
  when is_list(TC), TC =/= [] -> true;
is_assistant_with_tool_calls(#{role := assistant, tool_calls := TC})
  when is_list(TC), TC =/= [] -> true;
is_assistant_with_tool_calls(_) -> false.

%% @private 清理 assistant 消息中的工具调用
-spec clear_assistant_tool_calls(map()) -> map().
clear_assistant_tool_calls(#{<<"tool_calls">> := TC} = Msg) when is_list(TC) ->
    %% 保留工具调用 ID 和名称，但清理参数
    CleanedTC = lists:map(fun(Call) ->
        case Call of
            #{<<"function">> := Func} ->
                Call#{<<"function">> => Func#{<<"arguments">> => <<"{}">>}};
            _ ->
                Call
        end
    end, TC),
    Msg#{<<"tool_calls">> => CleanedTC};
clear_assistant_tool_calls(Msg) ->
    Msg.

%% @private 估算单条消息的字符数
-spec estimate_message_chars(map()) -> non_neg_integer().
estimate_message_chars(#{<<"content">> := Content}) when is_binary(Content) ->
    byte_size(Content);
estimate_message_chars(#{content := Content}) when is_binary(Content) ->
    byte_size(Content);
estimate_message_chars(#{<<"content">> := ContentList}) when is_list(ContentList) ->
    lists:sum([estimate_content_block_chars(B) || B <- ContentList]);
estimate_message_chars(#{<<"tool_calls">> := TC}) when is_list(TC) ->
    %% 估算工具调用的字符数
    lists:sum([estimate_tool_call_chars(T) || T <- TC]);
estimate_message_chars(_) ->
    0.

%% @private 估算内容块的字符数
-spec estimate_content_block_chars(map()) -> non_neg_integer().
estimate_content_block_chars(#{<<"text">> := Text}) when is_binary(Text) ->
    byte_size(Text);
estimate_content_block_chars(#{<<"type">> := <<"tool_use">>, <<"input">> := Input}) ->
    %% 工具使用块
    case jsx:is_json(jsx:encode(Input)) of
        true -> byte_size(jsx:encode(Input));
        false -> 0
    end;
estimate_content_block_chars(#{<<"type">> := <<"tool_result">>, <<"content">> := Content})
  when is_binary(Content) ->
    byte_size(Content);
estimate_content_block_chars(_) ->
    0.

%% @private 估算工具调用的字符数
-spec estimate_tool_call_chars(map()) -> non_neg_integer().
estimate_tool_call_chars(#{<<"function">> := #{<<"arguments">> := Args}}) when is_binary(Args) ->
    byte_size(Args) + 50;  %% 加上函数名等开销
estimate_tool_call_chars(_) ->
    50.
