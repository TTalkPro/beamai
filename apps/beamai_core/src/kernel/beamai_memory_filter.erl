%%%-------------------------------------------------------------------
%%% @doc Memory 过滤器：会话历史的存储与注入
%%%
%%% 通过 pre_chat/post_chat 两个过滤器，把会话历史的存储完全从
%%% kernel 中剥离：
%%%
%%% - pre_chat（优先级 -1000，最先执行）：
%%%   读取 context 的 conversation_id，把本轮传入的 delta 消息存入 store，
%%%   再用 store 里的完整历史替换 messages 发给 LLM。
%%%   无 conversation_id 时原样透传（退化为单次无状态调用）。
%%%
%%% - post_chat（优先级 +1000，最后执行）：
%%%   把 LLM 的 assistant 回复存入 store。
%%%
%%% store 句柄通过闭包绑定，符合 beamai_chat_memory:handle/0。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_filter).

%% API
-export([memory_filters/1]).
-export([pre_chat_filter/1, post_chat_filter/1]).
-export([response_to_message/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 返回绑定 store 的 [pre_chat, post_chat] 过滤器
-spec memory_filters(beamai_chat_memory:handle()) -> [beamai_filter:filter_spec()].
memory_filters(Store) ->
    [pre_chat_filter(Store), post_chat_filter(Store)].

%% @doc 构造 pre_chat 过滤器：存 delta + 用完整历史替换 messages
-spec pre_chat_filter(beamai_chat_memory:handle()) -> beamai_filter:filter_spec().
pre_chat_filter(Store) ->
    beamai_filter:new(<<"memory_pre">>, pre_chat, fun(FilterCtx) ->
        Context = maps:get(context, FilterCtx),
        case beamai_context:conversation_id(Context) of
            undefined ->
                {continue, FilterCtx};
            ConvId ->
                Delta = maps:get(messages, FilterCtx, []),
                ok = beamai_chat_memory:mem_add(Store, ConvId, Delta),
                Full = beamai_chat_memory:mem_get(Store, ConvId),
                {continue, FilterCtx#{messages => Full}}
        end
    end, -1000).

%% @doc 构造 post_chat 过滤器：存 assistant 回复
-spec post_chat_filter(beamai_chat_memory:handle()) -> beamai_filter:filter_spec().
post_chat_filter(Store) ->
    beamai_filter:new(<<"memory_post">>, post_chat, fun(FilterCtx) ->
        Context = maps:get(context, FilterCtx),
        case beamai_context:conversation_id(Context) of
            undefined ->
                {continue, FilterCtx};
            ConvId ->
                Response = maps:get(result, FilterCtx),
                case response_to_message(Response) of
                    undefined -> ok;
                    Msg -> ok = beamai_chat_memory:mem_add(Store, ConvId, [Msg])
                end,
                {continue, FilterCtx}
        end
    end, 1000).

%% @doc 把 LLM 响应转为中性 assistant 消息（无可存内容返回 undefined）
-spec response_to_message(term()) -> beamai_message:message() | undefined.
response_to_message(Response) ->
    case beamai_llm_response:has_tool_calls(Response) of
        true ->
            beamai_message:tool_calls(beamai_llm_response:tool_calls(Response));
        false ->
            case beamai_llm_response:content(Response) of
                null -> undefined;
                Content -> beamai_message:assistant(Content)
            end
    end.
