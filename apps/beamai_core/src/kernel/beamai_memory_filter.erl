%%%-------------------------------------------------------------------
%%% @doc Memory Filter：会话历史的存储与注入（洋葱式）
%%%
%%% 把会话历史管理收敛到**单个** filter 的 around_chat hook：
%%%
%%% - 前置（调 LLM 前）：读 context 的 conversation_id，把本轮 delta 存入 store，
%%%   再用 store 里的完整历史替换 messages 发给 LLM。
%%% - 后置（调 LLM 后）：把 LLM 的 assistant 回复存入 store。
%%%
%%% 无 conversation_id 时原样透传（退化为单次无状态调用）。前后逻辑同处一个
%%% around 闭包，只需查一次 conversation_id；洋葱链天然保证前置在内层之前、
%%% 后置在内层之后。order 越小越外层，memory 用较小 order 即可包在其它 filter
%%% （如 system prompt 注入）外层。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_filter).

%% API
-export([memory_filter/1, memory_filter/2]).
-export([response_to_message/1]).

%% memory filter 默认 order（较小 → 外层：先展开历史，再让内层注入系统提示）
-define(DEFAULT_ORDER, -1000).

%%====================================================================
%% API
%%====================================================================

%% @doc 构造绑定 store 的 memory filter（around_chat）
-spec memory_filter(beamai_chat_memory:handle()) -> beamai_filter:filter().
memory_filter(Store) ->
    memory_filter(Store, ?DEFAULT_ORDER).

%% @doc 构造 memory filter（指定 order）
-spec memory_filter(beamai_chat_memory:handle(), integer()) -> beamai_filter:filter().
memory_filter(Store, Order) ->
    beamai_filter:new(<<"memory">>, #{
        around_chat => fun(#{messages := Delta, context := Ctx} = Req, _FCtx, Next) ->
            case beamai_context:conversation_id(Ctx) of
                undefined ->
                    %% 无 conversation_id：原样透传，退化为单次无状态调用
                    Next(Req);
                ConvId ->
                    %% 前置：存 delta + 用完整历史替换 messages
                    ok = beamai_chat_memory:mem_add(Store, ConvId, Delta),
                    Full = beamai_chat_memory:mem_get(Store, ConvId),
                    #{response := Response} = Resp = Next(Req#{messages => Full}),
                    %% 后置：存 assistant 回复
                    case response_to_message(Response) of
                        undefined -> ok;
                        Msg -> ok = beamai_chat_memory:mem_add(Store, ConvId, [Msg])
                    end,
                    Resp
            end
        end
    }, Order).

%% @doc 把 LLM 响应转为中性 assistant 消息（无可存内容返回 undefined）
%%
%% 同时保留响应的 content_blocks（含 Anthropic extended-thinking 的 thinking
%% 块与 signature），使该消息回放给 LLM 时适配器能原样拼回 thinking 块——
%% 否则历史前缀与模型真实产出的回合不一致，会破坏 prompt cache 命中。
-spec response_to_message(term()) -> beamai_message:message() | undefined.
response_to_message(Response) ->
    case base_message(Response) of
        undefined ->
            undefined;
        Base ->
            beamai_message:with_content_blocks(
                Base, beamai_llm_response:content_blocks(Response))
    end.

%% @private 根据响应构建基础 assistant 消息（tool_calls 优先，否则取 content）
base_message(Response) ->
    case beamai_llm_response:has_tool_calls(Response) of
        true ->
            beamai_message:tool_calls(beamai_llm_response:tool_calls(Response));
        false ->
            case beamai_llm_response:content(Response) of
                null -> undefined;
                Content -> beamai_message:assistant(Content)
            end
    end.
