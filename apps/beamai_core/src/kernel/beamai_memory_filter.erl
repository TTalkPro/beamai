%%%-------------------------------------------------------------------
%%% @doc Memory Filter：会话历史的存储与注入（kernel 级，洋葱式）
%%%
%%% 这是 **kernel 级**的会话记忆：给"直接用 beamai_kernel / beamai facade"的
%%% 调用方提供 filter 形态的记忆——构建 kernel 时放进 filters 列表**首位**
%%% （最外层：先展开完整历史，再让内层 filter 处理），如
%%% `beamai_kernel:new(Settings, [beamai_memory_filter:memory_filter(Store) | Rest])`。
%%%
%%% 注意：beamai_agent 层**不再**用本 filter——Agent 在自己的 tool loop 里通过
%%% beamai_memory_provider 显式编排记忆（见该模块）。两者互不影响。
%%%
%%% 把会话历史管理收敛到**单个** around_chat hook（delta 模式）：
%%% - 前置（调 LLM 前）：读 context 的 conversation_id，把本轮 delta 存入 store，
%%%   再用 store 里的完整历史替换 messages 发给 LLM。
%%% - 后置（调 LLM 后）：把 LLM 的 assistant 回复存入 store。
%%%
%%% 无 conversation_id 时原样透传（退化为单次无状态调用）。
%%%
%%% **与 Spring AI MessageChatMemoryAdvisor 的一处对照**：它有个
%%% isMemoryAlreadyInPrompt 滑窗查重，防的是「历史被重复**前置**进提示词」——
%%% 那是它 prepend 语义的产物。本 filter 是 **replace** 语义（用 store 的完整
%%% 历史整个替换 messages），结构上就不可能重复前置，故不需要对应的查重。
%%%
%%% 唯一的重入隐患是外层 filter 拿同一 delta 重跑内层（同一 delta 会被存两次）。
%%% 这要求把重试类 filter 放在本 filter **之外**，与「memory 放列表首位」的约定
%%% 相悖；且真要修得干净，得让「存 delta + 存回复」整体幂等，不是加个查重能了事
%%% 的。故此处不设半吊子防护——按约定放首位即无此问题。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_filter).

%% API
-export([memory_filter/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 构造绑定 store 的 memory filter（around_chat；放 filters 列表首位）
-spec memory_filter(beamai_chat_memory:handle()) -> beamai_filter:filter().
memory_filter(Store) ->
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
                    case beamai_message:from_response(Response) of
                        undefined -> ok;
                        Msg -> ok = beamai_chat_memory:mem_add(Store, ConvId, [Msg])
                    end,
                    Resp
            end
        end
    }).
