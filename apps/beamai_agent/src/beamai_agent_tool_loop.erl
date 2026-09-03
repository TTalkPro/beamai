%%%-------------------------------------------------------------------
%%% @doc Tool Loop 执行模块（Agent 自管编排：full-messages + 显式记忆/回调）
%%%
%%% 驱动 ReAct 工具调用循环。**循环本身是 turn 链上的一环**：`loop_filter/1`
%%% 返回一个 `around_turn` filter（注册在 turn 链最内层），它的 `Next` 不是别的
%%% turn filter，而是 **step 链**——一次迭代（该轮 LLM 调用 + 该轮工具执行）。
%%% 于是层次成为：
%%%
%%%   around_turn（用户 turn filter，每 turn 一次）
%%%     tool_loop filter（本模块：while 调 Next → 判 status → 继续/收尾）
%%%       around_step（每轮迭代一次）
%%%         step_terminal/1（本模块：一次迭代的真正执行）
%%%
%%% 换循环策略（plan-execute / reflexion / 树搜索）＝换掉这个 filter：agent 配置
%%% `loop_filter => fun((LoopOpts) -> beamai_filter:filter())` 即可，agent 与 ChatClient
%%% 代码一行不动（见 beamai_agent:run_turn_chain/3）。
%%%
%%% **记忆与回调都由本模块显式编排**，不经任何 ChatClient filter：
%%%
%%%   - 本轮**完整 messages**（within-run 累积）随 step 请求/响应逐轮穿线。
%%%   - 每轮（step_terminal）：触发 on_llm_call → 经 memory provider 的 prepare
%%%     变换(窗口/摘要) → invoke_chat(_stream) → 把 assistant 回合并入 messages 并
%%%     append 持久化 → 有 tool_calls 则执行(触发 on_tool_call/result)、把工具结果
%%%     并入并持久化 → 返回 status=continue 交回驱动。
%%%   - 循环终止于四种情形：模型不再要工具（正常完成）、整批工具标注
%%%     return_direct（工具结果即最终答案，不回灌模型）、中断（HITL/env_retry）、
%%%     迭代耗尽。
%%%   - 跨轮历史(cross-run)由 memory provider 的 history/append 负责；本模块只负责
%%%     within-run 累积与按序持久化。memory=undefined 时仅在本轮内累积、不持久化。
%%%
%%% chat_opts 带 system_prompts（ChatClient invoke_chat 内按 opts 注入）；context 不再
%%% 固定在 chat_opts 里，而是随 step 请求逐轮穿线（每轮由 step_terminal 覆写进
%%% chat_opts），turn filter 改写的 context 因此对循环全程可见。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_tool_loop).

-export([run/2, loop_filter/1, step_terminal/1]).
-export([build_env_interrupt_context/7]).
-export([agent_result_cb/1]).

-type provider() :: beamai_memory_provider:provider() | undefined.

-type loop_opts() :: #{
    chat_client := beamai_chat_client:chat_client(),
    %% 以下三项是**消息来源**：经 turn 链时由循环驱动按 turn 请求 / continuation
    %% 覆盖填入，直跑 run/2 时由调用方给出
    messages => [map()],            %% 已有上下文（resume 时为中断时携带的完整 messages）
    new_messages => [map()],        %% 本轮新增消息（入口处持久化后并入）
    load_history => boolean(),      %% 是否前接跨轮历史（首轮 true，resume false）
    chat_opts := map(),             %% 含 context 与 system_prompts
    callbacks := map(),
    max_iterations := pos_integer(),
    max_tool_iterations := pos_integer(),  %% agent 配置的总迭代上限（中断上下文计数用）
    %% 整轮 run 的工具调用总数上限，缺省 infinity（不设限）。
    %% 与 max_tool_iterations 正交：后者限"来回几轮"，前者限"总共调了多少次工具"
    %% ——一轮可以并发调 20 个工具，迭代上限拦不住。
    max_tool_calls => pos_integer() | infinity,
    parallel_tools := boolean(),    %% 一轮多个 tool_call 是否并发执行
    interrupt_tools := [map()],     %% 中断 tool 定义列表
    on_env_error => proceed | pause, %% 环境类工具失败策略（缺省 proceed）
    memory := provider(),           %% 记忆 provider（undefined 则不持久/不变换）
    conversation_id := binary(),
    meta := map(),                  %% 回调元数据（on_llm_call 等）
    tool_calling_manager := beamai_tool_calling_manager:manager(),
    %% 流式 token 处理器：设置时每轮 LLM 调用走 invoke_chat_stream，
    %% 文本 token 经此回调实时透出（undefined 则非流式）。
    %% Fun/2 额外收 Info（目前只有 message_id），见 token_cb/2
    stream_token_handler => undefined | fun((binary()) -> ok)
                          | fun((binary(), map()) -> ok),
    %% 之前已执行的 tool 调用记录（resume 续接时携带）
    prev_tool_calls => [map()],
    %% 首次进入循环时的一次性分派（resume 用）：返回 {result, TurnResult} 直接
    %% 短路（如重跑后仍失败要再暂停），或 {loop, OptsOverride} 用覆盖后的 opts
    %% 跑续接循环。只消费一次——turn filter 递归重入时走全新循环。
    continuation => fun(() -> {result, term()} | {loop, map()})
}.

%% step 链的请求/响应（见模块头层次图）
-type step_request() :: #{
    messages := [map()],                %% 本轮起始的完整消息序列
    context := beamai_context:t(),      %% 贯穿全链的共享上下文
    iteration := non_neg_integer(),     %% 已用迭代数（跨中断累计）
    tool_calls_made := [map()]          %% 至此已发生的 tool 调用记录
}.
-type step_response() :: #{
    status := continue | final | interrupt | error,
    messages => [map()],
    context := beamai_context:t(),
    tool_calls_made => [map()],
    response => map(),                  %% status=final
    type => atom(),                     %% status=interrupt
    interrupt_context => map(),         %% status=interrupt
    reason => term()                    %% status=error
}.

-export_type([loop_opts/0, step_request/0, step_response/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 直接跑一轮 tool loop（不经 turn 链；供不走 agent 的调用方）
%%
%% 等价于「只有循环 filter、没有任何 step filter」：messages / new_messages /
%% load_history 全取自 Opts（不从 turn 请求里拿）。
%%
%% @param Opts 循环选项（ChatClient、完整 messages、memory、回调等）
%% @param PrevToolCalls 之前已执行的 tool 调用记录（resume 时携带）
%% @returns {ok, Response, ToolCallsMade, Iterations, Messages} |
%%          {interrupt, Type, Context} |
%%          {error, Reason}
%%
%% Messages 为本轮跑完的**完整消息序列**（含载入的跨轮历史、本轮新增、各轮
%% assistant 回合与工具结果，直至最终答案）。turn filter 要重入时全靠它：
%% 拿到它再配 `load_history => false` 续跑，才能不依赖记忆是否开启就重建出
%% 完整上下文（见 beamai_filters:validate_loop 与 design/spring_advisor_alignment.md §2）。
-spec run(loop_opts(), [map()]) ->
    {ok, map(), [map()], pos_integer(), [map()]} |
    {interrupt, atom(), map()} |
    {error, term()}.
run(Opts0, PrevToolCalls) ->
    %% continuation 返回空 override：首次进入即按 Opts 原样跑（不从 Req 取消息）
    Opts = Opts0#{prev_tool_calls => PrevToolCalls,
                  continuation => fun() -> {loop, #{}} end},
    Around = beamai_filter:hook(loop_filter(Opts), around_turn),
    Req = #{messages => maps:get(new_messages, Opts, []),
            context => ctx(Opts),
            load_history => maps:get(load_history, Opts, false)},
    Around(Req, #{}, step_terminal(Opts)).

%% @doc 循环驱动 filter（around_turn；注册在 turn 链**最内层**）
%%
%% 它的 `Next` 是 step 链——调一次 = 跑一轮迭代。驱动只做三件事：判限额、
%% 按 step 响应的 status 决定继续/收尾、把最终状态折成工具循环结果 tuple。
%%
%% 首次进入时若 Opts 带 continuation（resume），一次性消费之：要么直接返回它给的
%% 结果，要么用它给的 override 跑续接循环；之后（turn filter 递归重入）一律按
%% turn 请求跑全新循环。CAS 保证「延续」只跑一次。
-spec loop_filter(loop_opts()) -> beamai_filter:filter().
loop_filter(Opts) ->
    Consumed = atomics:new(1, [{signed, false}]),  %% 0=未消费
    beamai_filter:new(<<"tool_loop">>, #{
        around_turn => fun(Req, _FCtx, Next) ->
            case consume(Opts, Consumed) of
                {result, TurnResult} -> TurnResult;
                {loop, Override} -> drive(maps:merge(Opts, Override), Req, Next);
                fresh -> drive(fresh_opts(Opts, Req), Req, Next)
            end
        end
    }).

%% @doc step 链最内层：真正跑一轮迭代（LLM 调用 + 本轮工具执行）
-spec step_terminal(loop_opts()) -> fun((step_request()) -> step_response()).
step_terminal(Opts) ->
    fun(StepReq) -> step(Opts, StepReq) end.

%%====================================================================
%% 内部函数 - 循环驱动
%%====================================================================

%% @private 一次性消费 continuation（无则 fresh）
consume(#{continuation := Fun}, Consumed) when is_function(Fun, 0) ->
    case atomics:compare_exchange(Consumed, 1, 0, 1) of
        ok -> Fun();
        _ -> fresh
    end;
consume(_Opts, _Consumed) ->
    fresh.

%% @private 全新循环的 opts：消息来源全取自 turn 请求（turn filter 可改写）
fresh_opts(Opts, Req) ->
    Opts#{messages => [],
          new_messages => maps:get(messages, Req, []),
          load_history => maps:get(load_history, Req, true),
          prev_tool_calls => []}.

%% @private 驱动循环：组装起始消息 → 逐轮调 step 链
drive(Opts, Req, Next) ->
    #{max_iterations := MaxIter} = Opts,
    StepReq = #{messages => init_messages(Opts),
                context => maps:get(context, Req, beamai_context:new()),
                iteration => 0,
                tool_calls_made => maps:get(prev_tool_calls, Opts, [])},
    iterate(Opts, Next, MaxIter, StepReq).

%% @private 组装本轮起始 messages（记忆编排统一入口）
%%
%% 先按 load_history 载入跨轮历史（必须在持久化新消息**之前**，否则新消息
%% 会经历史重复带回），再持久化新增消息（cross-run append）并拼接：
%% 首轮 [历史 ++ 新消息]，resume [中断时 messages ++ 人类输入]。
init_messages(Opts) ->
    New = maps:get(new_messages, Opts, []),
    Prior = case maps:get(load_history, Opts, false) of
        true -> load_history(Opts);
        false -> []
    end,
    persist(Opts, New),
    Existing = maps:get(messages, Opts, []),
    Prior ++ Existing ++ New.

%% @private 载入跨轮历史（无 memory 则 []）
load_history(#{memory := undefined}) -> [];
load_history(#{memory := Provider, conversation_id := ConvId}) ->
    beamai_memory_provider:history(Provider, ConvId).

%% @private 迭代次数耗尽，返回错误
iterate(_Opts, _Next, 0, #{tool_calls_made := ToolCallsMade}) ->
    {error, {max_tool_iterations, ToolCallsMade}};

%% @private 主循环体：调一次 step 链（= 一轮迭代），按 status 决定继续/收尾
%%
%% 限额在这里判、而不是放进 around_tool filter：filter 拿到的 context 是**每轮
%% 初的只读快照**（见 beamai_agent_utils:execute_sequential/4 与
%% design/context_split_parallel_tools.md §4.1「快照 + 屏障折叠，不引入批内穿线」），
%% 批内每个工具读到的计数都一样，filter 里的计数器天生累加不起来。
%% 循环这一层是串行的，length(ToolCallsMade) 无歧义——限额就该待在这。
iterate(Opts, Next, N, #{tool_calls_made := ToolCallsMade} = StepReq0) ->
    case over_tool_call_limit(Opts, ToolCallsMade) of
        true ->
            {error, {max_tool_calls, ToolCallsMade}};
        false ->
            %% iteration 为**已用**迭代数（跨中断累计）：中断上下文按它还原剩余额度
            Used = maps:get(max_tool_iterations, Opts) - N,
            dispatch_step(Opts, Next, N, Next(StepReq0#{iteration => Used}))
    end.

%% @private 按 step 响应的 status 分派
dispatch_step(_Opts, _Next, _N, #{status := final, response := Response,
                                  messages := Messages,
                                  tool_calls_made := ToolCallsMade}) ->
    {ok, Response, ToolCallsMade, compute_iterations(ToolCallsMade), Messages};
dispatch_step(Opts, Next, N, #{status := continue} = Resp) ->
    iterate(Opts, Next, N - 1, next_step_req(Resp));
dispatch_step(_Opts, _Next, _N, #{status := interrupt, type := Type,
                                  interrupt_context := Context}) ->
    {interrupt, Type, Context};
dispatch_step(_Opts, _Next, _N, #{status := error, reason := Reason}) ->
    {error, Reason};
%% step filter 合成了不认识的响应：当错误报出去，别把循环挂死
dispatch_step(_Opts, _Next, _N, Other) ->
    {error, {invalid_step_response, Other}}.

%% @private 由 step 响应组装下一轮请求（只取契约字段，filter 加的键不穿线）
next_step_req(#{messages := Messages, context := Ctx,
                tool_calls_made := ToolCallsMade}) ->
    #{messages => Messages, context => Ctx, iteration => 0,
      tool_calls_made => ToolCallsMade}.

%% @private 已发生的工具调用数是否已达上限
%%
%% 在开新一轮之前判：本批已经执行完的调用不回滚（也无从回滚——副作用已经发生），
%% 所以实际执行数可能略微越过上限，就像 max_tool_iterations 一样是"不再往下走"
%% 而非"事后撤销"。
over_tool_call_limit(Opts, ToolCallsMade) ->
    case maps:get(max_tool_calls, Opts, infinity) of
        infinity -> false;
        Max when is_integer(Max) -> length(ToolCallsMade) >= Max
    end.

%%====================================================================
%% 内部函数 - 单步（step 链最内层）
%%====================================================================

%% @private 一轮迭代：LLM 调用 → assistant 入库 → 有工具则执行本轮工具
%%
%% context 从请求里来、从响应里回：chat 返回的 context（filter 私有状态、state 槽）
%% 与工具批次折叠后的 context 都经响应穿线给下一轮。
step(Opts, #{messages := Messages, context := Ctx,
             tool_calls_made := ToolCallsMade} = StepReq) ->
    #{callbacks := Callbacks, meta := Meta} = Opts,
    ToSend = prepare_messages(Opts, Messages),
    beamai_agent_callbacks:invoke(on_llm_call, [ToSend, Meta], Callbacks),
    %% 消息 id 在调用**之前**分配：流式 token 要能标出自己属于哪条 assistant 消息
    MsgId = beamai_id:gen_id(<<"msg">>),
    emit_message_start(Opts, MsgId),
    case invoke_llm(Opts, ToSend, Ctx, MsgId) of
        {ok, Response, ChatCtx} ->
            %% 每次 LLM 返回后触发（含中间轮，可据此累计各次 usage）
            beamai_agent_callbacks:invoke(on_llm_result, [Response, Meta], Callbacks),
            Messages1 = record_assistant(Opts, Response, Messages, MsgId),
            StepReq1 = StepReq#{messages => Messages1, context => ChatCtx},
            case beamai_chat_response:has_tool_calls(Response) of
                true ->
                    handle_tool_calls(beamai_chat_response:tool_calls(Response),
                                      Opts, StepReq1);
                false ->
                    #{status => final, response => Response, messages => Messages1,
                      context => ChatCtx, tool_calls_made => ToolCallsMade}
            end;
        {error, Reason} ->
            %% 出错也要闭合这条消息（Message=undefined），start/end 恒成对
            emit_message_end(Opts, MsgId, undefined),
            #{status => error, reason => Reason, context => Ctx,
              messages => Messages, tool_calls_made => ToolCallsMade}
    end.

%% @private 经 memory provider 变换待发送消息（无 provider 则原样）
prepare_messages(#{memory := undefined}, Messages) ->
    Messages;
prepare_messages(#{memory := Provider, conversation_id := ConvId}, Messages) ->
    beamai_memory_provider:prepare(Provider, ConvId, Messages).

%% @private 把 assistant 回合并入 messages 并持久化（无可存内容则原样返回）
%%
%% 这里是「一条 assistant 消息到此为止」的**唯一**权威点——直返合成的回合也走
%% 这条路径，故 on_message_end 对它同样成立。
record_assistant(Opts, Response, Messages, MsgId) ->
    case beamai_message:from_response(Response) of
        undefined ->
            emit_message_end(Opts, MsgId, undefined),
            Messages;
        Msg ->
            persist(Opts, [Msg]),
            emit_message_end(Opts, MsgId, Msg),
            Messages ++ [Msg]
    end.

%% @private 消息边界：一条 assistant 消息开始（id 已分配，尚未产出内容）
emit_message_start(#{callbacks := Callbacks} = Opts, MsgId) ->
    beamai_agent_callbacks:invoke(on_message_start, [MsgId, msg_meta(Opts, MsgId)],
                                  Callbacks).

%% @private 消息边界：一条 assistant 消息落定
%% Msg=undefined 表示没有消息落定（LLM 出错、或响应无可存内容）
emit_message_end(#{callbacks := Callbacks} = Opts, MsgId, Msg) ->
    beamai_agent_callbacks:invoke(on_message_end, [Msg, msg_meta(Opts, MsgId)],
                                  Callbacks).

%% @private 消息级元数据：turn 级 Meta + 本条消息的 id
msg_meta(Opts, MsgId) ->
    (maps:get(meta, Opts, #{}))#{message_id => MsgId}.

%% @private 调用 LLM：有 stream_token_handler 则走流式，否则非流式
%% （context 每轮由 step 请求覆写进 chat_opts）
invoke_llm(#{chat_client := ChatClient, chat_opts := ChatOpts0} = Opts, ToSend, Ctx, MsgId) ->
    ChatOpts = ChatOpts0#{context => Ctx},
    case maps:get(stream_token_handler, Opts, undefined) of
        undefined ->
            beamai_chat_client:invoke_chat(ChatClient, ToSend, ChatOpts);
        Handler when is_function(Handler) ->
            TokenCb = token_cb(Handler, msg_meta(Opts, MsgId)),
            beamai_chat_client:invoke_chat_stream(
              ChatClient, ToSend, with_raw_event_sink(ChatOpts, Opts), TokenCb)
    end.

%% @private 桥接 ChatClient 的 (Token, Meta) 回调到 stream_token_handler
%%
%% Handler 按 arity 兼容：Fun/1 只收 Token（旧签名）；Fun/2 额外收 Info——本轮
%% Meta 加上 message_id。message_id 是把 token 归到哪条 assistant 消息的唯一
%% 依据（一轮工具循环里 assistant 文本分成多条消息，靠 token 流本身猜不出边界）；
%% 带上整份 Meta 则让 on_token 与其余回调看到同样的 run_id/turn_count。
token_cb(Handler, Info) when is_function(Handler, 2) ->
    fun(Token, _Meta) -> Handler(Token, Info) end;
token_cb(Handler, _Info) ->
    fun(Token, _Meta) -> Handler(Token) end.

%% @private 注册了 on_llm_event 才把 raw 事件汇挂上去
%%
%% 未注册时不加这个键：ChatClient 侧据此退化成空操作，流式路径一分开销不多。
%% 只在流式分支调用——非流式压根没有流事件。
with_raw_event_sink(ChatOpts, #{callbacks := Callbacks} = Opts) ->
    case maps:is_key(on_llm_event, Callbacks) of
        false ->
            ChatOpts;
        true ->
            Meta = maps:get(meta, Opts, #{}),
            ChatOpts#{on_raw_event =>
                fun(Event) ->
                    beamai_agent_callbacks:invoke(on_llm_event, [Event, Meta], Callbacks)
                end}
    end.

%%====================================================================
%% 内部函数 - Tool Calls 处理
%%====================================================================

%% @private 处理 LLM 返回的 tool_calls：统一前置中断检测，命中则统一处理
handle_tool_calls(TCs, Opts, StepReq) ->
    case find_first_interrupt(TCs, Opts) of
        {interrupt, Type, Reason, InterruptedTC, SafeCalls, SkippedCalls} ->
            handle_interrupt(Type, Reason, InterruptedTC, SafeCalls, SkippedCalls,
                             Opts, StepReq);
        no ->
            execute_and_continue(TCs, Opts, StepReq)
    end.

%% @private 统一中断检测：先查中断 tool（LLM 显式请求人类介入），
%% 再查 on_tool_call 回调（宿主侧策略拦截）；返回首个命中。
%% find_interrupt_tool 匹配 #{interrupt_tools := _}，Opts 自身即满足。
%%
%% 返回 {interrupt, Type, Reason, InterruptedTC, SafeCalls, SkippedCalls}：
%%   SafeCalls    —— 同批可安全执行的 tools；
%%   SkippedCalls —— 同样被拦截但非首个的 tools（不执行，合成 skipped 结果）。
find_first_interrupt(TCs, Opts) ->
    case beamai_agent_interrupt:find_interrupt_tool(TCs, Opts) of
        {yes, InterruptTC, OtherCalls} ->
            {interrupt, tool_request, extract_interrupt_reason(InterruptTC),
             InterruptTC, OtherCalls, []};
        no ->
            case classify_tool_calls(TCs, Opts) of
                {interrupt, Reason, [Flagged | MoreFlagged], SafeCalls} ->
                    {interrupt, callback, Reason, Flagged, SafeCalls, MoreFlagged};
                ok ->
                    no
            end
    end.

%% @private 统一中断处理（两类中断同语义）
%%
%% 先执行同批安全 tools（结果并入 messages 并持久化），被拦截未执行的其余
%% tools 合成 skipped 结果，再构建中断上下文返回。这保证 resume 后消息历史
%% 中 assistant 的每个 tool_call 都有对应结果（被中断的那个由人类输入补全），
%% 不会出现 provider 拒绝的残缺历史。中断上下文携带当前完整 messages，供
%% resume 续接。
handle_interrupt(Type, Reason, InterruptedTC, SafeCalls, SkippedCalls, Opts,
                 #{messages := Messages, context := Ctx, iteration := Used,
                   tool_calls_made := ToolCallsMade}) ->
    #{chat_client := ChatClient, tool_calling_manager := TCM} = Opts,
    Parallel = maps:get(parallel_tools, Opts, true),
    #{messages := SafeResults, records := SafeCallRecords, context := NewCtx} =
        beamai_tool_calling_manager:execute_tool_calls(TCM, ChatClient, SafeCalls, #{
            context => Ctx,
            parallel => Parallel,
            on_result => tool_result_cb(Opts)
        }),
    ok = emit_state_change(Opts, Ctx, NewCtx),
    AllResults = SafeResults ++ [skipped_result(TC) || TC <- SkippedCalls],
    persist(Opts, AllResults),
    Context = build_interrupt_context(Used, AllResults, InterruptedTC,
                                      ToolCallsMade ++ SafeCallRecords, Reason,
                                      Messages ++ AllResults,
                                      beamai_context:get_state(NewCtx)),
    #{status => interrupt, type => Type, interrupt_context => Context,
      context => NewCtx, messages => Messages ++ AllResults,
      tool_calls_made => ToolCallsMade ++ SafeCallRecords}.

%% @private 被拦截未执行的 tool_call 的占位结果（保证消息历史完整）
skipped_result(TC) ->
    {Id, _Name, _Args} = beamai_tool:parse_tool_call(TC),
    #{role => tool, tool_call_id => Id,
      content => beamai_tool:encode_result(
          #{error => #{type => skipped,
                       message => <<"This tool call was skipped because the agent was "
                                    "interrupted before execution. It was not run; "
                                    "re-issue it after the interrupt is resolved if "
                                    "still needed.">>}})}.

%% @private 执行 tools，把结果并入 messages 并持久化，继续循环
%%
%% 屏障处（工具批次执行完、结果尚未交给下一轮 LLM 之前）做环境类失败分层路由：
%% 若批内含 environment 类失败且策略 pause → 带一致快照暂停等人（phase=env_retry），
%% 批次结果尚未持久化/交模型，携带在中断上下文的 batch_messages 里，resume 时按
%% 决策重跑失败调用或原样放行（见 beamai_agent:resume）。其余情形正常续跑
%% （语义/瞬态/策略类结果照旧 errors-are-data 交模型）。
execute_and_continue(TCs, Opts, #{messages := Messages, context := Ctx,
                                  iteration := Used,
                                  tool_calls_made := ToolCallsMade}) ->
    #{chat_client := ChatClient, tool_calling_manager := TCM} = Opts,
    Parallel = maps:get(parallel_tools, Opts, true),
    #{messages := ToolResults, records := NewToolCalls, context := NewCtx} =
        beamai_tool_calling_manager:execute_tool_calls(TCM, ChatClient, TCs, #{
            context => Ctx,
            parallel => Parallel,
            on_result => tool_result_cb(Opts)
        }),
    ok = emit_state_change(Opts, Ctx, NewCtx),
    case env_pause(Opts, TCs, NewToolCalls) of
        {pause, FailedCalls} ->
            Context = build_env_interrupt_context(
                        Used, Messages, ToolResults, NewToolCalls, FailedCalls,
                        beamai_context:get_state(NewCtx), ToolCallsMade),
            #{status => interrupt, type => env_retry, interrupt_context => Context,
              context => NewCtx, messages => Messages,
              tool_calls_made => ToolCallsMade ++ NewToolCalls};
        proceed ->
            persist(Opts, ToolResults),
            %% 穿线折叠后的 context：本轮工具写下的 state 槽下一轮工具/ filter 可见
            Messages1 = Messages ++ ToolResults,
            AllCalls = ToolCallsMade ++ NewToolCalls,
            case return_direct(Opts, TCs, NewToolCalls) of
                true -> finish_direct(Opts, Messages1, ToolResults, NewCtx, AllCalls);
                false -> #{status => continue, messages => Messages1,
                           context => NewCtx, tool_calls_made => AllCalls}
            end
    end.

%% @private 整批是否直返（对标 Spring AI ToolExecutionResult.returnDirect）
%%
%% **AND 语义**：批内 tool_calls 全部标注 return_direct 才直返。混批时任一未标注
%% 即照常回灌——否则未标注工具的结果会被静默丢弃、模型再没机会用上。
%%
%% **与 Spring 的一处有意分歧**：批内任一工具**失败**则不直返，退回正常回灌，
%% 让模型看到错误后自行补救。Spring 不区分成败、一律直返，会把错误 JSON 当最终
%% 答案端给用户——那与 errors-are-data（错误回模型、模型决定怎么办）相悖。
return_direct(_Opts, [], _Records) -> false;
return_direct(#{chat_client := ChatClient}, TCs, Records) ->
    Registry = beamai_chat_client:tools(ChatClient),
    lists:all(fun(TC) ->
        {_Id, Name, _Args} = beamai_tool:parse_tool_call(TC),
        beamai_tool_registry:return_direct(Registry, Name)
    end, TCs)
        andalso not lists:any(fun is_failed/1, Records).

%% @private CallRecord 是否失败（失败时才带 error 键）
is_failed(#{error := _}) -> true;
is_failed(_) -> false.

%% @private 直返：工具结果合成最终答案，落库后结束循环（不再回灌模型）
%%
%% 合成的 assistant 回合照常持久化：历史因此仍以 assistant 收尾（形如
%% assistant(tool_calls) → tool(result) → assistant(答案)），下一轮续接不残缺。
finish_direct(Opts, Messages, ToolResults, Ctx, ToolCallsMade) ->
    Response = direct_response(ToolResults),
    %% 合成的回合同样是一条 assistant 消息：边界照发（无流式 token，start 与 end
    %% 紧挨着），否则只听边界回调的宿主会整条漏掉直返的答案
    MsgId = beamai_id:gen_id(<<"msg">>),
    emit_message_start(Opts, MsgId),
    Messages1 = record_assistant(Opts, Response, Messages, MsgId),
    #{status => final, response => Response, messages => Messages1,
      context => Ctx, tool_calls_made => ToolCallsMade}.

%% @private 由工具结果合成最终响应（多工具按原始序换行拼接）
direct_response(ToolResults) ->
    Content = iolist_to_binary(
        lists:join(<<"\n">>, [C || #{content := C} <- ToolResults])),
    beamai_chat_response:new(#{
        content => Content,
        finish_reason => complete,
        metadata => #{return_direct => true}
    }).

%% @private 环境类失败暂停判定：策略 pause 且批内有 environment 类失败 →
%% {pause, FailedTCs}（FailedTCs 为环境失败的原始 tool_call，resume retry 用）；
%% 否则 proceed。策略缺省 proceed。
env_pause(Opts, TCs, Records) ->
    case maps:get(on_env_error, Opts, proceed) of
        pause ->
            Failed = [TC || {TC, R} <- lists:zip(TCs, Records),
                            env_failed(R)],
            case Failed of
                [] -> proceed;
                _ -> {pause, Failed}
            end;
        _ ->
            proceed
    end.

%% @private CallRecord 是否为环境类失败
env_failed(#{error := #{class := environment}}) -> true;
env_failed(_) -> false.

%% @private 把消息持久化到 memory provider（无 provider 或空列表则 no-op）
persist(#{memory := undefined}, _Msgs) -> ok;
persist(_Opts, []) -> ok;
persist(#{memory := Provider, conversation_id := ConvId}, Msgs) ->
    beamai_memory_provider:append(Provider, ConvId, Msgs).

%% @private state 槽在屏障处折叠完 writes 后**确实变了**才通知宿主
%%
%% 不变就不发：绝大多数工具不写 state，每批都发等于给宿主刷噪音。比较的是折叠
%% 前后的整份 state（纯数据，比较代价与它自身大小同阶）。
emit_state_change(#{callbacks := Callbacks} = Opts, OldCtx, NewCtx) ->
    Old = beamai_context:get_state(OldCtx),
    case beamai_context:get_state(NewCtx) of
        Old ->
            ok;
        New ->
            beamai_agent_callbacks:invoke(on_state_change, [New, maps:get(meta, Opts, #{})],
                                          Callbacks)
    end.

%% @private 构建实时结果回调：每个工具完成即触发 on_tool_result（进度实时性优先，
%% 并发时触发顺序不确定；需确定顺序读 CallRecords）。经 callbacks:invoke 吞异常。
%%
%% CallRecord 里的 tool_call_id / args / error 随 Info 一并透出（注册 arity-3
%% 回调才收得到）：并发批次下触发顺序不定，tool_call_id 是把结果配回具体调用
%% 的唯一依据。
%% @doc 由 agent 状态构建实时工具结果回调（循环之外执行工具时用）
%%
%% resume 有两处在循环**外面**执行工具：审批通过的那次调用、环境失败的重跑。
%% 那里没有 loop opts，但一样该触发 on_tool_result —— 否则宿主会看到一次
%% 有始无终的工具调用（前端事件流尤其明显：TOOL_CALL_START 发了，
%% TOOL_CALL_RESULT 永远不来）。
%%
%% 注意这里只补**结果**回调，不补 on_tool_call 那个策略门：被中断的调用刚被
%% 人批过，再过一次门只会又拦下来，死循环。
-spec agent_result_cb(map()) -> fun((map()) -> ok).
agent_result_cb(Agent) ->
    tool_result_cb(#{callbacks => maps:get(callbacks, Agent, #{}),
                     meta => beamai_agent_callbacks:build_metadata(Agent)}).

tool_result_cb(#{callbacks := Callbacks} = Opts) ->
    Meta = maps:get(meta, Opts, #{}),
    fun(#{name := Name, result := Result} = CallRecord) ->
        Info = maps:merge(Meta, maps:with([tool_call_id, args, error], CallRecord)),
        beamai_agent_callbacks:invoke(on_tool_result, [Name, Result], Info, Callbacks)
    end.

%% @private 取起始 context（仅 run/2 直跑路径用；经 turn 链时由请求给出）
ctx(#{chat_opts := ChatOpts}) ->
    maps:get(context, ChatOpts, beamai_context:new()).

%% @private 计算迭代次数
compute_iterations([]) -> 1;
compute_iterations(ToolCallsMade) -> length(ToolCallsMade) + 1.

%%====================================================================
%% 内部函数 - 中断上下文构建
%%====================================================================

%% @private 构建中断上下文 map（携带当前完整 messages 与 state 快照供 resume 续接）
%%
%% State 为中断前累积的 state 槽（纯数据），resume 时恢复进 context，避免累积
%% 状态在跨越中断时静默丢失。
build_interrupt_context(Iteration, CompletedResults, InterruptedTC,
                        ToolCallsMade, Reason, Messages, State) ->
    #{
        completed_tool_results => CompletedResults,
        interrupted_tool_call => InterruptedTC,
        iteration => Iteration,
        tool_calls_made => ToolCallsMade,
        reason => Reason,
        messages => Messages,
        state => State
    }.

%% @private 构建环境类暂停（phase=env_retry）的中断上下文
%%
%% 与审批中断不同：批次**已执行完**（一致快照），结果在 batch_messages 里、
%% 尚未持久化/交模型；messages 为**批前**消息（含触发本批的 assistant tool_calls）；
%% failed_calls 为环境类失败的原始 tool_call（resume retry 重跑并按 id 替换结果）。
build_env_interrupt_context(Iteration, Messages, BatchMessages, Records, FailedCalls,
                            State, ToolCallsMade) ->
    #{
        phase => env_retry,
        reason => env_error,
        %% iteration 存"已用计数"（= MaxIter - 剩余），与审批路径一致：resume 以
        %% MaxIter - iteration 还原剩余迭代
        iteration => Iteration,
        tool_calls_made => ToolCallsMade ++ Records,
        interrupted_tool_call => undefined,
        completed_tool_results => BatchMessages,
        messages => Messages,
        state => State,
        batch_messages => BatchMessages,
        failed_calls => FailedCalls
    }.

%%====================================================================
%% 内部函数 - Callback 中断检查
%%====================================================================

%% @private 按 on_tool_call callback 对同批 tool_calls 分类
%%
%% 对每个 tool_call 都执行回调（既是通知也是策略门），收集全部被拦截
%% 的调用：首个作为中断点，其余作为 skipped；未拦截的为安全可执行。
%% 返回 {interrupt, Reason, FlaggedCalls, SafeCalls} | ok。
classify_tool_calls(ToolCalls, #{callbacks := Callbacks} = Opts) ->
    case maps:get(on_tool_call, Callbacks, undefined) of
        undefined ->
            ok;
        Fun ->
            {Flagged, Safe} = partition_by_callback(ToolCalls, Fun,
                                                    maps:get(meta, Opts, #{})),
            case Flagged of
                [] -> ok;
                [{Reason, _TC} | _] ->
                    {interrupt, Reason, [TC || {_R, TC} <- Flagged], Safe}
            end
    end.

%% @private 按回调裁决分组：{被拦截的 [{Reason, TC}], 安全的 [TC]}（保持原顺序）
%%
%% tool_call_id 随 Info 透出（注册 arity-3 回调才收得到）：宿主据此把这次调用
%% 与随后的 on_tool_result 配对，也用于向前端发出带 id 的工具调用事件。
partition_by_callback(ToolCalls, Fun, Meta) ->
    lists:foldr(fun(TC, {FAcc, SAcc}) ->
        {Id, Name, Args} = beamai_tool:parse_tool_call(TC),
        Info = Meta#{tool_call_id => Id},
        case beamai_agent_callbacks:tool_gate(Fun, Name, Args, Info) of
            {interrupt, Reason} -> {[{Reason, TC} | FAcc], SAcc};
            ok -> {FAcc, [TC | SAcc]}
        end
    end, {[], []}, ToolCalls).

%%====================================================================
%% 内部函数 - 辅助
%%====================================================================

%% @private 从 interrupt tool_call 中提取中断原因
%% 支持 OpenAI 嵌套格式与统一响应的扁平格式（与 get_tool_call_name 同理）
extract_interrupt_reason(#{function := #{arguments := Args}}) when is_map(Args) ->
    Args;
extract_interrupt_reason(#{<<"function">> := #{<<"arguments">> := Args}}) when is_map(Args) ->
    Args;
extract_interrupt_reason(#{arguments := Args}) when is_map(Args) ->
    Args;
extract_interrupt_reason(TC) ->
    {_Id, Name, Args} = beamai_tool:parse_tool_call(TC),
    #{tool => Name, arguments => Args}.
