%%%-------------------------------------------------------------------
%%% @doc 洋葱式 Filter（around 模型）
%%%
%%% 一个 filter 是一层洋葱，用单个 `around` 闭包同时承担「前置 → 调内层 →
%%% 后置」三段逻辑，最多绑定 3 个可选 hook（一个机制三个粒度）：
%%% - `around_chat`：包裹一次 LLM 调用（chat 链，循环内每轮一次）
%%% - `around_tool`：包裹一次工具执行（tool 链，每个 tool call 一次、并行任务内）
%%% - `around_turn`：包裹整个工具循环（turn 链，每 turn 一次）——RAG 注入 /
%%%   最终答案 guardrail / turn 级预算 / evaluator 递归重入
%%%
%%% around 形态（middleware 模式，取代旧的 before/after 双闭包）：
%%%   `fun(Request, FCtx, Next) -> Response | {Response, NewFCtx}`
%%% - 前置：改写 Request
%%% - 调内层：`Next(Request1)` 拿到 Response（不调即短路，多调即重试）
%%% - 后置：改写 Response
%%% - 返回 Response（私有状态不变）或 {Response, NewFCtx}（更新私有状态）
%%%
%%% FCtx 是该 filter 的**私有上下文**（按 filter 名字隔离，贯穿一次 invoke，
%%% 含工具循环各轮），与请求里贯穿全链的共享 context 分离，由洋葱链负责
%%% 投影/合并，filter 代码只通过 FCtx 参数读、通过返回值写。
%%%
%%% Request / Response：
%%% - chat：Request `#{messages, context, opts}` → Response `#{response, context}`
%%% - tool：Request `#{tool, args, context}`     → Response `#{result, context}`
%%% - turn：Request `#{messages, context, resume, load_history}` → Response = 工具循环
%%%   结果 tuple（`{ok, Response, ToolCallsMade, Iterations, Messages}` |
%%%   `{interrupt, Type, Ctx}` | `{error, Reason}`）；turn filter 直接模式匹配该 tuple。
%%%   **硬规则**：`{interrupt,_,_}` / `{error,_}` 必须透传、不得重入 Next
%%%   （暂停/错误态上重试破坏 HITL 语义）。
%%%
%%%   **重入要用第 5 元 `Messages`**（该跑完整消息序列：跨轮历史 + 本轮新增 +
%%%   各轮 assistant/工具结果，直至最终答案）。要接着上一跑续走时，传
%%%   `messages => Messages ++ 追加的`、`load_history => false`：上下文全由 filter
%%%   重建，不依赖 agent 是否开了记忆。
%%%   Request 的 `messages` 语义是**本轮新增消息**（非完整历史），`load_history`
%%%   缺省 true（让 loop 前接跨轮历史）；只传新增消息而指望 loop 载入历史的写法，
%%%   在 `memory => false` 时会丢掉原始问题——那正是 load_history 存在的理由。
%%%
%%% 某条链只会用到该链对应的 around（chat 链用 around_chat，tool 链用
%%% around_tool，turn 链用 around_turn），不含相关 hook 的 filter 在该链中被跳过。
%%%
%%% **注册顺序即层序**：filters 列表靠前 = 外层（前置先执行、后置后执行）。
%%% 无 order 字段、无运行时排序——层次完全由构建 kernel 时给出的列表位置决定
%%% （对齐 clj-agent advisor.clj 的扁平 vector 模型）。
%%%
%%% 第四钩子 `token_transform`（token 流变换，对照 clj-agent :token-xf）：
%%% 不走三链洋葱，由流式 terminal（beamai_kernel invoke_chat_stream）按注册顺序
%%% 组装成 token 变换链，作用于送往 on-token sink 的**出站流**。Erlang 无
%%% transducer，契约为等价的 step/flush map（见 token_transform/0）：
%%% - `step(TokenData, State) -> {Emit :: [TokenData], NewState}`——1→N（吞掉/
%%%   改写/缓冲后批量放行）；
%%% - `flush(State) -> [TokenData]`（可选）——流正常结束时冲出缓冲残留；
%%% - `init`——状态初值，每次 LLM 流现场初始化（工具循环每轮各自新状态）。
%%% 硬边界：token 链只改"交付"（sink 看到什么），不改"答案"——流末归一化
%%% 响应不经过它（memory 落库/turn 结果用的都是原始完整答案）。
%%% 同步路径（invoke_chat）完全忽略 token_transform。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_filter).

%% 构造器
-export([new/2, new/3]).
%% 工具
-export([hook/2, init/1]).

%% Types
-export_type([filter/0, hooks/0, hook_type/0, request/0, response/0, fctx/0, next/0]).
-export_type([token_data/0, token_step/0, token_flush/0, token_transform/0]).

-type hook_type() :: around_chat | around_tool | around_turn | token_transform.
-type request() :: map().
-type response() :: map() | tuple().  %% chat/tool 为 map；turn 为工具循环结果 tuple
-type fctx() :: map().
-type next() :: fun((request()) -> response()).
-type around_fun() :: fun((request(), fctx(), next()) -> response() | {response(), fctx()}).

%% token 流变换（第四钩子，流式专用；等价于 clj transducer 的 step/completion）
-type token_data() :: #{token := binary(), meta := map()}.
-type token_step() :: fun((token_data(), State :: term()) ->
    {[token_data()], NewState :: term()}).
-type token_flush() :: fun((State :: term()) -> [token_data()]).
-type token_transform() :: #{
    init => term(),          %% 状态初值（缺省 undefined）
    step := token_step(),    %% 1→N：一个 token 进、0/1/N 个出
    flush => token_flush()   %% 流正常结束时冲出缓冲残留（缺省无残留）
}.

-type hooks() :: #{
    around_chat => around_fun(),
    around_tool => around_fun(),
    around_turn => around_fun(),
    token_transform => token_transform()
}.

-type filter() :: #{
    '__filter__' := true,
    name := binary(),
    hooks := hooks(),
    init := fctx()
}.

%%====================================================================
%% 构造器
%%====================================================================

%% @doc 创建 filter（私有状态初值 #{}）
%%
%% @param Name 名称（调试标识，也是私有上下文的隔离键）
%% @param Hooks hook map，可含 around_chat/around_tool/around_turn/token_transform 任意子集
-spec new(binary(), hooks()) -> filter().
new(Name, Hooks) ->
    new(Name, Hooks, #{}).

%% @doc 创建 filter（指定私有状态初值）
%%
%% @param Init filter 私有上下文初值，首次进入时种入（缺省 #{}）
-spec new(binary(), hooks(), fctx()) -> filter().
new(Name, Hooks, Init) when is_map(Hooks), is_map(Init) ->
    #{'__filter__' => true, name => Name, hooks => Hooks, init => Init}.

%%====================================================================
%% 工具
%%====================================================================

%% @doc 取 filter 的某个 hook（不存在返回 undefined）
-spec hook(filter(), hook_type()) -> around_fun() | token_transform() | undefined.
hook(#{hooks := Hooks}, HookType) ->
    maps:get(HookType, Hooks, undefined).

%% @doc 取 filter 的私有上下文初值
-spec init(filter()) -> fctx().
init(#{init := Init}) -> Init.
