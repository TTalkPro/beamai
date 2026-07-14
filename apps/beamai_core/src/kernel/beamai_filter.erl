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
%%% - turn：Request `#{messages, context, resume}` → Response = 工具循环结果 tuple
%%%   （`{ok, Response, ToolCallsMade, Iterations}` | `{interrupt, Type, Ctx}` |
%%%   `{error, Reason}`）；turn filter 直接模式匹配该 tuple。
%%%   **硬规则**：`{interrupt,_,_}` / `{error,_}` 必须透传、不得重入 Next
%%%   （暂停/错误态上重试破坏 HITL 语义）。
%%%
%%% 某条链只会用到该链对应的 around（chat 链用 around_chat，tool 链用
%%% around_tool，turn 链用 around_turn），不含相关 hook 的 filter 在该链中被跳过。
%%%
%%% **注册顺序即层序**：filters 列表靠前 = 外层（前置先执行、后置后执行）。
%%% 无 order 字段、无运行时排序——层次完全由构建 kernel 时给出的列表位置决定
%%% （对齐 clj-agent advisor.clj 的扁平 vector 模型）。
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

-type hook_type() :: around_chat | around_tool | around_turn.
-type request() :: map().
-type response() :: map() | tuple().  %% chat/tool 为 map；turn 为工具循环结果 tuple
-type fctx() :: map().
-type next() :: fun((request()) -> response()).
-type around_fun() :: fun((request(), fctx(), next()) -> response() | {response(), fctx()}).
-type hooks() :: #{
    around_chat => around_fun(),
    around_tool => around_fun(),
    around_turn => around_fun()
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
%% @param Hooks hook map，可含 around_chat/around_tool/around_turn 任意子集
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
-spec hook(filter(), hook_type()) -> around_fun() | undefined.
hook(#{hooks := Hooks}, HookType) ->
    maps:get(HookType, Hooks, undefined).

%% @doc 取 filter 的私有上下文初值
-spec init(filter()) -> fctx().
init(#{init := Init}) -> Init.
