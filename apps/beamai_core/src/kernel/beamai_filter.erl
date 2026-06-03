%%%-------------------------------------------------------------------
%%% @doc 洋葱式 Filter
%%%
%%% 一个 filter 是一层洋葱，最多绑定 4 个可选 hook：
%%% - `pre_chat`  / `post_chat`  ：包裹一次 LLM 调用（chat 链）
%%% - `pre_tool`  / `post_tool`  ：包裹一次工具执行（tool 链）
%%%
%%% 同一 filter 的 pre/post 配成一对，洋狀链保证 pre 在内层之前、post 在
%%% 内层之后（回程自动逆序）。一个 filter 可只实现其中若干 hook；某条链
%%% 只会用到该链对应的两个 hook（chat 链用 pre_chat/post_chat，tool 链用
%%% pre_tool/post_tool），不含相关 hook 的 filter 在该链中被跳过。
%%%
%%% hook 形态（对齐 Spring AI 的 before/after）：
%%% - pre  ：`fun(Request)  -> Request | {halt, Response}`（去程改写请求；
%%%          返回 {halt, Response} 跳过内层，但仍执行本层 post）
%%% - post ：`fun(Response) -> Response`（回程改写响应）
%%%
%%% Request / Response：
%%% - chat：Request `#{messages, context, opts}` → Response `#{response, context}`
%%% - tool：Request `#{tool, args, context}`     → Response `#{result, context}`
%%%
%%% order 越小越外层（pre 越先执行、post 越后执行）。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_filter).

%% 构造器
-export([new/2, new/3]).
%% 工具
-export([sort/1, hook/2]).

%% Types
-export_type([filter/0, hooks/0, hook_type/0, request/0, response/0]).

-type hook_type() :: pre_chat | post_chat | pre_tool | post_tool.
-type request() :: map().
-type response() :: map().
-type pre_fun() :: fun((request()) -> request() | {halt, response()}).
-type post_fun() :: fun((response()) -> response()).
-type hooks() :: #{
    pre_chat  => pre_fun(),
    post_chat => post_fun(),
    pre_tool  => pre_fun(),
    post_tool => post_fun()
}.

-type filter() :: #{
    '__filter__' := true,
    name := binary(),
    order := integer(),
    hooks := hooks()
}.

%%====================================================================
%% 构造器
%%====================================================================

%% @doc 创建 filter（默认 order 0）
%%
%% @param Name 名称（调试标识）
%% @param Hooks hook map，可含 pre_chat/post_chat/pre_tool/post_tool 任意子集
-spec new(binary(), hooks()) -> filter().
new(Name, Hooks) ->
    new(Name, Hooks, 0).

%% @doc 创建 filter（指定 order，越小越外层）
-spec new(binary(), hooks(), integer()) -> filter().
new(Name, Hooks, Order) when is_map(Hooks) ->
    #{'__filter__' => true, name => Name, order => Order, hooks => Hooks}.

%%====================================================================
%% 工具
%%====================================================================

%% @doc 按 order 升序稳定排序（同 order 保持注册顺序）
-spec sort([filter()]) -> [filter()].
sort(Filters) ->
    lists:sort(fun(#{order := O1}, #{order := O2}) -> O1 =< O2 end, Filters).

%% @doc 取 filter 的某个 hook（不存在返回 undefined）
-spec hook(filter(), hook_type()) -> pre_fun() | post_fun() | undefined.
hook(#{hooks := Hooks}, HookType) ->
    maps:get(HookType, Hooks, undefined).
