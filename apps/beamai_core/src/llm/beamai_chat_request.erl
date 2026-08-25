%%%-------------------------------------------------------------------
%%% @doc ChatRequest：一次模型调用的输入（对标 Spring AI 的 `Prompt`）
%%%
%%% 两部分，边界很清楚：
%%% - `messages` —— 本次要发给模型的消息序列
%%% - `options`  —— **本次调用**的模型参数（model / temperature / max_tokens /
%%%   tools / tool_choice / stream / response_format …）
%%%
%%% 与 provider **Config** 的分工（这是本模块存在的主要理由）：
%%% - Config 是**连接与凭证**：api_key / base_url / timeout / 该 provider 的默认模型参数，
%%%   一次创建、长期复用（`beamai_chat_model:create/2`）
%%% - options 是**这一次调用**的参数，覆盖 Config 上的同名默认值
%%%
%%% 与 filter 链上的 chat Request（`#{messages, context, opts}`）也不是一回事：
%%% 那个对应 Spring 的 `ChatClientRequest`（prompt + 调用上下文），本模块对应它里面的
%%% `Prompt`。链上的 filter 改的是前者，provider 收到的是后者。
%%%
%%% 结构是带标记的 map，可直接模式匹配；改写一律走 with_/put_ 系列以免写错键。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_chat_request).

%% 构造
-export([new/1, new/2]).
%% 读取
-export([messages/1, options/1, option/2, option/3, tools/1, is_stream/1]).
%% 改写（返回新请求）
-export([with_messages/2, with_options/2, put_option/3, merge_options/2]).

-export_type([t/0, options/0]).

-type options() :: #{
    %% 模型行为
    model => binary(),
    temperature => number(),
    max_tokens => pos_integer(),
    top_p => number(),
    stop_sequences => [term()],
    response_format => map(),
    %% 工具
    tools => [map()],
    %% tool_choice 的取值是 **provider 特定**的（auto | none | required | any |
    %% {tool, Name} | binary() | map()），故不在类型里收窄
    tool_choice => term(),
    %% 传输
    stream => boolean(),
    atom() => term()
}.

-type t() :: #{
    '__chat_request__' := true,
    messages := [map()],
    options := options()
}.

%%====================================================================
%% 构造
%%====================================================================

%% @doc 只有消息的请求（options 为空）
-spec new([map()]) -> t().
new(Messages) ->
    new(Messages, #{}).

%% @doc 消息 + 本次调用参数
-spec new([map()], options()) -> t().
new(Messages, Options) when is_list(Messages), is_map(Options) ->
    #{'__chat_request__' => true, messages => Messages, options => Options}.

%%====================================================================
%% 读取
%%====================================================================

-spec messages(t()) -> [map()].
messages(#{messages := Messages}) -> Messages.

-spec options(t()) -> options().
options(#{options := Options}) -> Options.

%% @doc 取某个调用参数（不存在返回 undefined）
-spec option(t(), atom()) -> term() | undefined.
option(Request, Key) -> option(Request, Key, undefined).

%% @doc 取某个调用参数（不存在返回 Default）
-spec option(t(), atom(), term()) -> term().
option(#{options := Options}, Key, Default) -> maps:get(Key, Options, Default).

%% @doc 本次广播给模型的工具定义（未给出为空列表）
-spec tools(t()) -> [map()].
tools(Request) -> option(Request, tools, []).

%% @doc 本次是否流式
-spec is_stream(t()) -> boolean().
is_stream(Request) -> option(Request, stream, false) =:= true.

%%====================================================================
%% 改写
%%====================================================================

%% @doc 换掉消息序列（RAG 前置 / 历史展开）
-spec with_messages(t(), [map()]) -> t().
with_messages(Request, Messages) when is_list(Messages) ->
    Request#{messages => Messages}.

%% @doc 整体换掉调用参数
-spec with_options(t(), options()) -> t().
with_options(Request, Options) when is_map(Options) ->
    Request#{options => Options}.

%% @doc 设置单个调用参数
-spec put_option(t(), atom(), term()) -> t().
put_option(#{options := Options} = Request, Key, Value) ->
    Request#{options => Options#{Key => Value}}.

%% @doc 合并调用参数（New 覆盖同名旧值）
-spec merge_options(t(), options()) -> t().
merge_options(#{options := Options} = Request, New) when is_map(New) ->
    Request#{options => maps:merge(Options, New)}.
