%%%-------------------------------------------------------------------
%%% @doc HTTP 客户端行为定义
%%%
%%% 定义 HTTP 客户端的标准接口。内置实现为 beamai_http_gun；
%%% 该抽象层也供测试替换后端使用（如 beamai_llm_fake_backend）。
%%%
%%% == 回调函数 ==
%%%
%%% 必须实现：
%%% - request/5: 发送 HTTP 请求
%%% - stream_request/6: 发送流式请求
%%% - ensure_started/0: 确保客户端已启动
%%%
%%% == 实现示例 ==
%%%
%%% ```erlang
%%% -module(my_http_client).
%%% -behaviour(beamai_http_behaviour).
%%% -export([request/5, stream_request/6, ensure_started/0]).
%%%
%%% request(Method, Url, Headers, Body, Opts) ->
%%%     %% 实现 HTTP 请求
%%%     ...
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_http_behaviour).

%%====================================================================
%% 类型定义
%%====================================================================

-type method() :: get | post | put | delete | head | options | patch.
-type url() :: binary() | string().
-type headers() :: [{binary(), binary()}].
-type body() :: binary() | iodata().
-type opts() :: #{
    timeout => pos_integer(),
    connect_timeout => pos_integer(),
    headers => headers(),
    pool => atom(),
    init_acc => term(),
    forward_headers => boolean()
}.

-type response() :: {ok, term()} | {error, term()}.
-type meta() :: #{status => non_neg_integer() | undefined, headers => headers()}.
-type response_meta() :: {ok, term(), meta()} | {error, term()}.
-type chunk_handler() :: fun((binary(), term()) -> {continue, term()} | {done, term()}).
-type stream_response() :: {ok, term()} | {error, term()}.

-export_type([method/0, url/0, headers/0, body/0, opts/0]).
-export_type([response/0, meta/0, response_meta/0, chunk_handler/0, stream_response/0]).

%%====================================================================
%% 行为回调定义
%%====================================================================

%% @doc 发送 HTTP 请求
%%
%% @param Method HTTP 方法
%% @param Url 请求 URL
%% @param Headers 请求头
%% @param Body 请求体
%% @param Opts 选项
%% @returns {ok, Response} | {error, Reason}
-callback request(Method :: method(),
                  Url :: url(),
                  Headers :: headers(),
                  Body :: body(),
                  Opts :: opts()) -> response().

%% @doc 发送流式请求
%%
%% @param Method HTTP 方法
%% @param Url 请求 URL
%% @param Headers 请求头
%% @param Body 请求体
%% @param Opts 选项
%% @param Handler 数据块处理函数
%% @returns {ok, Result} | {error, Reason}
-callback stream_request(Method :: method(),
                         Url :: url(),
                         Headers :: headers(),
                         Body :: body(),
                         Opts :: opts(),
                         Handler :: chunk_handler()) -> stream_response().

%% @doc 确保 HTTP 客户端已启动
%%
%% @returns ok
-callback ensure_started() -> ok.

%%====================================================================
%% 可选回调（带默认实现）
%%====================================================================

%% @doc 关闭连接（可选）
-callback close(Ref :: term()) -> ok.

%% @doc 发送 HTTP 请求并返回响应元信息（状态码 + 响应头）（可选）
%%
%% 与 request/5 相同，但额外返回 #{status => 状态码, headers => 响应头}，
%% 供上层提取速率限制等响应头信息。未实现该回调的后端，
%% beamai_http:request_meta/5 会回退到 request/5 并返回空 headers。
%%
%% @returns {ok, Response, Meta} | {error, Reason}
-callback request_meta(Method :: method(),
                       Url :: url(),
                       Headers :: headers(),
                       Body :: body(),
                       Opts :: opts()) -> response_meta().

-optional_callbacks([close/1, request_meta/5]).
