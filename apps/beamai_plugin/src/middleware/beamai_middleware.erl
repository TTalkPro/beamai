%%%-------------------------------------------------------------------
%%% @doc 中间件行为定义（4 个钩子）
%%%
%%% 本模块定义了中间件的行为接口，与 beamai_core 的 filter 类型对齐：
%%% - pre_chat: 在 LLM 调用之前执行
%%% - post_chat: 在 LLM 响应之后执行
%%% - pre_invocation: 在工具/函数执行之前执行
%%% - post_invocation: 在工具/函数执行之后执行
%%%
%%% == 返回值说明 ==
%%%
%%% - `ok` - 不做任何修改，继续执行后续中间件
%%% - `{continue, UpdatedFilterCtx}` - 更新过滤器上下文后继续执行
%%% - `{skip, Term}` - 跳过剩余处理流程，直接返回指定值
%%% - `{error, Reason}` - 中止执行并返回错误
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% -module(my_middleware).
%%% -behaviour(beamai_middleware).
%%% -export([init/1, pre_chat/2]).
%%%
%%% init(Opts) ->
%%%     #{max_calls => maps:get(max_calls, Opts, 10), count => 0}.
%%%
%%% pre_chat(FilterCtx, #{max_calls := Max, count := Count} = _MwState) ->
%%%     case Count >= Max of
%%%         true -> {error, call_limit_exceeded};
%%%         false -> ok
%%%     end.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_middleware).

-export_type([
    middleware/0,
    middleware_result/0,
    middleware_state/0,
    hook_name/0
]).

%%====================================================================
%% 类型定义
%%====================================================================

%% @type middleware() :: map().
%% 中间件定义结构体，包含模块名、状态和优先级。
%% - module: 中间件实现模块的原子名称
%% - state: 中间件的当前状态（由 init/1 初始化）
%% - priority: 执行优先级，数值越小优先级越高（可选，默认 100）
-type middleware() :: #{
    module := module(),
    state := middleware_state(),
    priority => integer()
}.

%% @type middleware_state() :: map().
%% 中间件状态，由各中间件模块的 init/1 回调返回，
%% 用于在多次钩子调用之间保存中间件的内部状态数据。
-type middleware_state() :: map().

%% @type hook_name() :: pre_chat | post_chat | pre_invocation | post_invocation.
%% 钩子名称类型，标识中间件可以拦截的四个执行阶段：
%% - pre_chat: LLM 调用前
%% - post_chat: LLM 响应后
%% - pre_invocation: 工具调用前
%% - post_invocation: 工具调用后
-type hook_name() :: pre_chat | post_chat | pre_invocation | post_invocation.

%% @type middleware_result() :: ok | {continue, map()} | {skip, term()} | {error, term()}.
%% 中间件钩子的返回结果类型：
%% - ok: 不修改上下文，继续执行
%% - {continue, UpdatedFilterCtx}: 更新过滤器上下文并继续
%% - {skip, Value}: 跳过后续处理，直接返回指定值
%% - {error, Reason}: 中止执行链并报错
-type middleware_result() ::
    ok
    | {continue, map()}
    | {skip, term()}
    | {error, term()}.

%%====================================================================
%% 回调函数定义
%%====================================================================

%% @doc 初始化中间件状态。
%% 在中间件链初始化时调用，根据配置选项创建中间件的初始状态。
%% @param Opts 配置选项映射，包含中间件的各项参数
%% @returns 初始化后的中间件状态映射
-callback init(Opts :: map()) -> middleware_state().

%% @doc LLM 调用前钩子。
%% 在每次发送请求给 LLM 之前被调用，可用于限制调用次数、修改请求内容等。
%% @param FilterCtx 过滤器上下文，包含当前请求的完整信息
%% @param MwState 当前中间件状态
%% @returns 中间件处理结果
-callback pre_chat(FilterCtx :: map(), MwState :: middleware_state()) ->
    middleware_result().

%% @doc LLM 响应后钩子。
%% 在收到 LLM 响应之后被调用，可用于检测错误、修改响应、触发重试等。
%% @param FilterCtx 过滤器上下文，包含 LLM 的响应结果
%% @param MwState 当前中间件状态
%% @returns 中间件处理结果
-callback post_chat(FilterCtx :: map(), MwState :: middleware_state()) ->
    middleware_result().

%% @doc 工具调用前钩子。
%% 在执行工具/函数之前被调用，可用于权限检查、参数验证、人工审批等。
%% @param FilterCtx 过滤器上下文，包含待执行的工具信息
%% @param MwState 当前中间件状态
%% @returns 中间件处理结果
-callback pre_invocation(FilterCtx :: map(), MwState :: middleware_state()) ->
    middleware_result().

%% @doc 工具调用后钩子。
%% 在工具/函数执行完成后被调用，可用于结果验证、错误处理、重试逻辑等。
%% @param FilterCtx 过滤器上下文，包含工具执行的结果
%% @param MwState 当前中间件状态
%% @returns 中间件处理结果
-callback post_invocation(FilterCtx :: map(), MwState :: middleware_state()) ->
    middleware_result().

%% 所有回调均为可选实现，中间件只需实现所关注的钩子即可
-optional_callbacks([
    init/1,
    pre_chat/2, post_chat/2,
    pre_invocation/2, post_invocation/2
]).
