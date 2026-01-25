%%%-------------------------------------------------------------------
%%% @doc A2A Server 核心模块
%%%
%%% 实现 A2A 协议的服务端核心逻辑。作为 gen_server 行为实现，
%%% 负责管理服务器状态和协调各个子模块的工作。
%%%
%%% == 职责 ==
%%%
%%% 1. 服务器生命周期管理（启动/停止）
%%% 2. 任务进程映射管理
%%% 3. 请求分发和响应构建
%%% 4. Agent Card 缓存
%%%
%%% == 子模块 ==
%%%
%%% - beamai_a2a_handler: JSON-RPC 方法处理器
%%% - beamai_a2a_middleware: 认证和限流中间件
%%% - beamai_a2a_convert: JSON 转换工具
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 启动服务器
%%% {ok, Server} = beamai_a2a_server:start_link(#{
%%%     agent_config => AgentConfig
%%% }).
%%%
%%% %% 处理请求（无认证）
%%% {ok, Response} = beamai_a2a_server:handle_request(Server, JsonRpcRequest).
%%%
%%% %% 处理请求（带认证和限流）
%%% Headers = [{<<"authorization">>, <<"Bearer my-api-key">>}],
%%% case beamai_a2a_server:handle_json_with_auth(Server, JsonBin, Headers) of
%%%     {ok, ResponseJson, RateLimitInfo} -> respond(200, ResponseJson);
%%%     {error, auth_error, ErrorJson} -> respond(401, ErrorJson);
%%%     {error, rate_limited, ErrorJson} -> respond(429, ErrorJson)
%%% end.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_server).

-behaviour(gen_server).

%% API 导出
-export([
    start_link/1,
    start/1,
    handle_request/2,
    handle_json/2,
    get_agent_card/1,
    stop/1,

    %% 带认证和限流的请求处理
    handle_request_with_auth/3,
    handle_json_with_auth/3,

    %% 任务状态控制（供外部模块调用）
    request_input/3,
    complete_task/3,
    fail_task/3
]).

%% gen_server 回调
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("beamai_core/include/beamai_common.hrl").

%% 服务器状态记录
-record(state, {
    agent_config :: map(),               %% Agent 配置
    agent_card :: map(),                 %% 缓存的 Agent Card
    tasks :: #{binary() => pid()},       %% Task ID -> Task Pid 映射
    contexts :: #{binary() => [binary()]}  %% Context ID -> [Task IDs] 映射
}).

%%====================================================================
%% API - 服务器生命周期
%%====================================================================

%% @doc 启动 A2A Server（带链接）
%%
%% @param Opts 配置选项
%%   - agent_config: Agent 配置（必需）
%%   - name: 服务器注册名称（可选）
%% @returns {ok, Pid} | {error, Reason}
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Opts) ->
    case maps:get(name, Opts, undefined) of
        undefined ->
            gen_server:start_link(?MODULE, Opts, []);
        Name ->
            gen_server:start_link({local, Name}, ?MODULE, Opts, [])
    end.

%% @doc 启动 A2A Server（不带链接）
-spec start(map()) -> {ok, pid()} | {error, term()}.
start(Opts) ->
    gen_server:start(?MODULE, Opts, []).

%% @doc 停止服务器
-spec stop(pid() | atom()) -> ok.
stop(Server) ->
    gen_server:stop(Server, normal, ?DEFAULT_TIMEOUT).

%%====================================================================
%% API - 请求处理（无认证）
%%====================================================================

%% @doc 处理 JSON-RPC 请求
%%
%% @param Server 服务器进程引用
%% @param Request 请求 map（已解码的 JSON）
%% @returns {ok, Response} | {error, Reason}
-spec handle_request(pid() | atom(), map()) -> {ok, map()} | {error, term()}.
handle_request(Server, Request) ->
    gen_server:call(Server, {handle_request, Request}, ?DEFAULT_LLM_TIMEOUT).

%% @doc 处理 JSON 字符串请求
%%
%% @param Server 服务器进程引用
%% @param JsonBin JSON 二进制字符串
%% @returns {ok, ResponseJson} | {error, Reason}
-spec handle_json(pid() | atom(), binary()) -> {ok, binary()} | {error, term()}.
handle_json(Server, JsonBin) ->
    case beamai_a2a_jsonrpc:decode(JsonBin) of
        {ok, Request} ->
            case handle_request(Server, Request) of
                {ok, Response} ->
                    {ok, jsx:encode(Response, [])};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, parse_error} ->
            {ok, jsx:encode(beamai_a2a_jsonrpc:parse_error(null), [])};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% API - 请求处理（带认证和限流）
%%====================================================================

%% @doc 带认证和限流的 JSON-RPC 请求处理
%%
%% 处理流程：
%% 1. 验证 API Key 认证
%% 2. 检查限流
%% 3. 处理实际请求
%%
%% @param Server 服务器进程引用
%% @param Request 请求 map
%% @param Headers HTTP 请求头列表
%% @returns {ok, Response, RateLimitInfo} | {error, Type, Response}
-spec handle_request_with_auth(pid() | atom(), map(), [{binary(), binary()}]) ->
    {ok, map(), map()} | {error, atom(), map()}.
handle_request_with_auth(Server, Request, Headers) ->
    Id = maps:get(<<"id">>, Request, null),
    case beamai_a2a_middleware:check_auth_and_rate_limit(Headers) of
        {ok, _AuthInfo, RateLimitInfo} ->
            %% 认证和限流都通过
            case handle_request(Server, Request) of
                {ok, Response} ->
                    {ok, Response, RateLimitInfo};
                {error, Reason} ->
                    {error, Reason, RateLimitInfo}
            end;
        {error, auth_disabled, RateLimitInfo} ->
            %% 认证已禁用，直接处理请求
            case handle_request(Server, Request) of
                {ok, Response} ->
                    {ok, Response, RateLimitInfo};
                {error, Reason} ->
                    {error, Reason, RateLimitInfo}
            end;
        {error, missing_api_key, _} ->
            {error, auth_error, beamai_a2a_middleware:make_auth_error_response(Id, missing_api_key)};
        {error, invalid_api_key, _} ->
            {error, auth_error, beamai_a2a_middleware:make_auth_error_response(Id, invalid_api_key)};
        {error, key_expired, _} ->
            {error, auth_error, beamai_a2a_middleware:make_auth_error_response(Id, key_expired)};
        {error, insufficient_permissions, _} ->
            {error, auth_error, beamai_a2a_middleware:make_auth_error_response(Id, insufficient_permissions)};
        {error, rate_limited, RateLimitInfo} ->
            {error, rate_limited, beamai_a2a_middleware:make_rate_limit_error_response(Id, RateLimitInfo)}
    end.

%% @doc 带认证和限流的 JSON 字符串请求处理
%%
%% @param Server 服务器进程引用
%% @param JsonBin JSON 二进制字符串
%% @param Headers HTTP 请求头列表
%% @returns {ok, ResponseJson, RateLimitInfo} | {error, Type, ResponseJson}
-spec handle_json_with_auth(pid() | atom(), binary(), [{binary(), binary()}]) ->
    {ok, binary(), map()} | {error, atom(), binary()}.
handle_json_with_auth(Server, JsonBin, Headers) ->
    case beamai_a2a_jsonrpc:decode(JsonBin) of
        {ok, Request} ->
            case handle_request_with_auth(Server, Request, Headers) of
                {ok, Response, RateLimitInfo} ->
                    {ok, jsx:encode(Response, []), RateLimitInfo};
                {error, Type, Response} when is_map(Response) ->
                    {error, Type, jsx:encode(Response, [])};
                {error, Reason, RateLimitInfo} ->
                    {error, Reason, RateLimitInfo}
            end;
        {error, parse_error} ->
            %% 解析错误不需要认证检查
            {error, parse_error, jsx:encode(beamai_a2a_jsonrpc:parse_error(null), [])};
        {error, Reason} ->
            {error, Reason, #{}}
    end.

%%====================================================================
%% API - Agent Card
%%====================================================================

%% @doc 获取服务器的 Agent Card
-spec get_agent_card(pid() | atom()) -> {ok, map()} | {error, term()}.
get_agent_card(Server) ->
    gen_server:call(Server, get_agent_card, ?DEFAULT_TIMEOUT).

%%====================================================================
%% API - 任务状态控制
%%====================================================================

%% @doc 请求用户输入
%%
%% 将任务设置为 input_required 状态。
%%
%% @param TaskPid 任务进程
%% @param Question 询问用户的问题
%% @param Options 可选参数（预留）
%% @returns ok | {error, Reason}
-spec request_input(pid(), binary(), map()) -> ok | {error, term()}.
request_input(TaskPid, Question, _Options) ->
    QuestionMessage = #{
        role => agent,
        parts => [#{kind => text, text => Question}],
        message_id => beamai_a2a_convert:generate_message_id()
    },
    case beamai_a2a_task:update_status(TaskPid, {input_required, QuestionMessage}) of
        ok ->
            %% 发送 push 通知
            {ok, TaskId} = beamai_a2a_task:get_id(TaskPid),
            {ok, Task} = beamai_a2a_task:get(TaskPid),
            spawn(fun() -> beamai_a2a_push:notify_async(TaskId, Task) end),
            ok;
        Error ->
            Error
    end.

%% @doc 完成任务
%%
%% 将任务设置为 completed 状态。
%%
%% @param TaskPid 任务进程
%% @param Response 响应内容
%% @param Options 可选参数
%% @returns ok | {error, Reason}
-spec complete_task(pid(), binary() | map(), map()) -> ok | {error, term()}.
complete_task(TaskPid, Response, Options) when is_binary(Response) ->
    %% 创建 Artifact
    ArtifactName = maps:get(artifact_name, Options, <<"response">>),
    Artifact = #{
        name => ArtifactName,
        parts => [#{kind => text, text => Response}]
    },
    ok = beamai_a2a_task:add_artifact(TaskPid, Artifact),
    case beamai_a2a_task:update_status(TaskPid, completed) of
        ok ->
            notify_task_change(TaskPid),
            ok;
        Error ->
            Error
    end;
complete_task(TaskPid, ResponseMsg, _Options) when is_map(ResponseMsg) ->
    case beamai_a2a_task:update_status(TaskPid, {completed, ResponseMsg}) of
        ok ->
            notify_task_change(TaskPid),
            ok;
        Error ->
            Error
    end.

%% @doc 标记任务失败
%%
%% @param TaskPid 任务进程
%% @param Reason 错误原因
%% @param Options 可选参数
%% @returns ok | {error, Reason}
-spec fail_task(pid(), term(), map()) -> ok | {error, term()}.
fail_task(TaskPid, Reason, _Options) ->
    ErrorMessage = #{
        role => agent,
        parts => [#{kind => text, text => format_error(Reason)}]
    },
    case beamai_a2a_task:update_status(TaskPid, {failed, ErrorMessage}) of
        ok ->
            notify_task_change(TaskPid),
            ok;
        Error ->
            Error
    end.

%%====================================================================
%% gen_server 回调
%%====================================================================

%% @private 初始化服务器
init(Opts) ->
    AgentConfig = maps:get(agent_config, Opts, #{}),

    %% 生成 Agent Card
    {ok, AgentCard} = beamai_a2a_card:generate(AgentConfig),

    State = #state{
        agent_config = AgentConfig,
        agent_card = AgentCard,
        tasks = #{},
        contexts = #{}
    },

    {ok, State}.

%% @private 处理同步调用
handle_call({handle_request, Request}, _From, State) ->
    case do_handle_request(Request, State) of
        {ok, Response, NewState} ->
            {reply, {ok, Response}, NewState};
        {error, Reason, NewState} ->
            {reply, {error, Reason}, NewState}
    end;

handle_call(get_agent_card, _From, #state{agent_card = Card} = State) ->
    {reply, {ok, Card}, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private 处理异步消息
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private 处理系统消息
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
    %% Task 进程终止，清理映射
    NewTasks = maps:filter(fun(_Id, TaskPid) -> TaskPid =/= Pid end, State#state.tasks),
    {noreply, State#state{tasks = NewTasks}};

handle_info(_Info, State) ->
    {noreply, State}.

%% @private 终止回调
terminate(_Reason, _State) ->
    ok.

%% @private 代码升级
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% 内部函数 - 请求处理
%%====================================================================

%% @private 处理请求分发
do_handle_request(#{<<"method">> := Method} = Request, State) ->
    Id = maps:get(<<"id">>, Request, null),
    Params = maps:get(<<"params">>, Request, #{}),

    %% 构建处理器上下文
    Context = #{
        id => Id,
        tasks => State#state.tasks,
        contexts => State#state.contexts,
        agent_config => State#state.agent_config
    },

    %% 调用处理器
    case beamai_a2a_handler:dispatch(Method, Params, Context) of
        {ok, Response, NewContext} ->
            NewState = State#state{
                tasks = maps:get(tasks, NewContext, State#state.tasks),
                contexts = maps:get(contexts, NewContext, State#state.contexts)
            },
            {ok, Response, NewState};
        {error, Reason, NewContext} ->
            NewState = State#state{
                tasks = maps:get(tasks, NewContext, State#state.tasks),
                contexts = maps:get(contexts, NewContext, State#state.contexts)
            },
            {error, Reason, NewState}
    end;

do_handle_request(Request, State) when is_map(Request) ->
    %% 请求缺少 method 字段
    Id = maps:get(<<"id">>, Request, null),
    Response = #{
        <<"jsonrpc">> => <<"2.0">>,
        <<"id">> => Id,
        <<"error">> => #{
            <<"code">> => -32600,
            <<"message">> => <<"Invalid Request">>
        }
    },
    {ok, Response, State};

do_handle_request({batch, Requests}, State) ->
    %% 批处理请求
    {Responses, FinalState} = lists:foldl(
        fun(Req, {Acc, S}) ->
            case do_handle_request(Req, S) of
                {ok, Resp, NewS} -> {[Resp | Acc], NewS};
                {error, _, NewS} -> {Acc, NewS}
            end
        end,
        {[], State},
        Requests
    ),
    {ok, lists:reverse(Responses), FinalState}.

%%====================================================================
%% 内部函数 - 工具
%%====================================================================

%% @private 发送任务变更通知
notify_task_change(TaskPid) ->
    {ok, TaskId} = beamai_a2a_task:get_id(TaskPid),
    {ok, Task} = beamai_a2a_task:get(TaskPid),
    spawn(fun() -> beamai_a2a_push:notify_async(TaskId, Task) end),
    ok.

%% @private 格式化错误（委托给公共模块）
format_error(Reason) ->
    beamai_a2a_utils:format_error(Reason).
