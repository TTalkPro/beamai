%%%-------------------------------------------------------------------
%%% @doc Graph 执行版本的简单 Agent
%%%
%%% 使用 Graph 节点构建的 Agent 实现，提供两种使用模式：
%%%
%%% == 纯函数模式（推荐） ==
%%%
%%% 适用于单次对话、高并发场景、无状态服务。
%%% 主要 API: run_once/2, create_state/1, run_with_state/3
%%%
%%% == 进程模式（高级） ==
%%%
%%% 适用于长期运行的服务、需要监督树、自动状态管理。
%%% 主要 API: start_link/2, run/2, save_checkpoint/1
%%%
%%% == 模块拆分说明 ==
%%%
%%% 本模块为 API 门面，实际逻辑由以下子模块实现：
%%% - beamai_agent_init: 初始化逻辑
%%% - beamai_agent_runner: 图执行逻辑
%%% - beamai_agent_callbacks: 回调管理
%%% - beamai_agent_checkpoint: 检查点持久化（仅进程模式）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent).

-behaviour(gen_server).

-include("beamai_agent.hrl").

%% API (兼容 beamai_agent)
-export([start_link/2, stop/1, run/2, run/3]).
-export([add_tool/2, remove_tool/2, set_system_prompt/2]).

%% 纯函数 API（无需启动 gen_server）
-export([
    run_once/2,           %% 单次执行
    run_with_state/3,     %% 带状态执行
    create_state/1,       %% 创建状态
    create_state/2,
    export_state/1,       %% 导出状态（用于持久化）
    import_state/2        %% 导入状态
]).

%% Scratchpad API
-export([get_scratchpad/1, clear_scratchpad/1]).

%% Messages API（对话历史）
-export([get_messages/1, clear_messages/1]).

%% Context API（用户自定义上下文）
-export([get_context/1, get_context/2, get_context/3]).
-export([set_context/2, update_context/2, put_context/3]).

%% Checkpoint API
-export([save_checkpoint/1, save_checkpoint/2]).
-export([load_checkpoint/2, load_latest_checkpoint/1]).
-export([list_checkpoints/1, list_checkpoints/2]).
-export([restore_from_checkpoint/2]).

%% 中断控制 API
-export([resume/2, abort/1]).

%% 回调 API
-export([set_callbacks/2, get_callbacks/1]).
-export([emit_custom_event/3, emit_custom_event/4]).

%% Middleware API
-export([get_middlewares/1, add_middleware/2, remove_middleware/2]).
-export([set_middlewares/2]).

%% 图构建 API
-export([build_graph/1]).

%% 协调器 API
-export([start_pipeline/2, start_orchestrator/2]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动 Graph Agent
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(Id, Opts) ->
    gen_server:start_link(?MODULE, {Id, Opts}, []).

%% @doc 停止 Agent
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc 运行对话
-spec run(pid(), binary()) -> {ok, map()} | {error, term()}.
run(Pid, Msg) ->
    run(Pid, Msg, #{}).

%% @doc 运行对话（带选项）
-spec run(pid(), binary(), map()) -> {ok, map()} | {error, term()}.
run(Pid, Msg, Opts) ->
    Timeout = maps:get(timeout, Opts, 300000),
    gen_server:call(Pid, {run, Msg, Opts}, Timeout).

%% @doc 向 Agent 添加工具
-spec add_tool(pid(), map()) -> ok | {error, term()}.
add_tool(Pid, Tool) ->
    gen_server:call(Pid, {add_tool, Tool}).

%% @doc 从 Agent 移除工具
-spec remove_tool(pid(), binary()) -> ok | {error, term()}.
remove_tool(Pid, Name) ->
    gen_server:call(Pid, {remove_tool, Name}).

%% @doc 设置系统提示词
-spec set_system_prompt(pid(), binary()) -> ok | {error, term()}.
set_system_prompt(Pid, Prompt) ->
    gen_server:call(Pid, {set_prompt, Prompt}).

%% @doc 获取 Scratchpad 中的所有步骤
-spec get_scratchpad(pid()) -> [map()].
get_scratchpad(Pid) ->
    gen_server:call(Pid, get_scratchpad).

%% @doc 清空 Scratchpad
-spec clear_scratchpad(pid()) -> ok.
clear_scratchpad(Pid) ->
    gen_server:call(Pid, clear_scratchpad).

%% @doc 获取完整对话历史
-spec get_messages(pid()) -> [map()].
get_messages(Pid) ->
    gen_server:call(Pid, get_messages).

%% @doc 清空对话历史（开始新对话）
-spec clear_messages(pid()) -> ok.
clear_messages(Pid) ->
    gen_server:call(Pid, clear_messages).

%%====================================================================
%% Context API（用户自定义上下文）
%%====================================================================

%% @doc 获取完整的用户上下文
%%
%% 返回 context map，包含所有用户自定义数据。
%% Context 会在 checkpoint 中自动持久化。
%%
%% 示例：
%% ```
%% Context = beamai_agent:get_context(Pid),
%% %% => #{user_id => <<"u001">>, credits => 100}
%% ```
-spec get_context(pid()) -> map().
get_context(Pid) ->
    gen_server:call(Pid, get_context).

%% @doc 获取上下文中的单个值
%%
%% 示例：
%% ```
%% UserId = beamai_agent:get_context(Pid, user_id),
%% %% => <<"u001">> 或 undefined
%% ```
-spec get_context(pid(), atom() | binary()) -> term() | undefined.
get_context(Pid, Key) ->
    get_context(Pid, Key, undefined).

%% @doc 获取上下文中的单个值（带默认值）
%%
%% 示例：
%% ```
%% Credits = beamai_agent:get_context(Pid, credits, 0),
%% %% => 100 或 0（如果不存在）
%% ```
-spec get_context(pid(), atom() | binary(), term()) -> term().
get_context(Pid, Key, Default) ->
    Context = gen_server:call(Pid, get_context),
    maps:get(Key, Context, Default).

%% @doc 设置整个上下文（替换）
%%
%% 警告：会完全替换现有 context。
%% 如需合并更新，请使用 update_context/2。
%%
%% 示例：
%% ```
%% ok = beamai_agent:set_context(Pid, #{user_id => <<"u002">>, credits => 50}).
%% ```
-spec set_context(pid(), map()) -> ok.
set_context(Pid, Context) when is_map(Context) ->
    gen_server:call(Pid, {set_context, Context}).

%% @doc 更新上下文（合并）
%%
%% 将 Updates 合并到现有 context 中，保留未更新的键。
%%
%% 示例：
%% ```
%% %% 假设 context = #{user_id => <<"u001">>, credits => 100}
%% ok = beamai_agent:update_context(Pid, #{credits => 90, last_action => buy}),
%% %% context 变为 #{user_id => <<"u001">>, credits => 90, last_action => buy}
%% ```
-spec update_context(pid(), map()) -> ok.
update_context(Pid, Updates) when is_map(Updates) ->
    gen_server:call(Pid, {update_context, Updates}).

%% @doc 设置上下文中的单个值
%%
%% 示例：
%% ```
%% ok = beamai_agent:put_context(Pid, credits, 200).
%% ```
-spec put_context(pid(), atom() | binary(), term()) -> ok.
put_context(Pid, Key, Value) ->
    gen_server:call(Pid, {put_context, Key, Value}).

%%====================================================================
%% Checkpoint API
%%====================================================================

%% @doc 保存当前状态为检查点
-spec save_checkpoint(pid()) -> {ok, binary()} | {error, term()}.
save_checkpoint(Pid) ->
    save_checkpoint(Pid, #{}).

%% @doc 保存检查点（带元数据）
-spec save_checkpoint(pid(), map()) -> {ok, binary()} | {error, term()}.
save_checkpoint(Pid, Meta) ->
    gen_server:call(Pid, {save_checkpoint, Meta}).

%% @doc 加载指定检查点
-spec load_checkpoint(pid(), binary()) -> {ok, map()} | {error, term()}.
load_checkpoint(Pid, CheckpointId) ->
    gen_server:call(Pid, {load_checkpoint, CheckpointId}).

%% @doc 加载最新检查点
-spec load_latest_checkpoint(pid()) -> {ok, map()} | {error, term()}.
load_latest_checkpoint(Pid) ->
    gen_server:call(Pid, load_latest_checkpoint).

%% @doc 列出所有检查点
-spec list_checkpoints(pid()) -> {ok, [map()]} | {error, term()}.
list_checkpoints(Pid) ->
    list_checkpoints(Pid, #{}).

%% @doc 列出检查点（带过滤选项）
-spec list_checkpoints(pid(), map()) -> {ok, [map()]} | {error, term()}.
list_checkpoints(Pid, Opts) ->
    gen_server:call(Pid, {list_checkpoints, Opts}).

%% @doc 从检查点恢复状态
-spec restore_from_checkpoint(pid(), binary()) -> ok | {error, term()}.
restore_from_checkpoint(Pid, CheckpointId) ->
    gen_server:call(Pid, {restore_from_checkpoint, CheckpointId}).

%% @doc 恢复被中断的执行
-spec resume(pid(), confirm | reject | {modify, term()}) -> ok | {error, term()}.
resume(Pid, Action) ->
    gen_server:call(Pid, {resume, Action}, 60000).

%% @doc 中止当前执行
-spec abort(pid()) -> ok.
abort(Pid) ->
    gen_server:call(Pid, abort).

%%====================================================================
%% Callback API
%%====================================================================

%% @doc 设置回调处理器
-spec set_callbacks(pid(), map()) -> ok.
set_callbacks(Pid, CallbackOpts) ->
    gen_server:call(Pid, {set_callbacks, CallbackOpts}).

%% @doc 获取当前回调配置
-spec get_callbacks(pid()) -> map().
get_callbacks(Pid) ->
    gen_server:call(Pid, get_callbacks).

%% @doc 触发自定义事件
-spec emit_custom_event(pid(), atom() | binary(), term()) -> ok.
emit_custom_event(Pid, EventName, Data) ->
    emit_custom_event(Pid, EventName, Data, #{}).

%% @doc 触发自定义事件（带元数据）
-spec emit_custom_event(pid(), atom() | binary(), term(), map()) -> ok.
emit_custom_event(Pid, EventName, Data, Metadata) ->
    gen_server:cast(Pid, {emit_custom_event, EventName, Data, Metadata}).

%%====================================================================
%% Middleware API
%%====================================================================

%% @doc 获取当前 Middleware 配置
-spec get_middlewares(pid()) -> [term()].
get_middlewares(Pid) ->
    gen_server:call(Pid, get_middlewares).

%% @doc 添加 Middleware
%%
%% 添加新的 Middleware 到链末尾，会重新构建图。
%% MiddlewareSpec 格式: {Module, Opts} | {Module, Opts, Priority} | Module
-spec add_middleware(pid(), term()) -> ok | {error, term()}.
add_middleware(Pid, MiddlewareSpec) ->
    gen_server:call(Pid, {add_middleware, MiddlewareSpec}).

%% @doc 移除 Middleware
%%
%% 按模块名移除 Middleware，会重新构建图。
-spec remove_middleware(pid(), module()) -> ok | {error, term()}.
remove_middleware(Pid, Module) ->
    gen_server:call(Pid, {remove_middleware, Module}).

%% @doc 设置完整的 Middleware 配置
%%
%% 替换所有现有 Middleware，会重新构建图。
-spec set_middlewares(pid(), [term()]) -> ok | {error, term()}.
set_middlewares(Pid, Middlewares) ->
    gen_server:call(Pid, {set_middlewares, Middlewares}).

%%====================================================================
%% 图构建 API
%%====================================================================

%% @doc 构建 Agent 执行图
-spec build_graph(map()) -> {ok, map()} | {error, term()}.
build_graph(Opts) ->
    beamai_agent_runner:build_graph(Opts).

%%====================================================================
%% 纯函数 API
%%====================================================================

%% @doc 单次执行 Agent（无状态，推荐用于单次对话）
%%
%% 使用场景：
%% - HTTP API 请求处理
%% - 批处理任务
%% - 无需保持上下文的单次对话
-spec run_once(map(), binary()) -> {ok, map()} | {error, term()}.
run_once(Config, Message) ->
    case create_state(Config) of
        {ok, State} ->
            case run_with_state(State, Message, #{}) of
                {ok, Result, _NewState} -> {ok, Result};
                {error, Reason, _NewState} -> {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc 带状态执行 Agent（用于多轮对话）
%%
%% 使用场景：
%% - 需要保持上下文的多轮对话
%% - 显式管理状态流转
%% - 状态序列化与恢复
%%
%% 示例：
-spec run_with_state(#state{}, binary(), map()) ->
    {ok, map(), #state{}} | {error, term(), #state{}}.
run_with_state(State, Message, Opts) ->
    RunId = beamai_agent_callbacks:generate_run_id(),
    State1 = State#state{run_id = RunId},
    Metadata = beamai_agent_callbacks:build_metadata(State1),

    %% 触发 chain_start 回调
    beamai_agent_callbacks:invoke(on_chain_start, [Message, Metadata], State1#state.callbacks),

    case beamai_agent_runner:execute(Message, Opts, State1) of
        {ok, Result, NewState} ->
            beamai_agent_callbacks:invoke(on_chain_end, [Result, Metadata], NewState#state.callbacks),
            {ok, Result, NewState#state{run_id = undefined}};
        {error, Reason, NewState} ->
            beamai_agent_callbacks:invoke(on_chain_error, [Reason, Metadata], NewState#state.callbacks),
            {error, Reason, NewState#state{run_id = undefined}}
    end.

%% @doc 创建 Agent 状态（纯函数模式）
%%
%% 创建不带存储后端的状态，用于纯函数模式。
%% 自动生成 Agent ID。
%%
%% 示例：
-spec create_state(map()) -> {ok, #state{}} | {error, term()}.
create_state(Config) ->
    Id = beamai_utils:gen_id(),
    create_state(Id, Config).

%% @doc 创建 Agent 状态（指定 ID）
%%
%% 创建带指定 ID 的状态，用于纯函数模式。
%% 注意：纯函数模式下 storage 和 auto_checkpoint 被强制禁用。
-spec create_state(binary(), map()) -> {ok, #state{}} | {error, term()}.
create_state(Id, Config) ->
    %% 强制禁用存储（纯函数模式）
    PureConfig = Config#{
        enable_storage => false,
        auto_checkpoint => false
    },
    beamai_agent_init:create_state(Id, PureConfig).

%% @doc 导出状态（用于外部持久化）
%%
%% 将对话状态导出为可序列化的 map，便于用户自行存储到
%% Redis、PostgreSQL、文件等后端。
%%
%% 导出内容与 checkpoint 保持一致：
%% - messages: 压缩后的消息（用于 LLM 调用）
%% - full_messages: 完整对话历史（用于审计、调试、回溯）
%% - scratchpad: 中间步骤记录
%% - context: 用户自定义上下文数据
%%
%% 不导出配置数据（应在 import_state 时通过 Config 传入）：
%% - tools, llm_config, middlewares, system_prompt 等
%%
%% 示例：
%% ```
%% %% 导出状态
%% Exported = beamai_agent:export_state(State),
%% %% 存储到外部系统
%% redis:set(<<"agent:123">>, jsx:encode(Exported)),
%% %% 恢复时传入配置
%% {ok, NewState} = beamai_agent:import_state(Exported, Config).
%% ```
-spec export_state(pid() | #state{}) -> map().
export_state(Pid) when is_pid(Pid) ->
    gen_server:call(Pid, export_state);
export_state(#state{} = State) ->
    #{
        messages => State#state.messages,
        full_messages => State#state.full_messages,
        scratchpad => State#state.scratchpad,
        context => State#state.context
    }.

%% @doc 从导出数据恢复状态
%%
%% 从 export_state/1 导出的数据恢复对话状态。
%% 配置（tools, llm, middlewares 等）通过 Config 参数传入。
%%
%% 参数：
%% - ExportedData: export_state/1 导出的 map（包含 messages, full_messages, scratchpad, context）
%% - Config: Agent 配置（tools, llm, system_prompt, middlewares 等）
%%
%% 示例：
%% ```
%% %% 从外部系统加载导出数据
%% {ok, Json} = redis:get(<<"agent:123">>),
%% Exported = jsx:decode(Json, [return_maps]),
%% %% 使用配置恢复状态
%% Config = #{
%%     llm => #{provider => openai, model => <<"gpt-4">>},
%%     tools => MyTools,
%%     system_prompt => <<"You are a helpful assistant.">>
%% },
%% {ok, State} = beamai_agent:import_state(Exported, Config).
%% ```
-spec import_state(pid(), map()) -> ok;
                  (map(), map()) -> {ok, #state{}} | {error, term()}.
import_state(Pid, ExportedData) when is_pid(Pid) ->
    %% gen_server 版本：将状态应用到运行中的 Agent
    gen_server:call(Pid, {import_state, ExportedData});
import_state(ExportedData, Config) when is_map(ExportedData), is_map(Config) ->
    %% 纯函数版本：从导出数据创建新状态
    Messages = maps:get(messages, ExportedData, []),
    FullMessages = maps:get(full_messages, ExportedData, []),
    Scratchpad = maps:get(scratchpad, ExportedData, []),
    Context = maps:get(context, ExportedData, #{}),

    %% 强制禁用存储（纯函数模式）
    PureConfig = Config#{
        enable_storage => false,
        auto_checkpoint => false
    },

    %% 创建新状态
    case create_state(PureConfig) of
        {ok, State} ->
            %% 恢复对话状态（与 checkpoint 恢复逻辑一致）
            CurrentCtx = State#state.context,
            MergedCtx = maps:merge(CurrentCtx, Context),
            {ok, State#state{
                messages = Messages,
                full_messages = FullMessages,
                scratchpad = Scratchpad,
                context = MergedCtx
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% 协调器 API
%%====================================================================

%% @doc 启动 Pipeline 模式协调器
%%
%% 创建一个流水线 Agent，任务在 workers 间顺序传递。
%% 适合场景：内容生产流程、代码审查流程、数据处理流程。
%%
%% 示例: 研究员 → 写作者 → 审核员
-spec start_pipeline(binary(), map()) -> {ok, pid()} | {error, term()}.
start_pipeline(Id, Opts) ->
    beamai_coordinator:start_pipeline(Id, Opts).

%% @doc 启动 Orchestrator 模式协调器
%%
%% 创建一个编排器 Agent，可以委托、路由、并行调用多个 workers。
%% 适合场景：复杂任务分解、多角度分析、专家咨询。
%%
%% 示例:
-spec start_orchestrator(binary(), map()) -> {ok, pid()} | {error, term()}.
start_orchestrator(Id, Opts) ->
    beamai_coordinator:start_orchestrator(Id, Opts).

%%====================================================================
%% gen_server 回调
%%====================================================================

%% @doc 初始化
init({Id, Opts}) ->
    case beamai_agent_init:create_state(Id, Opts) of
        {ok, State} -> {ok, State};
        {error, Reason} -> {stop, Reason}
    end.

%% @doc 处理同步调用
handle_call({run, Msg, Opts}, _From, State) ->
    handle_run(Msg, Opts, State);

handle_call({add_tool, Tool}, _From, #state{tools = Tools, tool_handlers = H} = State) ->
    handle_add_tool(Tool, Tools, H, State);

handle_call({remove_tool, Name}, _From, #state{tools = Tools, tool_handlers = H} = State) ->
    handle_remove_tool(Name, Tools, H, State);

handle_call({set_prompt, Prompt}, _From, State) ->
    handle_set_prompt(Prompt, State);

handle_call(get_scratchpad, _From, #state{scratchpad = Pad} = State) ->
    {reply, lists:reverse(Pad), State};

handle_call(clear_scratchpad, _From, State) ->
    {reply, ok, State#state{scratchpad = []}};

handle_call(get_messages, _From, #state{messages = Msgs} = State) ->
    {reply, Msgs, State};

handle_call(clear_messages, _From, State) ->
    {reply, ok, State#state{messages = []}};

%% Context API
handle_call(get_context, _From, #state{context = Context} = State) ->
    {reply, Context, State};

handle_call({set_context, NewContext}, _From, State) ->
    {reply, ok, State#state{context = NewContext}};

handle_call({update_context, Updates}, _From, #state{context = Context} = State) ->
    NewContext = maps:merge(Context, Updates),
    {reply, ok, State#state{context = NewContext}};

handle_call({put_context, Key, Value}, _From, #state{context = Context} = State) ->
    NewContext = maps:put(Key, Value, Context),
    {reply, ok, State#state{context = NewContext}};

handle_call({resume, Action}, _From, #state{pending_action = Pending} = State)
  when Pending =/= undefined ->
    handle_resume(Action, Pending, State);

handle_call({resume, _Action}, _From, State) ->
    {reply, {error, no_pending_action}, State};

handle_call(abort, _From, State) ->
    {reply, ok, State#state{pending_action = undefined}};

handle_call({save_checkpoint, Meta}, _From, State) ->
    {reply, beamai_agent_checkpoint:save(Meta, State), State};

handle_call({load_checkpoint, CpId}, _From, State) ->
    {reply, beamai_agent_checkpoint:load(CpId, State), State};

handle_call(load_latest_checkpoint, _From, State) ->
    {reply, beamai_agent_checkpoint:load_latest(State), State};

handle_call({list_checkpoints, Opts}, _From, State) ->
    {reply, beamai_agent_checkpoint:list(Opts, State), State};

handle_call({restore_from_checkpoint, CpId}, _From, State) ->
    case beamai_agent_checkpoint:restore(CpId, State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
    end;

handle_call({set_callbacks, CallbackOpts}, _From, #state{callbacks = Callbacks} = State) ->
    NewCallbacks = beamai_agent_callbacks:update(Callbacks, CallbackOpts),
    {reply, ok, State#state{callbacks = NewCallbacks}};

handle_call(get_callbacks, _From, #state{callbacks = Callbacks} = State) ->
    {reply, beamai_agent_callbacks:to_map(Callbacks), State};

%% Middleware API
handle_call(get_middlewares, _From, #state{middlewares = Middlewares} = State) ->
    {reply, Middlewares, State};

handle_call({add_middleware, MiddlewareSpec}, _From, #state{middlewares = Middlewares} = State) ->
    NewMiddlewares = Middlewares ++ [MiddlewareSpec],
    handle_update_middlewares(NewMiddlewares, State);

handle_call({remove_middleware, Module}, _From, #state{middlewares = Middlewares} = State) ->
    NewMiddlewares = lists:filter(fun(Spec) ->
        extract_middleware_module(Spec) =/= Module
    end, Middlewares),
    handle_update_middlewares(NewMiddlewares, State);

handle_call({set_middlewares, NewMiddlewares}, _From, State) ->
    handle_update_middlewares(NewMiddlewares, State);

%% State Import/Export API
handle_call(export_state, _From, State) ->
    {reply, export_state(State), State};

handle_call({import_state, ExportedData}, _From, #state{context = CurrentCtx} = State) ->
    Messages = maps:get(messages, ExportedData, []),
    FullMessages = maps:get(full_messages, ExportedData, []),
    Scratchpad = maps:get(scratchpad, ExportedData, []),
    Context = maps:get(context, ExportedData, #{}),
    MergedCtx = maps:merge(CurrentCtx, Context),
    NewState = State#state{
        messages = Messages,
        full_messages = FullMessages,
        scratchpad = Scratchpad,
        context = MergedCtx
    },
    {reply, ok, NewState};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc 处理异步消息
handle_cast({emit_custom_event, EventName, Data, Metadata}, State) ->
    FullMetadata = maps:merge(beamai_agent_callbacks:build_metadata(State), Metadata),
    beamai_agent_callbacks:invoke(on_custom_event, [EventName, Data, FullMetadata],
                                   State#state.callbacks),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

%% @doc 处理其他消息
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc 终止回调
terminate(_Reason, _State) ->
    ok.

%%====================================================================
%% 内部函数 - handle_call 处理器
%%====================================================================

%% @private 处理运行请求
-spec handle_run(binary(), map(), #state{}) ->
    {reply, {ok, map()} | {error, term()}, #state{}}.
handle_run(Msg, Opts, State) ->
    RunId = beamai_agent_callbacks:generate_run_id(),
    State1 = State#state{run_id = RunId},
    Metadata = beamai_agent_callbacks:build_metadata(State1),

    %% 触发 chain_start 回调
    beamai_agent_callbacks:invoke(on_chain_start, [Msg, Metadata], State1#state.callbacks),

    case beamai_agent_runner:execute(Msg, Opts, State1) of
        {ok, Result, NewState} ->
            beamai_agent_callbacks:invoke(on_chain_end, [Result, Metadata], NewState#state.callbacks),
            FinalState = beamai_agent_checkpoint:maybe_auto_save(Result, NewState),
            {reply, {ok, Result}, FinalState#state{run_id = undefined}};
        {error, Reason, NewState} ->
            beamai_agent_callbacks:invoke(on_chain_error, [Reason, Metadata], NewState#state.callbacks),
            {reply, {error, Reason}, NewState#state{run_id = undefined}}
    end.

%% @private 处理添加工具
-spec handle_add_tool(map(), [map()], map(), #state{}) ->
    {reply, ok | {error, term()}, #state{}}.
handle_add_tool(Tool, Tools, H, State) ->
    NewTools = [Tool | Tools],
    NewHandlers = maps:merge(H, beamai_nodes:build_tool_handlers([Tool])),
    case beamai_agent_runner:rebuild_graph(State#state{tools = NewTools, tool_handlers = NewHandlers}) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
    end.

%% @private 处理移除工具
-spec handle_remove_tool(binary(), [map()], map(), #state{}) ->
    {reply, ok | {error, term()}, #state{}}.
handle_remove_tool(Name, Tools, H, State) ->
    NewTools = lists:filter(fun(#{name := N}) -> N =/= Name end, Tools),
    NewHandlers = maps:remove(Name, H),
    case beamai_agent_runner:rebuild_graph(State#state{tools = NewTools, tool_handlers = NewHandlers}) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
    end.

%% @private 处理设置提示词
-spec handle_set_prompt(binary(), #state{}) -> {reply, ok | {error, term()}, #state{}}.
handle_set_prompt(Prompt, State) ->
    case beamai_agent_runner:rebuild_graph(State#state{system_prompt = Prompt}) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
    end.

%% @private 处理恢复动作
-spec handle_resume(term(), map(), #state{}) -> {reply, ok | {error, term()}, #state{}}.
handle_resume(confirm, _Pending, State) ->
    {reply, ok, State#state{pending_action = undefined}};
handle_resume(reject, _Pending, State) ->
    {reply, ok, State#state{pending_action = undefined}};
handle_resume({modify, _NewData}, _Pending, State) ->
    {reply, ok, State#state{pending_action = undefined}};
handle_resume(_, _, State) ->
    {reply, {error, invalid_action}, State}.

%% @private 更新 Middleware 配置并重建图
-spec handle_update_middlewares([term()], #state{}) ->
    {reply, ok | {error, term()}, #state{}}.
handle_update_middlewares(NewMiddlewares, #state{tools = Tools, system_prompt = Prompt,
                                                  llm_config = LLMConfig, max_iterations = MaxIter,
                                                  response_format = RF} = State) ->
    %% 初始化新的 Middleware 链
    NewChain = case NewMiddlewares of
        [] -> undefined;
        _ -> beamai_middleware_runner:init(NewMiddlewares)
    end,

    %% 重建图
    Opts = #{
        tools => Tools,
        system_prompt => Prompt,
        llm => LLMConfig,
        max_iterations => MaxIter,
        response_format => RF,
        middlewares => NewMiddlewares
    },

    case beamai_agent_runner:build_graph(Opts) of
        {ok, NewGraph} ->
            NewState = State#state{
                middlewares = NewMiddlewares,
                middleware_chain = NewChain,
                graph = NewGraph
            },
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

%% @private 从 Middleware 规范提取模块名
-spec extract_middleware_module(term()) -> module().
extract_middleware_module({Module, _Opts, _Priority}) -> Module;
extract_middleware_module({Module, _Opts}) -> Module;
extract_middleware_module(Module) when is_atom(Module) -> Module.
