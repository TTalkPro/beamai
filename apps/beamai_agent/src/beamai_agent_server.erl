%%%-------------------------------------------------------------------
%%% @doc Agent gen_server 实现模块
%%%
%%% 从 beamai_agent.erl 中分离出来的 gen_server 逻辑。
%%% 包含：
%%%
%%% - gen_server 回调函数（init, handle_call, handle_cast 等）
%%% - 分组的请求分发逻辑（减少 handle_call 分支）
%%% - 进程状态管理
%%%
%%% 本模块处理有状态的、基于进程的 Agent 操作。
%%% 对于无状态操作，请参见 beamai_agent_api.erl。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_server).

-behaviour(gen_server).

-include("beamai_agent.hrl").

%%====================================================================
%% API 导出
%%====================================================================

%% 生命周期 API
-export([start_link/2, stop/1]).

%% 执行 API
-export([run/2, run/3]).

%% 工具 API
-export([add_tool/2, remove_tool/2]).

%% 配置 API
-export([set_system_prompt/2]).

%% 状态查询 API
-export([
    get_scratchpad/1,
    clear_scratchpad/1,
    get_messages/1,
    clear_messages/1
]).

%% Context API
-export([
    get_context/1,
    set_context/2,
    update_context/2,
    put_context/3
]).

%% Meta API
-export([
    get_meta/1,
    set_meta/2,
    put_meta/3
]).

%% Checkpoint API
-export([
    save_checkpoint/1,
    save_checkpoint/2,
    load_checkpoint/2,
    load_latest_checkpoint/1,
    list_checkpoints/1,
    list_checkpoints/2,
    restore_from_checkpoint/2
]).

%% 中断控制 API
-export([resume/2, abort/1]).

%% 回调 API
-export([
    set_callbacks/2,
    get_callbacks/1,
    emit_custom_event/3,
    emit_custom_event/4
]).

%% Middleware API
-export([
    get_middlewares/1,
    add_middleware/2,
    remove_middleware/2,
    set_middlewares/2
]).

%% 状态导入/导出 API
-export([export_state/1, import_state/2]).

%% gen_server 回调
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 启动 Agent 服务器
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(Id, Opts) ->
    gen_server:start_link(?MODULE, {Id, Opts}, []).

%% @doc 停止 Agent 服务器
-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:stop(Pid).

%% @doc 执行对话
-spec run(pid(), binary()) -> {ok, map()} | {error, term()}.
run(Pid, Msg) ->
    run(Pid, Msg, #{}).

%% @doc 执行对话（带选项）
-spec run(pid(), binary(), map()) -> {ok, map()} | {error, term()}.
run(Pid, Msg, Opts) ->
    Timeout = maps:get(timeout, Opts, 300000),
    gen_server:call(Pid, {run, Msg, Opts}, Timeout).

%% @doc 添加工具
-spec add_tool(pid(), map()) -> ok | {error, term()}.
add_tool(Pid, Tool) ->
    gen_server:call(Pid, {tool, add, Tool}).

%% @doc 移除工具
-spec remove_tool(pid(), binary()) -> ok | {error, term()}.
remove_tool(Pid, Name) ->
    gen_server:call(Pid, {tool, remove, Name}).

%% @doc 设置系统提示词
-spec set_system_prompt(pid(), binary()) -> ok | {error, term()}.
set_system_prompt(Pid, Prompt) ->
    gen_server:call(Pid, {config, set_prompt, Prompt}).

%% @doc 获取 scratchpad
-spec get_scratchpad(pid()) -> [map()].
get_scratchpad(Pid) ->
    gen_server:call(Pid, {state, get_scratchpad}).

%% @doc 清空 scratchpad
-spec clear_scratchpad(pid()) -> ok.
clear_scratchpad(Pid) ->
    gen_server:call(Pid, {state, clear_scratchpad}).

%% @doc 获取消息历史
-spec get_messages(pid()) -> [map()].
get_messages(Pid) ->
    gen_server:call(Pid, {state, get_messages}).

%% @doc 清空消息历史
-spec clear_messages(pid()) -> ok.
clear_messages(Pid) ->
    gen_server:call(Pid, {state, clear_messages}).

%% @doc 获取上下文
-spec get_context(pid()) -> map().
get_context(Pid) ->
    gen_server:call(Pid, {context, get}).

%% @doc 设置上下文
-spec set_context(pid(), map()) -> ok.
set_context(Pid, Context) when is_map(Context) ->
    gen_server:call(Pid, {context, set, Context}).

%% @doc 更新上下文
-spec update_context(pid(), map()) -> ok.
update_context(Pid, Updates) when is_map(Updates) ->
    gen_server:call(Pid, {context, update, Updates}).

%% @doc 设置单个上下文值
-spec put_context(pid(), atom() | binary(), term()) -> ok.
put_context(Pid, Key, Value) ->
    gen_server:call(Pid, {context, put, Key, Value}).

%% @doc 获取元数据
-spec get_meta(pid()) -> map().
get_meta(Pid) ->
    gen_server:call(Pid, {meta, get}).

%% @doc 设置元数据
-spec set_meta(pid(), map()) -> ok.
set_meta(Pid, Meta) when is_map(Meta) ->
    gen_server:call(Pid, {meta, set, Meta}).

%% @doc 设置单个元数据值
-spec put_meta(pid(), atom() | binary(), term()) -> ok.
put_meta(Pid, Key, Value) ->
    gen_server:call(Pid, {meta, put, Key, Value}).

%% @doc 保存 checkpoint
-spec save_checkpoint(pid()) -> {ok, binary()} | {error, term()}.
save_checkpoint(Pid) ->
    save_checkpoint(Pid, #{}).

%% @doc 保存 checkpoint（带元数据）
-spec save_checkpoint(pid(), map()) -> {ok, binary()} | {error, term()}.
save_checkpoint(Pid, Meta) ->
    gen_server:call(Pid, {checkpoint, save, Meta}).

%% @doc 加载 checkpoint
-spec load_checkpoint(pid(), binary()) -> {ok, map()} | {error, term()}.
load_checkpoint(Pid, CheckpointId) ->
    gen_server:call(Pid, {checkpoint, load, CheckpointId}).

%% @doc 加载最新 checkpoint
-spec load_latest_checkpoint(pid()) -> {ok, map()} | {error, term()}.
load_latest_checkpoint(Pid) ->
    gen_server:call(Pid, {checkpoint, load_latest}).

%% @doc 列出 checkpoints
-spec list_checkpoints(pid()) -> {ok, [map()]} | {error, term()}.
list_checkpoints(Pid) ->
    list_checkpoints(Pid, #{}).

%% @doc 列出 checkpoints（带选项）
-spec list_checkpoints(pid(), map()) -> {ok, [map()]} | {error, term()}.
list_checkpoints(Pid, Opts) ->
    gen_server:call(Pid, {checkpoint, list, Opts}).

%% @doc 从 checkpoint 恢复
-spec restore_from_checkpoint(pid(), binary()) -> ok | {error, term()}.
restore_from_checkpoint(Pid, CheckpointId) ->
    gen_server:call(Pid, {checkpoint, restore, CheckpointId}).

%% @doc 恢复中断的执行
-spec resume(pid(), confirm | reject | {modify, term()}) -> ok | {error, term()}.
resume(Pid, Action) ->
    gen_server:call(Pid, {interrupt, resume, Action}, 60000).

%% @doc 中止当前执行
-spec abort(pid()) -> ok.
abort(Pid) ->
    gen_server:call(Pid, {interrupt, abort}).

%% @doc 设置回调
-spec set_callbacks(pid(), map()) -> ok.
set_callbacks(Pid, CallbackOpts) ->
    gen_server:call(Pid, {callback, set, CallbackOpts}).

%% @doc 获取回调
-spec get_callbacks(pid()) -> map().
get_callbacks(Pid) ->
    gen_server:call(Pid, {callback, get}).

%% @doc 发送自定义事件
-spec emit_custom_event(pid(), atom() | binary(), term()) -> ok.
emit_custom_event(Pid, EventName, Data) ->
    emit_custom_event(Pid, EventName, Data, #{}).

%% @doc 发送自定义事件（带元数据）
-spec emit_custom_event(pid(), atom() | binary(), term(), map()) -> ok.
emit_custom_event(Pid, EventName, Data, Metadata) ->
    gen_server:cast(Pid, {emit_custom_event, EventName, Data, Metadata}).

%% @doc 获取 middlewares
-spec get_middlewares(pid()) -> [term()].
get_middlewares(Pid) ->
    gen_server:call(Pid, {middleware, get}).

%% @doc 添加 middleware
-spec add_middleware(pid(), term()) -> ok | {error, term()}.
add_middleware(Pid, MiddlewareSpec) ->
    gen_server:call(Pid, {middleware, add, MiddlewareSpec}).

%% @doc 移除 middleware
-spec remove_middleware(pid(), module()) -> ok | {error, term()}.
remove_middleware(Pid, Module) ->
    gen_server:call(Pid, {middleware, remove, Module}).

%% @doc 设置 middlewares
-spec set_middlewares(pid(), [term()]) -> ok | {error, term()}.
set_middlewares(Pid, Middlewares) ->
    gen_server:call(Pid, {middleware, set, Middlewares}).

%% @doc 导出状态
-spec export_state(pid()) -> map().
export_state(Pid) ->
    gen_server:call(Pid, {state, export}).

%% @doc 导入状态
-spec import_state(pid(), map()) -> ok.
import_state(Pid, ExportedData) ->
    gen_server:call(Pid, {state, import, ExportedData}).

%%====================================================================
%% gen_server 回调
%%====================================================================

%% @doc 初始化服务器
init({Id, Opts}) ->
    case beamai_agent_init:create_state(Id, Opts) of
        {ok, State} -> {ok, State};
        {error, Reason} -> {stop, Reason}
    end.

%% @doc 处理同步调用（分组分发）
%%
%% 使用分组分发减少 handle_call 分支数量，提高可读性。
%% 每种操作类型委托给对应的 dispatch_* 函数。
handle_call({run, Msg, Opts}, _From, State) ->
    handle_run(Msg, Opts, State);

handle_call({tool, Op, Arg}, _From, State) ->
    dispatch_tool(Op, Arg, State);

handle_call({config, Op, Arg}, _From, State) ->
    dispatch_config(Op, Arg, State);

handle_call({state, Op}, _From, State) ->
    dispatch_state_query(Op, State);

handle_call({state, Op, Arg}, _From, State) ->
    dispatch_state_mutation(Op, Arg, State);

handle_call({context, Op}, _From, State) ->
    dispatch_context(Op, undefined, undefined, State);

handle_call({context, Op, Arg}, _From, State) ->
    dispatch_context(Op, Arg, undefined, State);

handle_call({context, Op, Arg1, Arg2}, _From, State) ->
    dispatch_context(Op, Arg1, Arg2, State);

handle_call({meta, Op}, _From, State) ->
    dispatch_meta(Op, undefined, undefined, State);

handle_call({meta, Op, Arg}, _From, State) ->
    dispatch_meta(Op, Arg, undefined, State);

handle_call({meta, Op, Arg1, Arg2}, _From, State) ->
    dispatch_meta(Op, Arg1, Arg2, State);

handle_call({checkpoint, Op}, _From, State) ->
    dispatch_checkpoint(Op, undefined, State);

handle_call({checkpoint, Op, Arg}, _From, State) ->
    dispatch_checkpoint(Op, Arg, State);

handle_call({interrupt, Op}, _From, State) ->
    dispatch_interrupt(Op, undefined, State);

handle_call({interrupt, Op, Arg}, _From, State) ->
    dispatch_interrupt(Op, Arg, State);

handle_call({callback, Op}, _From, State) ->
    dispatch_callback(Op, undefined, State);

handle_call({callback, Op, Arg}, _From, State) ->
    dispatch_callback(Op, Arg, State);

handle_call({middleware, Op}, _From, State) ->
    dispatch_middleware(Op, undefined, State);

handle_call({middleware, Op, Arg}, _From, State) ->
    dispatch_middleware(Op, Arg, State);

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @doc 处理异步消息
handle_cast({emit_custom_event, EventName, Data, Metadata},
            #state{config = #agent_config{callbacks = Callbacks}} = State) ->
    FullMetadata = maps:merge(beamai_agent_callbacks:build_metadata(State), Metadata),
    beamai_agent_callbacks:invoke(on_custom_event, [EventName, Data, FullMetadata], Callbacks),
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
%% 分发函数（按类别分组）
%%====================================================================

%% @private 分发工具操作
-spec dispatch_tool(atom(), term(), #state{}) ->
    {reply, ok | {error, term()}, #state{}}.
dispatch_tool(add, Tool,
              #state{config = #agent_config{tools = Tools, tool_handlers = Handlers} = Config} = State) ->
    NewTools = [Tool | Tools],
    NewHandlers = maps:merge(Handlers, beamai_nodes:build_tool_handlers([Tool])),
    NewConfig = Config#agent_config{tools = NewTools, tool_handlers = NewHandlers},
    rebuild_and_reply(State#state{config = NewConfig}, State);

dispatch_tool(remove, Name,
              #state{config = #agent_config{tools = Tools, tool_handlers = Handlers} = Config} = State) ->
    NewTools = lists:filter(fun(#{name := ToolName}) -> ToolName =/= Name end, Tools),
    NewHandlers = maps:remove(Name, Handlers),
    NewConfig = Config#agent_config{tools = NewTools, tool_handlers = NewHandlers},
    rebuild_and_reply(State#state{config = NewConfig}, State).

%% @private 分发配置操作
-spec dispatch_config(atom(), term(), #state{}) ->
    {reply, ok | {error, term()}, #state{}}.
dispatch_config(set_prompt, Prompt, #state{config = Config} = State) ->
    NewConfig = Config#agent_config{system_prompt = Prompt},
    rebuild_and_reply(State#state{config = NewConfig}, State).

%% @private 分发状态查询操作
-spec dispatch_state_query(atom(), #state{}) ->
    {reply, term(), #state{}}.
dispatch_state_query(get_scratchpad, #state{scratchpad = Pad} = State) ->
    {reply, lists:reverse(Pad), State};
dispatch_state_query(clear_scratchpad, State) ->
    {reply, ok, State#state{scratchpad = []}};
dispatch_state_query(get_messages, #state{messages = Msgs} = State) ->
    {reply, Msgs, State};
dispatch_state_query(clear_messages, State) ->
    {reply, ok, State#state{messages = []}};
dispatch_state_query(export, State) ->
    Exported = #{
        messages => State#state.messages,
        full_messages => State#state.full_messages,
        scratchpad => State#state.scratchpad,
        context => State#state.context
    },
    {reply, Exported, State}.

%% @private 分发状态修改操作
-spec dispatch_state_mutation(atom(), term(), #state{}) ->
    {reply, ok, #state{}}.
dispatch_state_mutation(import, ExportedData, #state{context = CurrentCtx} = State) ->
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
    {reply, ok, NewState}.

%% @private 分发上下文操作
-spec dispatch_context(atom(), term(), term(), #state{}) ->
    {reply, term(), #state{}}.
dispatch_context(get, _, _, #state{context = Context} = State) ->
    {reply, Context, State};
dispatch_context(set, NewContext, _, State) ->
    {reply, ok, State#state{context = NewContext}};
dispatch_context(update, Updates, _, #state{context = Context} = State) ->
    NewContext = maps:merge(Context, Updates),
    {reply, ok, State#state{context = NewContext}};
dispatch_context(put, Key, Value, #state{context = Context} = State) ->
    NewContext = maps:put(Key, Value, Context),
    {reply, ok, State#state{context = NewContext}}.

%% @private 分发元数据操作
-spec dispatch_meta(atom(), term(), term(), #state{}) ->
    {reply, term(), #state{}}.
dispatch_meta(get, _, _, #state{config = #agent_config{meta = Meta}} = State) ->
    {reply, Meta, State};
dispatch_meta(set, NewMeta, _, #state{config = Config} = State) ->
    NewConfig = Config#agent_config{meta = NewMeta},
    {reply, ok, State#state{config = NewConfig}};
dispatch_meta(put, Key, Value, #state{config = #agent_config{meta = Meta} = Config} = State) ->
    NewMeta = maps:put(Key, Value, Meta),
    NewConfig = Config#agent_config{meta = NewMeta},
    {reply, ok, State#state{config = NewConfig}}.

%% @private 分发 checkpoint 操作
-spec dispatch_checkpoint(atom(), term(), #state{}) ->
    {reply, term(), #state{}}.
dispatch_checkpoint(save, Meta, State) ->
    {reply, beamai_agent_checkpoint:save(Meta, State), State};
dispatch_checkpoint(load, CpId, State) ->
    {reply, beamai_agent_checkpoint:load(CpId, State), State};
dispatch_checkpoint(load_latest, _, State) ->
    {reply, beamai_agent_checkpoint:load_latest(State), State};
dispatch_checkpoint(list, Opts, State) ->
    {reply, beamai_agent_checkpoint:list(Opts, State), State};
dispatch_checkpoint(restore, CpId, State) ->
    case beamai_agent_checkpoint:restore(CpId, State) of
        {ok, NewState} -> {reply, ok, NewState};
        {error, Reason} -> {reply, {error, Reason}, State}
    end.

%% @private 分发中断操作
-spec dispatch_interrupt(atom(), term(), #state{}) ->
    {reply, ok | {error, term()}, #state{}}.
dispatch_interrupt(resume, Action, #state{pending_action = Pending} = State)
  when Pending =/= undefined ->
    handle_resume(Action, Pending, State);
dispatch_interrupt(resume, _, State) ->
    {reply, {error, no_pending_action}, State};
dispatch_interrupt(abort, _, State) ->
    {reply, ok, State#state{pending_action = undefined}}.

%% @private 分发回调操作
-spec dispatch_callback(atom(), term(), #state{}) ->
    {reply, term(), #state{}}.
dispatch_callback(get, _, #state{config = #agent_config{callbacks = Callbacks}} = State) ->
    {reply, beamai_agent_callbacks:to_map(Callbacks), State};
dispatch_callback(set, CallbackOpts,
                  #state{config = #agent_config{callbacks = Callbacks} = Config} = State) ->
    NewCallbacks = beamai_agent_callbacks:update(Callbacks, CallbackOpts),
    NewConfig = Config#agent_config{callbacks = NewCallbacks},
    {reply, ok, State#state{config = NewConfig}}.

%% @private 分发 middleware 操作
-spec dispatch_middleware(atom(), term(), #state{}) ->
    {reply, term(), #state{}}.
dispatch_middleware(get, _, #state{config = #agent_config{middlewares = Middlewares}} = State) ->
    {reply, Middlewares, State};
dispatch_middleware(add, MiddlewareSpec,
                    #state{config = #agent_config{middlewares = Middlewares}} = State) ->
    NewMiddlewares = Middlewares ++ [MiddlewareSpec],
    handle_update_middlewares(NewMiddlewares, State);
dispatch_middleware(remove, Module,
                    #state{config = #agent_config{middlewares = Middlewares}} = State) ->
    NewMiddlewares = lists:filter(fun(Spec) ->
        extract_middleware_module(Spec) =/= Module
    end, Middlewares),
    handle_update_middlewares(NewMiddlewares, State);
dispatch_middleware(set, NewMiddlewares, State) ->
    handle_update_middlewares(NewMiddlewares, State).

%%====================================================================
%% 处理函数
%%====================================================================

%% @private 处理执行请求
%%
%% 调用图执行引擎执行对话，并触发相应回调。
-spec handle_run(binary(), map(), #state{}) ->
    {reply, {ok, map()} | {error, term()}, #state{}}.
handle_run(Msg, Opts, #state{config = #agent_config{callbacks = Callbacks}} = State) ->
    Metadata = beamai_agent_callbacks:build_metadata(State),

    %% 调用 chain_start 回调
    beamai_agent_callbacks:invoke(on_chain_start, [Msg, Metadata], Callbacks),

    case beamai_agent_runner:execute(Msg, Opts, State) of
        {ok, Result, NewState} ->
            beamai_agent_callbacks:invoke(on_chain_end, [Result, Metadata],
                                          NewState#state.config#agent_config.callbacks),
            FinalState = beamai_agent_checkpoint:maybe_auto_save(Result, NewState),
            {reply, {ok, Result}, FinalState};
        {error, Reason, NewState} ->
            beamai_agent_callbacks:invoke(on_chain_error, [Reason, Metadata],
                                          NewState#state.config#agent_config.callbacks),
            {reply, {error, Reason}, NewState}
    end.

%% @private 处理恢复操作
-spec handle_resume(term(), map(), #state{}) ->
    {reply, ok | {error, term()}, #state{}}.
handle_resume(confirm, _Pending, State) ->
    {reply, ok, State#state{pending_action = undefined}};
handle_resume(reject, _Pending, State) ->
    {reply, ok, State#state{pending_action = undefined}};
handle_resume({modify, _NewData}, _Pending, State) ->
    {reply, ok, State#state{pending_action = undefined}};
handle_resume(_, _, State) ->
    {reply, {error, invalid_action}, State}.

%% @private 更新 middlewares 并重建图
-spec handle_update_middlewares([term()], #state{}) ->
    {reply, ok | {error, term()}, #state{}}.
handle_update_middlewares(NewMiddlewares,
                          #state{config = #agent_config{tools = Tools, system_prompt = Prompt,
                                                         llm_config = LLMConfig, max_iterations = MaxIter,
                                                         response_format = RF} = Config} = State) ->
    %% 初始化新的 middleware 链
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
            NewConfig = Config#agent_config{
                middlewares = NewMiddlewares,
                middleware_chain = NewChain,
                graph = NewGraph
            },
            {reply, ok, State#state{config = NewConfig}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end.

%%====================================================================
%% 辅助函数
%%====================================================================

%% @private 重建图并返回结果
%%
%% 在配置变更后重建执行图。
-spec rebuild_and_reply(#state{}, #state{}) ->
    {reply, ok | {error, term()}, #state{}}.
rebuild_and_reply(NewState, OldState) ->
    case beamai_agent_runner:rebuild_graph(NewState) of
        {ok, UpdatedState} -> {reply, ok, UpdatedState};
        {error, Reason} -> {reply, {error, Reason}, OldState}
    end.

%% @private 从规格中提取 middleware 模块
-spec extract_middleware_module(term()) -> module().
extract_middleware_module({Module, _Opts, _Priority}) -> Module;
extract_middleware_module({Module, _Opts}) -> Module;
extract_middleware_module(Module) when is_atom(Module) -> Module.
