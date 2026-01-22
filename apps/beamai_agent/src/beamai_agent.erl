%%%-------------------------------------------------------------------
%%% @doc Agent 门面模块
%%%
%%% 本模块作为 Agent 系统的 API 门面。
%%% 实际实现委托给专门的模块：
%%%
%%% - beamai_agent_server: gen_server 实现（有状态进程）
%%% - beamai_agent_api: 纯函数 API（无状态）
%%% - beamai_agent_init: 初始化逻辑
%%% - beamai_agent_runner: 图执行逻辑
%%% - beamai_agent_callbacks: 回调管理
%%% - beamai_agent_checkpoint: Checkpoint 持久化
%%%
%%% == 使用模式 ==
%%%
%%% === 纯函数模式（推荐） ===
%%% 适用于单次对话、高并发、无状态服务。
%%% 主要 API: run_once/2, create_state/1, run_with_state/3
%%%
%%% === 进程模式（高级） ===
%%% 适用于长期运行的服务、监督树、自动状态管理。
%%% 主要 API: start_link/2, run/2
%%%
%%% 设计原则：
%%% - 本模块为纯门面，不包含业务逻辑
%%% - 进程 API 委托给 beamai_agent_server
%%% - 纯函数 API 委托给 beamai_agent_api
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent).

-include("beamai_agent.hrl").

%%====================================================================
%% 进程 API（委托给 beamai_agent_server）
%%====================================================================
-export([start_link/2, stop/1]).
-export([run/2, run/3]).
-export([add_tool/2, remove_tool/2, set_system_prompt/2]).

%% 状态查询 API
-export([get_scratchpad/1, clear_scratchpad/1]).
-export([get_messages/1, clear_messages/1]).

%% Context API
-export([get_context/1, get_context/2, get_context/3]).
-export([set_context/2, update_context/2, put_context/3]).

%% Meta API（进程级元数据，不参与对话）
-export([get_meta/1, get_meta/2, get_meta/3]).
-export([set_meta/2, put_meta/3]).

%% 中断控制 API
-export([resume/2, abort/1]).

%% 回调 API
-export([set_callbacks/2, get_callbacks/1]).
-export([emit_custom_event/3, emit_custom_event/4]).

%% Middleware API
-export([get_middlewares/1, add_middleware/2, remove_middleware/2]).
-export([set_middlewares/2]).

%% 状态导入/导出 API（进程模式）
-export([export_state/1, import_state/2]).

%%====================================================================
%% 纯函数 API（委托给 beamai_agent_api）
%%====================================================================
-export([
    run_once/2,
    run_with_state/3,
    create_state/1,
    create_state/2
]).

%% 纯函数状态导入/导出
-export([export_state_pure/1, import_state_pure/2]).

%%====================================================================
%% 图构建 API
%%====================================================================
-export([build_graph/1]).

%%====================================================================
%% 协调器 API
%%====================================================================
-export([start_pipeline/2, start_orchestrator/2]).

%%====================================================================
%% 进程 API 实现
%%====================================================================

%% @doc 启动 Agent 进程
%%
%% 创建一个 gen_server 进程来管理 Agent 状态。
%% 适用于需要长期运行的场景。
%%
%% @param Id Agent 唯一标识符
%% @param Opts 配置选项
%% @returns {ok, Pid} | {error, Reason}
-spec start_link(binary(), map()) -> {ok, pid()} | {error, term()}.
start_link(Id, Opts) ->
    beamai_agent_server:start_link(Id, Opts).

%% @doc 停止 Agent 进程
-spec stop(pid()) -> ok.
stop(Pid) ->
    beamai_agent_server:stop(Pid).

%% @doc 执行对话
-spec run(pid(), binary()) -> {ok, map()} | {error, term()}.
run(Pid, Msg) ->
    beamai_agent_server:run(Pid, Msg).

%% @doc 执行对话（带选项）
-spec run(pid(), binary(), map()) -> {ok, map()} | {error, term()}.
run(Pid, Msg, Opts) ->
    beamai_agent_server:run(Pid, Msg, Opts).

%% @doc 添加工具
-spec add_tool(pid(), map()) -> ok | {error, term()}.
add_tool(Pid, Tool) ->
    beamai_agent_server:add_tool(Pid, Tool).

%% @doc 移除工具
-spec remove_tool(pid(), binary()) -> ok | {error, term()}.
remove_tool(Pid, Name) ->
    beamai_agent_server:remove_tool(Pid, Name).

%% @doc 设置系统提示词
-spec set_system_prompt(pid(), binary()) -> ok | {error, term()}.
set_system_prompt(Pid, Prompt) ->
    beamai_agent_server:set_system_prompt(Pid, Prompt).

%%====================================================================
%% 状态查询 API
%%====================================================================

%% @doc 获取 scratchpad（中间步骤记录）
-spec get_scratchpad(pid()) -> [map()].
get_scratchpad(Pid) ->
    beamai_agent_server:get_scratchpad(Pid).

%% @doc 清空 scratchpad
-spec clear_scratchpad(pid()) -> ok.
clear_scratchpad(Pid) ->
    beamai_agent_server:clear_scratchpad(Pid).

%% @doc 获取对话历史
-spec get_messages(pid()) -> [map()].
get_messages(Pid) ->
    beamai_agent_server:get_messages(Pid).

%% @doc 清空对话历史
-spec clear_messages(pid()) -> ok.
clear_messages(Pid) ->
    beamai_agent_server:clear_messages(Pid).

%%====================================================================
%% Context API
%%====================================================================

%% @doc 获取完整上下文
-spec get_context(pid()) -> map().
get_context(Pid) ->
    beamai_agent_server:get_context(Pid).

%% @doc 获取上下文值
-spec get_context(pid(), atom() | binary()) -> term() | undefined.
get_context(Pid, Key) ->
    get_context(Pid, Key, undefined).

%% @doc 获取上下文值（带默认值）
-spec get_context(pid(), atom() | binary(), term()) -> term().
get_context(Pid, Key, Default) ->
    Context = beamai_agent_server:get_context(Pid),
    maps:get(Key, Context, Default).

%% @doc 设置完整上下文
-spec set_context(pid(), map()) -> ok.
set_context(Pid, Context) when is_map(Context) ->
    beamai_agent_server:set_context(Pid, Context).

%% @doc 更新上下文（合并）
-spec update_context(pid(), map()) -> ok.
update_context(Pid, Updates) when is_map(Updates) ->
    beamai_agent_server:update_context(Pid, Updates).

%% @doc 设置单个上下文值
-spec put_context(pid(), atom() | binary(), term()) -> ok.
put_context(Pid, Key, Value) ->
    beamai_agent_server:put_context(Pid, Key, Value).

%%====================================================================
%% Meta API（进程级元数据，不参与对话）
%%====================================================================

%% @doc 获取完整元数据
-spec get_meta(pid()) -> map().
get_meta(Pid) ->
    beamai_agent_server:get_meta(Pid).

%% @doc 获取元数据值
-spec get_meta(pid(), atom() | binary()) -> term() | undefined.
get_meta(Pid, Key) ->
    get_meta(Pid, Key, undefined).

%% @doc 获取元数据值（带默认值）
-spec get_meta(pid(), atom() | binary(), term()) -> term().
get_meta(Pid, Key, Default) ->
    Meta = beamai_agent_server:get_meta(Pid),
    maps:get(Key, Meta, Default).

%% @doc 设置完整元数据
-spec set_meta(pid(), map()) -> ok.
set_meta(Pid, Meta) when is_map(Meta) ->
    beamai_agent_server:set_meta(Pid, Meta).

%% @doc 设置单个元数据值
-spec put_meta(pid(), atom() | binary(), term()) -> ok.
put_meta(Pid, Key, Value) ->
    beamai_agent_server:put_meta(Pid, Key, Value).

%%====================================================================
%% 中断控制 API
%%====================================================================

%% @doc 恢复中断的执行
-spec resume(pid(), confirm | reject | {modify, term()}) -> ok | {error, term()}.
resume(Pid, Action) ->
    beamai_agent_server:resume(Pid, Action).

%% @doc 中止当前执行
-spec abort(pid()) -> ok.
abort(Pid) ->
    beamai_agent_server:abort(Pid).

%%====================================================================
%% 回调 API
%%====================================================================

%% @doc 设置回调处理器
-spec set_callbacks(pid(), map()) -> ok.
set_callbacks(Pid, CallbackOpts) ->
    beamai_agent_server:set_callbacks(Pid, CallbackOpts).

%% @doc 获取当前回调配置
-spec get_callbacks(pid()) -> map().
get_callbacks(Pid) ->
    beamai_agent_server:get_callbacks(Pid).

%% @doc 发送自定义事件
-spec emit_custom_event(pid(), atom() | binary(), term()) -> ok.
emit_custom_event(Pid, EventName, Data) ->
    beamai_agent_server:emit_custom_event(Pid, EventName, Data).

%% @doc 发送自定义事件（带元数据）
-spec emit_custom_event(pid(), atom() | binary(), term(), map()) -> ok.
emit_custom_event(Pid, EventName, Data, Metadata) ->
    beamai_agent_server:emit_custom_event(Pid, EventName, Data, Metadata).

%%====================================================================
%% Middleware API
%%====================================================================

%% @doc 获取当前 middleware 列表
-spec get_middlewares(pid()) -> [term()].
get_middlewares(Pid) ->
    beamai_agent_server:get_middlewares(Pid).

%% @doc 添加 middleware
-spec add_middleware(pid(), term()) -> ok | {error, term()}.
add_middleware(Pid, MiddlewareSpec) ->
    beamai_agent_server:add_middleware(Pid, MiddlewareSpec).

%% @doc 移除 middleware
-spec remove_middleware(pid(), module()) -> ok | {error, term()}.
remove_middleware(Pid, Module) ->
    beamai_agent_server:remove_middleware(Pid, Module).

%% @doc 设置所有 middleware
-spec set_middlewares(pid(), [term()]) -> ok | {error, term()}.
set_middlewares(Pid, Middlewares) ->
    beamai_agent_server:set_middlewares(Pid, Middlewares).

%%====================================================================
%% 状态导入/导出 API（进程模式）
%%====================================================================

%% @doc 导出状态（用于外部持久化）
%%
%% 支持 pid（进程模式）或 #state{}（纯函数模式）。
-spec export_state(pid() | #state{}) -> map().
export_state(Pid) when is_pid(Pid) ->
    beamai_agent_server:export_state(Pid);
export_state(#state{} = State) ->
    beamai_agent_api:export_state(State).

%% @doc 导入状态
%%
%% 进程模式：传入 pid 和导出数据
%% 纯函数模式：使用 import_state_pure/2
-spec import_state(pid(), map()) -> ok.
import_state(Pid, ExportedData) when is_pid(Pid) ->
    beamai_agent_server:import_state(Pid, ExportedData).

%%====================================================================
%% 纯函数 API 实现
%%====================================================================

%% @doc 单次执行（无状态保留，推荐用于单轮对话）
-spec run_once(map(), binary()) -> {ok, map()} | {error, term()}.
run_once(Config, Message) ->
    beamai_agent_api:run_once(Config, Message).

%% @doc 带状态执行（多轮对话）
-spec run_with_state(#state{}, binary(), map()) ->
    {ok, map(), #state{}} | {error, term(), #state{}}.
run_with_state(State, Message, Opts) ->
    beamai_agent_api:run_with_state(State, Message, Opts).

%% @doc 创建 Agent 状态（自动生成 ID）
-spec create_state(map()) -> {ok, #state{}} | {error, term()}.
create_state(Config) ->
    beamai_agent_api:create_state(Config).

%% @doc 创建 Agent 状态（指定 ID）
-spec create_state(binary(), map()) -> {ok, #state{}} | {error, term()}.
create_state(Id, Config) ->
    beamai_agent_api:create_state(Id, Config).

%% @doc 导出纯函数状态
-spec export_state_pure(#state{}) -> map().
export_state_pure(#state{} = State) ->
    beamai_agent_api:export_state(State).

%% @doc 导入纯函数状态
-spec import_state_pure(map(), map()) -> {ok, #state{}} | {error, term()}.
import_state_pure(ExportedData, Config) ->
    beamai_agent_api:import_state(ExportedData, Config).

%%====================================================================
%% 图构建 API
%%====================================================================

%% @doc 构建 Agent 执行图
-spec build_graph(map()) -> {ok, map()} | {error, term()}.
build_graph(Opts) ->
    beamai_agent_runner:build_graph(Opts).

%%====================================================================
%% 协调器 API
%%====================================================================

%% @doc 启动 Pipeline 协调器
-spec start_pipeline(binary(), map()) -> {ok, pid()} | {error, term()}.
start_pipeline(Id, Opts) ->
    beamai_coordinator:start_pipeline(Id, Opts).

%% @doc 启动 Orchestrator 协调器
-spec start_orchestrator(binary(), map()) -> {ok, pid()} | {error, term()}.
start_orchestrator(Id, Opts) ->
    beamai_coordinator:start_orchestrator(Id, Opts).
