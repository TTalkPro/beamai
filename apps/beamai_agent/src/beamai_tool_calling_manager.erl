%%%-------------------------------------------------------------------
%%% @doc ToolCallingManager Behaviour：工具执行的统一管理 seam
%%%
%%% 把「执行入口」从 {@link beamai_agent_utils:execute_tools/5} 的私有函数
%%% 升格为可注入 behaviour。与 {@link beamai_memory_provider} 的
%%% <code>&#123;Module, Ref&#125;</code> + 分派 API 模式完全一致。
%%%
%%% 协议只有一个方法 <code>execute_tool_calls/4</code>（Spring 对齐命名）。
%%% 多种执行策略通过不同实现模块选择：
%%% <ul>
%%%   <li>{@link beamai_concurrent_tool_calling_manager} —— 默认，
%%%       spawn_monitor 并发，尊重 <code>parallel</code> / <code>serial</code>
%%%       声明（现状行为）</li>
%%%   <li>{@link beamai_sequential_tool_calling_manager} —— 全串行，
%%%       每个 tool_call 按序执行、无并发。适合调试或严格副作用场景</li>
%%% </ul>
%%%
%%% <code>execute_tools/5</code>（批调度）和 <code>run_one_tool/3</code>
%%% （单工具执行）是各实现模块内部委托的 helper，不是协议方法。
%%%
%%% <b>隔离是本 behaviour 的契约，不是可选项</b>：manager 是工具执行引擎，
%%% 持有进程模型。实现**必须**保证工具执行不会带崩调用者进程——用
%%% {@link beamai_tool_batch_worker:execute_isolated/5} 即可满足（内置的两个
%%% 实现都走它）。
%%%
%%% 边界划在 manager 级、而非每工具一个进程：工具存在必须串行执行的场景，
%%% 每工具一进程会把「隔离」与「调度」绑死。正确的正交是：
%%% <ul>
%%%   <li><b>进程边界</b> = manager 级，恒定一层，与批次大小 / 策略无关</li>
%%%   <li><b>串行 / 并发</b> = 边界**内部**的调度自由</li>
%%% </ul>
%%% 故 sequential manager 同样在独立进程里跑——串行不等于不隔离。
%%% 代价：故障单元是「批」而非「单个工具」（批崩 → 整批合成 error）。
%%%
%%% 定位（vs 现有抽象）：
%%% <ul>
%%%   <li><b>不夺</b> <code>parallel_tools</code> / <code>serial</code> 的权——
%%%       声明级执行策略仍由工具声明者 / agent 配置决定，manager 在分派前读取</li>
%%%   <li><b>不夺</b> <code>around_tool</code> filter 链的权——单工具洋葱链继续工作</li>
%%%   <li><b>不夺</b> <code>writes</code> 屏障的权——状态折叠仍走
%%%       {@link beamai_context:apply_writes/3}</li>
%%%   <li>只负责「拿到批准的 calls 后，执行 + 收结果」这一段</li>
%%% </ul>
%%%
%%% 循环控制（gate / eligibility / return_direct / env_pause）仍由
%%% {@link beamai_agent_tool_loop} 负责，不进 manager。
%%%
%%% 句柄约定 <code>&#123;Module, Ref&#125;</code>。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_calling_manager).

%% 调度 API
-export([execute_tool_calls/4]).
%% 构造
-export([default/0, concurrent/0, concurrent/1, sequential/0, sequential/1]).
%% 供实现模块解析 Ref
-export([opts_from_ref/1]).

-export_type([manager/0, manager_opts/0, execute_opts/0, execute_result/0]).

-type manager() :: {module(), term()}.

%% 构造 manager 时给定的执行策略（存进 {Mod, Ref} 的 Ref）。
%%
%% manager 是执行引擎，时间上限属于执行策略，故在此配置——而非散落在 app env
%% 或逐个工具声明。
%%
%% <b>两项都缺省 infinity</b>：框架不替调用者判断「多久算太久」，缺省一直等到
%% 工具交付，谁完成了、谁还没完成经 on_result 实时可见。要杀须显式声明。
-type manager_opts() :: #{
    %% 该 manager 执行的工具的**缺省**超时（毫秒）。工具自己声明的
    %% tool_spec.timeout 优先。不给即 infinity——工具跑多久等多久。
    tool_timeout => pos_integer() | infinity,
    %% 批级截止（毫秒）。不给即 infinity。给的时候应严格大于并发收集截止
    %% （app env `tool_gather_timeout'），否则并发路径本可保住的部分结果会被
    %% 整批冲成错误（见 beamai_tool_batch_worker:batch_timeout/1）。
    batch_timeout => pos_integer() | infinity
}.

-type execute_opts() :: #{
    context => beamai_context:t(),
    parallel => boolean(),
    on_result => fun((map()) -> ok)
}.

-type execute_result() :: #{
    messages := [map()],
    records := [map()],
    context := beamai_context:t()
}.

%%====================================================================
%% Behaviour 回调
%%====================================================================

%% @doc 执行工具调用（批量调度 + 屏障折叠）。
%%
%% Manager 为 <code>&#123;Mod, Ref&#125;</code>；调用方经
%% {@link execute_tool_calls/4} 分派到
%% <code>Mod:execute_tool_calls(Ref, Kernel, ToolCalls, Opts)</code>。
%% Ref 为实现模块的私有状态（默认实现不用，自定义实现可携配置）。
%%
%% 返回 <code>#{messages, records, context}</code>：
%% <ul>
%%   <li><b>messages</b>：tool 结果消息（按原 call 序，喂回 LLM 的 tool results）</li>
%%   <li><b>records</b>：调用记录（含 name/args/result/tool_call_id/error）</li>
%%   <li><b>context</b>：应用 writes 后的 context（屏障折叠后）</li>
%%% </ul>
%%
%% 内部如何调度（spawn_monitor 并发 / 全串行 / 线程池）完全由实现模块决定。
%% 协议不规定、框架不强加——选 manager 即选策略。
-callback execute_tool_calls(Ref, Kernel, ToolCalls, Opts) -> ExecuteResult when
    Ref            :: term(),
    Kernel         :: beamai_kernel:kernel(),
    ToolCalls      :: [map()],
    Opts           :: execute_opts(),
    ExecuteResult  :: execute_result().

%%====================================================================
%% 调度 API
%%====================================================================

%% @doc 分派到 manager 实现模块的 execute_tool_calls/4。
%%
%% 与 {@link beamai_memory_provider:history/2} 等分派函数同款：拆开
%% <code>&#123;Mod, Ref&#125;</code>，调 <code>Mod:execute_tool_calls(Ref, ...)</code>。
-spec execute_tool_calls(manager(), beamai_kernel:kernel(), [map()], execute_opts()) ->
    execute_result().
execute_tool_calls({Module, Ref}, Kernel, ToolCalls, Opts) ->
    Module:execute_tool_calls(Ref, Kernel, ToolCalls, Opts).

%%====================================================================
%% 构造
%%====================================================================

%% @doc 构造默认 manager（并发，尊重 parallel / serial 声明）。
-spec default() -> manager().
default() ->
    concurrent().

%% @doc 构造并发 manager（spawn_monitor，尊重 parallel / serial 声明）。
%%
%% 现状行为：parallel=true 且批内 >1 个 call 且无 serial 工具时并发执行；
%% 否则串行。适合大多数场景。
-spec concurrent() -> manager().
concurrent() ->
    concurrent(#{}).

%% @doc 构造并发 manager，并给定执行策略（见 {@link manager_opts()}）。
%%
%% 例：给未声明 timeout 的工具统一加 5 分钟上限（缺省是不限时），不必逐个改
%% tool_spec：
%% ```
%% beamai_tool_calling_manager:concurrent(#{tool_timeout => 300000})
%% '''
-spec concurrent(manager_opts()) -> manager().
concurrent(Opts) ->
    beamai_concurrent_tool_calling_manager:new(Opts).

%% @doc 构造串行 manager（永远按序执行、无并发）。
%%
%% 忽略 parallel opts，每个 tool_call 严格按序执行。
%% 适合调试或严格副作用场景。
-spec sequential() -> manager().
sequential() ->
    sequential(#{}).

%% @doc 构造串行 manager，并给定执行策略（见 {@link manager_opts()}）。
-spec sequential(manager_opts()) -> manager().
sequential(Opts) ->
    beamai_sequential_tool_calling_manager:new(Opts).

%% @doc 从 {@link manager()} 的 Ref 解析出 manager_opts。
%%
%% 兼容早期把 Ref 当占位原子（`default'）的构造形态——彼时 manager 无配置可携。
-spec opts_from_ref(term()) -> manager_opts().
opts_from_ref(Opts) when is_map(Opts) -> Opts;
opts_from_ref(_Legacy) -> #{}.
