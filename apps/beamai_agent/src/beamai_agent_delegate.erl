%%%-------------------------------------------------------------------
%%% @doc 子 Agent 委派工具（构建在 beamai_subagent_manager 之上）
%%%
%%% 所有子 agent 执行统一经 `beamai_subagent_manager`(受管异步会话)。本模块提供
%%% 两类入口：
%%%
%%%  - **同步便利**(一次调用拿结果，绝大多数 ReAct 委派)：
%%%      tool/1        —— 单委派(spawn→await→drop)
%%%      fanout_tool/1 —— 一次调用并发多个子任务(spawn 全部→await 全部)
%%%      run_many/2,3  —— 程序化并发 fan-out
%%%  - **异步管理**(长跑/监控/监督，LLM 跨轮自管)：
%%%      management_tools/1 —— spawn/list/result/kill/restart 一组工具
%%%
%%% 同步入口在 await 超时后会 kill+drop(有界);异步管理入口把生命周期交给 LLM/调用方。
%%% 子 agent 默认隔离：`memory => false` + fresh conversation_id(subagent 配置可覆盖)。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_delegate).

-export([tool/1, fanout_tool/1, management_tools/1]).
-export([run_many/2, run_many/3]).

-type config() :: #{
    name := binary(),
    description => binary(),
    subagent := fun((map(), beamai_context:t()) -> map()),
    seed => fun((map(), beamai_context:t()) -> binary()),
    result => fun((map()) -> binary()),
    timeout => timeout()
}.
-export_type([config/0]).

-define(DEFAULT_TIMEOUT, 60000).

%%====================================================================
%% 同步委派工具
%%====================================================================

%% @doc 单委派工具(同步:spawn→await→drop)。参数 task(必)+ context(可选,父 LLM 填)。
-spec tool(config()) -> map().
tool(Config) ->
    {SubagentFn, SeedFn, ResultFn, Timeout} = unpack(Config),
    #{
        name => maps:get(name, Config),
        description => maps:get(description, Config,
                                <<"Delegate a subtask to an isolated sub-agent.">>),
        parameters => #{
            task => #{type => string, required => true,
                      description => <<"The subtask to hand to the sub-agent.">>},
            context => #{type => string,
                         description => <<"Relevant background for the subtask.">>}
        },
        handler => fun(Args, Ctx) ->
            SubConfig = ensure_isolated(SubagentFn(Args, Ctx)),
            Prompt = compose([SeedFn(Args, Ctx), arg(Args, <<"context">>)], arg(Args, <<"task">>)),
            sync_run(SubConfig, Prompt, ResultFn, Timeout, owner(Ctx))
        end
    }.

%% @doc 并发 fan-out 工具：一次调用传 tasks 列表 → 各子 agent 并发 → 汇总返回。
-spec fanout_tool(config()) -> map().
fanout_tool(Config) ->
    {SubagentFn, SeedFn, ResultFn, Timeout} = unpack(Config),
    #{
        name => maps:get(name, Config),
        description => maps:get(description, Config,
                                <<"Delegate multiple subtasks to sub-agents running concurrently.">>),
        parameters => #{
            tasks => #{type => array, items => #{type => string}, required => true,
                       description => <<"Subtasks to run concurrently, one sub-agent each.">>},
            context => #{type => string, description => <<"Shared background for all subtasks.">>}
        },
        handler => fun(Args, Ctx) ->
            case arg_list(Args, <<"tasks">>) of
                [] ->
                    {error, #{type => invalid_arguments,
                              message => <<"tasks must be a non-empty array of strings">>}};
                Tasks ->
                    Shared = [SeedFn(Args, Ctx), arg(Args, <<"context">>)],
                    Jobs = [{ensure_isolated(SubagentFn(Args#{<<"task">> => T}, Ctx)),
                             compose(Shared, T)} || T <- Tasks],
                    Results = run_many(Jobs, Timeout, ResultFn),
                    fanout_outcome(Tasks, Results)
            end
        end
    }.

%% @doc 程序化并发 fan-out：并发跑一批 {SubConfig, Prompt}，按序返回 outcome 列表。
-spec run_many([{map(), binary()}], timeout()) ->
    [beamai_subagent_manager:outcome() | {error, sub_agent_timeout}].
run_many(Jobs, Timeout) ->
    run_many(Jobs, Timeout, fun(#{content := C}) when is_binary(C) -> C; (_) -> <<>> end).

-spec run_many([{map(), binary()}], timeout(), fun((map()) -> binary())) ->
    [beamai_subagent_manager:outcome() | {error, sub_agent_timeout}].
run_many(Jobs, Timeout, ResultFn) ->
    _ = beamai_subagent_manager:ensure_started(),
    Ids = [begin
               {ok, Id} = beamai_subagent_manager:spawn(
                   #{subagent => C, prompt => P, result => ResultFn}),
               Id
           end || {C, P} <- Jobs],
    End = deadline(Timeout),
    [collect_one(Id, End) || Id <- Ids].

%%====================================================================
%% 异步管理工具（LLM 跨轮自管子 agent）
%%====================================================================

%% @doc 返回一组管理工具：spawn/list/result/kill/restart。按 conversation_id 归属隔离。
-spec management_tools(config()) -> [map()].
management_tools(Config) ->
    SubagentFn = maps:get(subagent, Config),
    SeedFn = maps:get(seed, Config, fun(_A, _C) -> <<>> end),
    ResultFn = maps:get(result, Config, fun(#{content := C}) when is_binary(C) -> C; (_) -> <<>> end),
    [
     %% 异步起一个子 agent，立即返回 id
     #{name => <<"spawn_subagent">>,
       description => <<"Start a sub-agent asynchronously; returns its id.">>,
       parameters => #{task => #{type => string, required => true,
                                 description => <<"The subtask.">>},
                       context => #{type => string, description => <<"Background.">>}},
       handler => fun(Args, Ctx) ->
           _ = beamai_subagent_manager:ensure_started(),
           SubConfig = ensure_isolated(SubagentFn(Args, Ctx)),
           Prompt = compose([SeedFn(Args, Ctx), arg(Args, <<"context">>)], arg(Args, <<"task">>)),
           {ok, Id} = beamai_subagent_manager:spawn(
               #{subagent => SubConfig, prompt => Prompt, result => ResultFn, owner => owner(Ctx)}),
           {ok, json(#{id => Id, status => running})}
       end},
     %% 列出本会话的子 agent
     #{name => <<"list_subagents">>,
       description => <<"List this agent's sub-agents and their status.">>,
       parameters => #{},
       handler => fun(_Args, Ctx) ->
           {ok, json(#{agents => beamai_subagent_manager:list(owner(Ctx))})}
       end},
     %% 取某子 agent 的结果
     #{name => <<"subagent_result">>,
       description => <<"Fetch a sub-agent's result by id (running => not_ready).">>,
       parameters => #{id => #{type => string, required => true}},
       handler => fun(Args, _Ctx) ->
           Id = arg(Args, <<"id">>),
           {ok, json(result_view(Id, beamai_subagent_manager:result(Id)))}
       end},
     %% kill 某子 agent
     #{name => <<"kill_subagent">>,
       description => <<"Kill a running sub-agent by id.">>,
       parameters => #{id => #{type => string, required => true}},
       handler => fun(Args, _Ctx) ->
           Id = arg(Args, <<"id">>),
           _ = beamai_subagent_manager:kill(Id),
           {ok, json(#{id => Id, killed => true})}
       end},
     %% 重启某子 agent
     #{name => <<"restart_subagent">>,
       description => <<"Restart a sub-agent by id (re-runs its task).">>,
       parameters => #{id => #{type => string, required => true}},
       handler => fun(Args, _Ctx) ->
           Id = arg(Args, <<"id">>),
           case beamai_subagent_manager:restart(Id) of
               {ok, Id} -> {ok, json(#{id => Id, status => running})};
               {error, Reason} -> {error, Reason}
           end
       end}
    ].

%%====================================================================
%% 内部
%%====================================================================

unpack(Config) ->
    {maps:get(subagent, Config),
     maps:get(seed, Config, fun(_A, _C) -> <<>> end),
     maps:get(result, Config, fun(#{content := C}) when is_binary(C) -> C; (_) -> <<>> end),
     maps:get(timeout, Config, ?DEFAULT_TIMEOUT)}.

%% @private 同步:起子 agent、等结果、回收;超时则 kill+drop
sync_run(SubConfig, Prompt, ResultFn, Timeout, Owner) ->
    _ = beamai_subagent_manager:ensure_started(),
    {ok, Id} = beamai_subagent_manager:spawn(
        #{subagent => SubConfig, prompt => Prompt, result => ResultFn, owner => Owner}),
    Outcome = case beamai_subagent_manager:await(Id, Timeout) of
        {error, timeout} ->
            _ = beamai_subagent_manager:kill(Id),
            {error, sub_agent_timeout};
        Other -> Other
    end,
    _ = beamai_subagent_manager:drop(Id),
    Outcome.

%% @private 等单个 Id(共享截止);超时 kill;最后 drop
collect_one(Id, End) ->
    Outcome = case beamai_subagent_manager:await(Id, remaining(End)) of
        {error, timeout} ->
            _ = beamai_subagent_manager:kill(Id),
            {error, sub_agent_timeout};
        Other -> Other
    end,
    _ = beamai_subagent_manager:drop(Id),
    Outcome.

%% @private 强制隔离默认值（caller 可覆盖）
ensure_isolated(Cfg) ->
    maps:merge(#{memory => false, conversation_id => beamai_id:gen_id(<<"sub">>)}, Cfg).

%% @private 把非空背景段与 task 拼成子 agent 输入（task 末尾）
compose(Parts0, Task) ->
    Parts = [P || P <- Parts0, is_binary(P), P =/= <<>>] ++ [Task],
    iolist_to_binary(lists:join(<<"\n\n">>, Parts)).

%% @private fan-out 结果裁决：全部失败时显式返回 error（区别于部分成功的正常汇总），
%% 让调用方/LLM 能区分"全失败"与"部分成功"并采取回退策略（Tasks 非空，入口已校验）
fanout_outcome(Tasks, Results) ->
    case lists:any(fun({ok, _}) -> true; (_) -> false end, Results) of
        true ->
            {ok, combine(Tasks, Results)};
        false ->
            {error, #{all_failed =>
                [#{task => T, error => to_msg(R)} || {T, {error, R}} <- lists:zip(Tasks, Results)]}}
    end.

%% @private 汇总 fan-out 结果为 JSON
combine(Tasks, Results) ->
    json([#{task => T, result => fmt_outcome(R)} || {T, R} <- lists:zip(Tasks, Results)]).

fmt_outcome({ok, Bin}) -> Bin;
fmt_outcome({error, Reason}) -> #{error => to_msg(Reason)}.

%% @private result 工具的可读视图
result_view(Id, {ok, {ok, Bin}}) -> #{id => Id, status => done, result => Bin};
result_view(Id, {ok, {error, R}}) -> #{id => Id, status => failed, error => to_msg(R)};
result_view(Id, {error, not_ready}) -> #{id => Id, status => running};
result_view(Id, {error, not_found}) -> #{id => Id, error => <<"not_found">>}.

owner(Ctx) -> beamai_context:conversation_id(Ctx).

deadline(infinity) -> infinity;
deadline(Ms) when is_integer(Ms) -> erlang:monotonic_time(millisecond) + Ms.

remaining(infinity) -> infinity;
remaining(End) -> max(0, End - erlang:monotonic_time(millisecond)).

json(Term) -> beamai_tool:encode_result(Term).

to_msg(R) when is_binary(R) -> R;
to_msg(R) when is_atom(R) -> atom_to_binary(R, utf8);
to_msg(R) -> iolist_to_binary(io_lib:format("~p", [R])).

arg(Args, Key) when is_binary(Key) ->
    AtomKey = binary_to_atom(Key, utf8),
    to_bin(maps:get(Key, Args, maps:get(AtomKey, Args, <<>>))).

arg_list(Args, Key) when is_binary(Key) ->
    AtomKey = binary_to_atom(Key, utf8),
    case maps:get(Key, Args, maps:get(AtomKey, Args, [])) of
        L when is_list(L) -> [to_bin(X) || X <- L];
        _ -> []
    end.

to_bin(B) when is_binary(B) -> B;
to_bin(undefined) -> <<>>;
to_bin(V) -> beamai_utils:to_binary(V).
