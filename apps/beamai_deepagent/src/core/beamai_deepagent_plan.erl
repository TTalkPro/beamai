%%%-------------------------------------------------------------------
%%% @doc 深度 Agent 计划模块
%%%
%%% 提供计划创建和管理的纯函数。
%%% 计划由目标和多个步骤组成，支持步骤状态跟踪。
%%%
%%% 功能:
%%%   - 创建计划
%%%   - 更新步骤状态
%%%   - 检查完成状态
%%%   - 序列化/反序列化
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_plan).

-export([new/2, get_steps/1, current_step/1]).
-export([update_step/3, next_step/1, is_complete/1]).
-export([to_map/1, from_map/1]).
-export([format_status/1]).
-export([steps_to_maps/1]).

%% 计划记录
-record(plan, {
    goal    :: binary(),            %% 计划目标
    steps   :: [step()],            %% 步骤列表
    current :: pos_integer(),       %% 当前步骤索引
    status  :: plan_status()        %% 计划状态
}).

%% 步骤记录
-record(step, {
    id       :: pos_integer(),      %% 步骤 ID
    desc     :: binary(),           %% 步骤描述
    deps     :: [pos_integer()],    %% 依赖的步骤 ID
    status   :: step_status(),      %% 步骤状态
    result   :: binary() | undefined, %% 执行结果
    is_deep  :: boolean()           %% 是否需要深度处理
}).

%% 类型定义
-type plan_status() :: pending | in_progress | completed | failed.
-type step_status() :: pending | in_progress | completed | failed | skipped.
-opaque t() :: #plan{}.
-type step() :: #step{}.

-export_type([t/0, step/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 创建新计划
-spec new(binary(), [map()]) -> t().
new(Goal, StepDefs) ->
    Steps = [make_step(I, S) || {I, S} <- lists:zip(lists:seq(1, length(StepDefs)), StepDefs)],
    #plan{goal = Goal, steps = Steps, current = 1, status = in_progress}.

%% @doc 获取所有步骤
-spec get_steps(t()) -> [step()].
get_steps(#plan{steps = S}) -> S.

%% @doc 获取当前步骤索引
-spec current_step(t()) -> pos_integer().
current_step(#plan{current = C}) -> C.

%% @doc 更新步骤状态
-spec update_step(t(), pos_integer(), map()) -> t().
update_step(#plan{steps = Steps} = P, StepId, Updates) ->
    NewSteps = [update_single_step(S, StepId, Updates) || S <- Steps],
    check_completion(P#plan{steps = NewSteps}).

%% @doc 移动到下一步骤
-spec next_step(t()) -> t().
next_step(#plan{current = C, steps = S} = P) when C < length(S) ->
    P#plan{current = C + 1};
next_step(P) -> P.

%% @doc 检查计划是否完成
-spec is_complete(t()) -> boolean().
is_complete(#plan{status = completed}) -> true;
is_complete(_) -> false.

%% @doc 序列化为 map
-spec to_map(t()) -> map().
to_map(#plan{goal = G, steps = S, current = C, status = St}) ->
    #{goal => G, steps => [step_to_map(X) || X <- S], current => C, status => St}.

%% @doc 从 map 反序列化
-spec from_map(map()) -> t().
from_map(#{goal := G, steps := S, current := C, status := St}) ->
    Steps = [step_from_map(I, X) || {I, X} <- lists:zip(lists:seq(1, length(S)), S)],
    #plan{goal = G, steps = Steps, current = C, status = St}.

%% @doc 格式化计划状态
-spec format_status(t()) -> binary().
format_status(#plan{steps = Steps, current = Current}) ->
    Lines = [format_step(S) || S <- Steps],
    Header = <<"Plan (step ", (integer_to_binary(Current))/binary, "):\n">>,
    iolist_to_binary([Header | lists:join(<<"\n">>, Lines)]).

%% @doc 将计划步骤转换为 map 列表（供 dependencies:analyze 使用）
-spec steps_to_maps(t()) -> [map()].
steps_to_maps(#plan{steps = Steps}) ->
    [#{id => S#step.id,
       description => S#step.desc,
       dependencies => S#step.deps,
       is_deep => S#step.is_deep} || S <- Steps].

%%====================================================================
%% 内部函数 - 步骤创建
%%====================================================================

%% @private 创建步骤（binary keys from LLM JSON）
-spec make_step(pos_integer(), map()) -> step().
make_step(Id, #{<<"description">> := Desc} = M) ->
    #step{
        id = Id,
        desc = Desc,
        deps = maps:get(<<"dependencies">>, M, []),
        status = pending,
        result = undefined,
        is_deep = maps:get(<<"requires_deep">>, M, false)
    }.

%%====================================================================
%% 内部函数 - 步骤更新
%%====================================================================

%% @private 更新单个步骤
-spec update_single_step(step(), pos_integer(), map()) -> step().
update_single_step(#step{id = Id} = S, Id, Updates) ->
    Status = maps:get(status, Updates, S#step.status),
    Result = maps:get(result, Updates, S#step.result),
    S#step{status = Status, result = Result};
update_single_step(S, _Id, _Updates) -> S.

%% @private 检查计划是否全部完成
-spec check_completion(t()) -> t().
check_completion(#plan{steps = Steps} = P) ->
    AllDone = lists:all(fun(#step{status = S}) ->
        S =:= completed orelse S =:= skipped orelse S =:= failed
    end, Steps),
    case AllDone of
        true -> P#plan{status = completed};
        false -> P
    end.

%%====================================================================
%% 内部函数 - 序列化
%%====================================================================

%% @private 步骤转 map
-spec step_to_map(step()) -> map().
step_to_map(#step{id = I, desc = D, deps = Deps, status = S, result = R, is_deep = Deep}) ->
    Base = #{id => I, description => D, dependencies => Deps, status => S, is_deep => Deep},
    case R of
        undefined -> Base;
        _ -> Base#{result => R}
    end.

%% @private map 转步骤
-spec step_from_map(pos_integer(), map()) -> step().
step_from_map(Id, M) ->
    #step{
        id = Id,
        desc = maps:get(description, M, <<>>),
        deps = maps:get(dependencies, M, []),
        status = maps:get(status, M, pending),
        result = maps:get(result, M, undefined),
        is_deep = maps:get(is_deep, M, false)
    }.

%%====================================================================
%% 内部函数 - 格式化
%%====================================================================

%% @private 格式化步骤
-spec format_step(step()) -> binary().
format_step(#step{id = Id, desc = Desc, status = Status}) ->
    Mark = status_mark(Status),
    <<Mark/binary, " ", (integer_to_binary(Id))/binary, ". ", Desc/binary>>.

%% @private 状态标记
-spec status_mark(step_status()) -> binary().
status_mark(completed) -> <<"[✓]">>;
status_mark(failed) -> <<"[✗]">>;
status_mark(skipped) -> <<"[-]">>;
status_mark(in_progress) -> <<"[→]">>;
status_mark(pending) -> <<"[ ]">>.
