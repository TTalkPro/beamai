%%%-------------------------------------------------------------------
%%% @doc 流程事件类型与路由
%%%
%%% 事件是步骤之间流转的标记 Map。
%%% 事件绑定定义事件如何路由到步骤输入。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_event).

-export([
    new/2,
    new/3,
    error_event/2,
    error_event/3,
    system_event/2,
    binding/3,
    binding/4,
    route/2
]).

-export_type([event/0, event_type/0, event_binding/0, delivery/0, transform_fun/0]).

-type event_type() :: public | internal | error | system.

-type event() :: #{
    '__process_event__' := true,
    id := binary(),
    name := atom(),
    type := event_type(),
    source := atom() | undefined,
    data := term(),
    timestamp := integer()
}.

-type transform_fun() :: fun((term()) -> term()) | undefined.

-type event_binding() :: #{
    '__event_binding__' := true,
    event_name := atom(),
    target_step := atom(),
    target_input := atom(),
    transform := transform_fun()
}.

-type delivery() :: {StepId :: atom(), InputName :: atom(), Data :: term()}.

%%====================================================================
%% API
%%====================================================================

%% @doc 创建公共事件
%%
%% @param Name 事件名称（atom）
%% @param Data 事件携带的数据
%% @returns 事件 Map
-spec new(atom(), term()) -> event().
new(Name, Data) ->
    new(Name, Data, #{}).

%% @doc 创建事件（带选项）
%%
%% @param Name 事件名称
%% @param Data 事件数据
%% @param Opts 选项（可指定 type 和 source）
%% @returns 事件 Map
-spec new(atom(), term(), map()) -> event().
new(Name, Data, Opts) ->
    #{
        '__process_event__' => true,
        id => beamai_id:gen_id(<<"evt">>),
        name => Name,
        type => maps:get(type, Opts, public),
        source => maps:get(source, Opts, undefined),
        data => Data,
        timestamp => erlang:system_time(millisecond)
    }.

%% @doc 创建错误事件
%%
%% @param Source 错误来源步骤
%% @param Reason 错误原因
%% @returns 错误事件 Map
-spec error_event(atom(), term()) -> event().
error_event(Source, Reason) ->
    error_event(Source, Reason, #{}).

%% @doc 创建错误事件（带选项）
-spec error_event(atom(), term(), map()) -> event().
error_event(Source, Reason, Opts) ->
    new(error, #{reason => Reason, source => Source},
        Opts#{type => error, source => Source}).

%% @doc 创建系统事件
%%
%% @param Name 事件名称
%% @param Data 事件数据
%% @returns 系统事件 Map
-spec system_event(atom(), term()) -> event().
system_event(Name, Data) ->
    new(Name, Data, #{type => system}).

%% @doc 创建事件绑定（无转换）
%%
%% @param EventName 要匹配的事件名称
%% @param TargetStep 目标步骤 ID
%% @param TargetInput 目标输入名称
%% @returns 事件绑定 Map
-spec binding(atom(), atom(), atom()) -> event_binding().
binding(EventName, TargetStep, TargetInput) ->
    binding(EventName, TargetStep, TargetInput, undefined).

%% @doc 创建事件绑定（带转换函数）
%%
%% @param EventName 要匹配的事件名称
%% @param TargetStep 目标步骤 ID
%% @param TargetInput 目标输入名称
%% @param Transform 数据转换函数（fun/1 或 undefined）
%% @returns 事件绑定 Map
-spec binding(atom(), atom(), atom(), transform_fun()) -> event_binding().
binding(EventName, TargetStep, TargetInput, Transform) ->
    #{
        '__event_binding__' => true,
        event_name => EventName,
        target_step => TargetStep,
        target_input => TargetInput,
        transform => Transform
    }.

%% @doc 路由事件到匹配的绑定，生成投递列表
%%
%% 遍历所有绑定，对事件名匹配的绑定生成 {StepId, InputName, Data} 投递项。
%% 若绑定包含转换函数则对数据进行转换。
%%
%% @param Event 待路由的事件
%% @param Bindings 事件绑定列表
%% @returns 投递列表
-spec route(event(), [event_binding()]) -> [delivery()].
route(#{name := EventName, data := Data}, Bindings) ->
    lists:filtermap(
        fun(#{event_name := BoundName, target_step := Step,
              target_input := Input, transform := Transform}) ->
            case BoundName =:= EventName of
                true ->
                    TransformedData = apply_transform(Transform, Data),
                    {true, {Step, Input, TransformedData}};
                false ->
                    false
            end
        end,
        Bindings
    ).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 应用转换函数（undefined 表示不转换）
apply_transform(undefined, Data) -> Data;
apply_transform(Fun, Data) when is_function(Fun, 1) -> Fun(Data).
