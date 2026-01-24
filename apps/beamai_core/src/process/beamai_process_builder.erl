%%%-------------------------------------------------------------------
%%% @doc 流程构建器 - 纯数据构造
%%%
%%% 构建可序列化、可检视的 process_spec() 流程定义。
%%% 在编译时验证步骤模块和绑定引用的合法性。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_builder).

-export([
    new/1,
    add_step/3,
    add_step/4,
    add_binding/2,
    add_initial_event/2,
    set_error_handler/2,
    set_execution_mode/2,
    compile/1
]).

-export_type([builder/0, process_spec/0, step_def/0]).

-type step_def() :: #{
    '__step_def__' := true,
    id := atom(),
    module := module(),
    config := map(),
    required_inputs := [atom()]
}.

-type error_handler_def() :: #{
    module := module(),
    config := map()
}.

-type builder() :: #{
    '__process_builder__' := true,
    name := atom(),
    steps := #{atom() => step_def()},
    bindings := [beamai_process_event:event_binding()],
    initial_events := [beamai_process_event:event()],
    error_handler := error_handler_def() | undefined,
    execution_mode := concurrent | sequential
}.

-type process_spec() :: #{
    '__process_spec__' := true,
    name := atom(),
    steps := #{atom() => step_def()},
    bindings := [beamai_process_event:event_binding()],
    initial_events := [beamai_process_event:event()],
    error_handler := error_handler_def() | undefined,
    execution_mode := concurrent | sequential
}.

%%====================================================================
%% API
%%====================================================================

%% @doc 创建新的流程构建器
%%
%% @param Name 流程名称（atom）
%% @returns 构建器 Map
-spec new(atom()) -> builder().
new(Name) when is_atom(Name) ->
    #{
        '__process_builder__' => true,
        name => Name,
        steps => #{},
        bindings => [],
        initial_events => [],
        error_handler => undefined,
        execution_mode => concurrent
    }.

%% @doc 添加步骤（使用默认配置）
%%
%% @param Builder 构建器
%% @param StepId 步骤标识（atom）
%% @param Module 步骤实现模块（需实现 init/1、can_activate/2、on_activate/3）
%% @returns 更新后的构建器
-spec add_step(builder(), atom(), module()) -> builder().
add_step(Builder, StepId, Module) ->
    add_step(Builder, StepId, Module, #{}).

%% @doc 添加步骤（使用自定义配置）
%%
%% @param Builder 构建器
%% @param StepId 步骤标识（atom）
%% @param Module 步骤实现模块
%% @param Config 步骤配置（可包含 required_inputs 等）
%% @returns 更新后的构建器
-spec add_step(builder(), atom(), module(), map()) -> builder().
add_step(#{steps := Steps} = Builder, StepId, Module, Config) when is_atom(StepId), is_atom(Module) ->
    StepDef = #{
        '__step_def__' => true,
        id => StepId,
        module => Module,
        config => Config,
        required_inputs => maps:get(required_inputs, Config, [input])
    },
    Builder#{steps => Steps#{StepId => StepDef}}.

%% @doc 添加事件绑定
%%
%% @param Builder 构建器
%% @param Binding 事件绑定定义（通过 beamai_process_event:binding/3,4 创建）
%% @returns 更新后的构建器
-spec add_binding(builder(), beamai_process_event:event_binding()) -> builder().
add_binding(#{bindings := Bindings} = Builder, Binding) ->
    Builder#{bindings => Bindings ++ [Binding]}.

%% @doc 添加初始事件（流程启动时自动触发）
%%
%% @param Builder 构建器
%% @param Event 初始事件
%% @returns 更新后的构建器
-spec add_initial_event(builder(), beamai_process_event:event()) -> builder().
add_initial_event(#{initial_events := Events} = Builder, Event) ->
    Builder#{initial_events => Events ++ [Event]}.

%% @doc 设置错误处理步骤
%%
%% @param Builder 构建器
%% @param Handler 错误处理器定义（包含 module 和 config）
%% @returns 更新后的构建器
-spec set_error_handler(builder(), error_handler_def()) -> builder().
set_error_handler(Builder, Handler) ->
    Builder#{error_handler => Handler}.

%% @doc 设置执行模式
%%
%% @param Builder 构建器
%% @param Mode concurrent（并发）或 sequential（顺序）
%% @returns 更新后的构建器
-spec set_execution_mode(builder(), concurrent | sequential) -> builder().
set_execution_mode(Builder, Mode) when Mode =:= concurrent; Mode =:= sequential ->
    Builder#{execution_mode => Mode}.

%% @doc 编译构建器为已验证的流程定义
%%
%% 验证所有步骤模块存在且实现必要回调，
%% 验证所有绑定的目标步骤已注册，
%% 验证错误处理模块存在。
%%
%% @param Builder 构建器
%% @returns {ok, 流程定义} | {error, 错误列表}
-spec compile(builder()) -> {ok, process_spec()} | {error, [term()]}.
compile(#{name := Name, steps := Steps, bindings := Bindings,
          initial_events := InitEvents, error_handler := ErrorHandler,
          execution_mode := ExecMode}) ->
    Errors = validate_steps(Steps) ++
             validate_bindings(Bindings, Steps) ++
             validate_error_handler(ErrorHandler),
    case Errors of
        [] ->
            {ok, #{
                '__process_spec__' => true,
                name => Name,
                steps => Steps,
                bindings => Bindings,
                initial_events => InitEvents,
                error_handler => ErrorHandler,
                execution_mode => ExecMode
            }};
        _ ->
            {error, Errors}
    end.

%%====================================================================
%% 内部函数 - 验证
%%====================================================================

%% @private 验证所有步骤模块的合法性
validate_steps(Steps) ->
    maps:fold(
        fun(StepId, #{module := Module}, Acc) ->
            case code:which(Module) of
                non_existing ->
                    [{step_module_not_found, StepId, Module} | Acc];
                _ ->
                    validate_step_callbacks(StepId, Module, Acc)
            end
        end,
        [],
        Steps
    ).

%% @private 验证步骤模块是否实现了必要的回调函数
validate_step_callbacks(StepId, Module, Acc) ->
    Exports = Module:module_info(exports),
    Required = [{init, 1}, {can_activate, 2}, {on_activate, 3}],
    lists:foldl(
        fun({Fun, Arity}, InnerAcc) ->
            case lists:member({Fun, Arity}, Exports) of
                true -> InnerAcc;
                false ->
                    [{missing_callback, StepId, Module, {Fun, Arity}} | InnerAcc]
            end
        end,
        Acc,
        Required
    ).

%% @private 验证所有绑定的目标步骤是否已注册
validate_bindings(Bindings, Steps) ->
    lists:foldl(
        fun(#{target_step := TargetStep}, Acc) ->
            case maps:is_key(TargetStep, Steps) of
                true -> Acc;
                false ->
                    [{binding_target_not_found, TargetStep} | Acc]
            end
        end,
        [],
        Bindings
    ).

%% @private 验证错误处理模块是否存在
validate_error_handler(undefined) -> [];
validate_error_handler(#{module := Module}) ->
    case code:which(Module) of
        non_existing ->
            [{error_handler_module_not_found, Module}];
        _ ->
            []
    end.
