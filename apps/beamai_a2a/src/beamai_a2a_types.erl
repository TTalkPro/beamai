%%%-------------------------------------------------------------------
%%% @doc A2A 协议类型定义
%%%
%%% 定义 A2A 协议中使用的所有数据类型，包括：
%%% - Agent Card（代理卡片）
%%% - Skill（技能）
%%% - Task（任务）和状态
%%% - Message（消息）和 Part（内容部分）
%%% - Artifact（产出物）
%%%
%%% == 设计原则 ==
%%%
%%% 1. 内部使用 atom 作为 key（性能和模式匹配）
%%% 2. 外部 JSON 使用 binary key
%%% 3. 提供类型转换函数
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_types).

%% 类型导出
-export_type([
    %% Agent Card
    agent_card/0,
    skill/0,
    capability/0,
    provider/0,

    %% Task
    task/0,
    task_id/0,
    task_state/0,
    task_status/0,
    context_id/0,

    %% Message
    message/0,
    message_id/0,
    role/0,
    part/0,
    text_part/0,
    file_part/0,
    data_part/0,

    %% Artifact
    artifact/0,
    artifact_id/0
]).

%% API 导出
-export([
    %% Task 状态转换
    binary_to_task_state/1,
    task_state_to_binary/1,
    is_terminal_state/1,

    %% Role 转换
    binary_to_role/1,
    role_to_binary/1,

    %% Part kind 转换
    binary_to_part_kind/1,
    part_kind_to_binary/1,

    %% Push 事件转换（安全版本）
    binary_to_push_event/1,
    push_event_to_binary/1,
    is_valid_push_event/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

%% Agent Card 类型
-type agent_card() :: #{
    name := binary(),
    description := binary(),
    url := binary(),
    version => binary(),
    protocol_version => binary(),
    provider => provider(),
    capabilities => capability(),
    default_input_modes => [binary()],
    default_output_modes => [binary()],
    skills => [skill()],
    security_schemes => map(),
    security => [map()]
}.

%% 技能定义
-type skill() :: #{
    id := binary(),
    name := binary(),
    description := binary(),
    tags => [binary()],
    examples => [binary()],
    input_modes => [binary()],
    output_modes => [binary()]
}.

%% 能力定义
-type capability() :: #{
    streaming => boolean(),
    push_notifications => boolean(),
    state_transition_history => boolean(),
    extended_agent_card => boolean()
}.

%% 提供者信息
-type provider() :: #{
    organization := binary(),
    url => binary()
}.

%% Task 类型
-type task_id() :: binary().
-type context_id() :: binary().

-type task_state() :: submitted
                    | working
                    | input_required
                    | auth_required
                    | completed
                    | failed
                    | canceled
                    | rejected.

-type task_status() :: #{
    state := task_state(),
    message => message(),
    timestamp => non_neg_integer()
}.

-type task() :: #{
    id := task_id(),
    context_id => context_id(),
    status := task_status(),
    history => [task_status()],
    artifacts => [artifact()],
    metadata => map(),
    created_at => non_neg_integer(),
    updated_at => non_neg_integer()
}.

%% Message 类型
-type message_id() :: binary().
-type role() :: user | agent.

-type message() :: #{
    message_id => message_id(),
    role := role(),
    parts := [part()],
    metadata => map()
}.

%% Part 类型
-type text_part() :: #{
    kind := text,
    text := binary()
}.

-type file_part() :: #{
    kind := file,
    file := #{
        name := binary(),
        mime_type => binary(),
        bytes => binary(),
        uri => binary()
    }
}.

-type data_part() :: #{
    kind := data,
    data := map() | list()
}.

-type part() :: text_part() | file_part() | data_part().

%% Artifact 类型
-type artifact_id() :: binary().

-type artifact() :: #{
    artifact_id := artifact_id(),
    name := binary(),
    description => binary(),
    parts := [part()],
    metadata => map()
}.

%%====================================================================
%% Task 状态转换
%%====================================================================

%% Task 状态映射表
-define(TASK_STATES_MAP, #{
    <<"submitted">> => submitted,
    <<"working">> => working,
    <<"input-required">> => input_required,
    <<"auth-required">> => auth_required,
    <<"completed">> => completed,
    <<"failed">> => failed,
    <<"canceled">> => canceled,
    <<"rejected">> => rejected
}).

%% @doc 将 binary 转换为 task_state atom
-spec binary_to_task_state(binary()) -> task_state().
binary_to_task_state(Bin) ->
    maps:get(Bin, ?TASK_STATES_MAP, submitted).

%% @doc 将 task_state atom 转换为 binary
-spec task_state_to_binary(task_state()) -> binary().
task_state_to_binary(submitted) -> <<"submitted">>;
task_state_to_binary(working) -> <<"working">>;
task_state_to_binary(input_required) -> <<"input-required">>;
task_state_to_binary(auth_required) -> <<"auth-required">>;
task_state_to_binary(completed) -> <<"completed">>;
task_state_to_binary(failed) -> <<"failed">>;
task_state_to_binary(canceled) -> <<"canceled">>;
task_state_to_binary(rejected) -> <<"rejected">>;
task_state_to_binary(_) -> <<"submitted">>.

%% @doc 判断是否为终态
%%
%% 终态的任务不可重启，需要创建新任务。
-spec is_terminal_state(task_state()) -> boolean().
is_terminal_state(completed) -> true;
is_terminal_state(failed) -> true;
is_terminal_state(canceled) -> true;
is_terminal_state(rejected) -> true;
is_terminal_state(_) -> false.

%%====================================================================
%% Role 转换
%%====================================================================

%% @doc 将 binary 转换为 role atom
-spec binary_to_role(binary()) -> role().
binary_to_role(<<"user">>) -> user;
binary_to_role(<<"agent">>) -> agent;
binary_to_role(_) -> user.

%% @doc 将 role atom 转换为 binary
-spec role_to_binary(role()) -> binary().
role_to_binary(user) -> <<"user">>;
role_to_binary(agent) -> <<"agent">>;
role_to_binary(_) -> <<"user">>.

%%====================================================================
%% Part Kind 转换
%%====================================================================

%% @doc 将 binary 转换为 part kind atom
-spec binary_to_part_kind(binary()) -> atom().
binary_to_part_kind(<<"text">>) -> text;
binary_to_part_kind(<<"file">>) -> file;
binary_to_part_kind(<<"data">>) -> data;
binary_to_part_kind(_) -> text.

%% @doc 将 part kind atom 转换为 binary
-spec part_kind_to_binary(atom()) -> binary().
part_kind_to_binary(text) -> <<"text">>;
part_kind_to_binary(file) -> <<"file">>;
part_kind_to_binary(data) -> <<"data">>;
part_kind_to_binary(_) -> <<"text">>.

%%====================================================================
%% Push 事件转换（安全版本，防止 atom 表耗尽攻击）
%%====================================================================

%% Push 事件白名单 - 仅允许这些预定义的事件类型
%% 对应 task_state 的所有可能值
-define(VALID_PUSH_EVENTS, #{
    <<"submitted">> => submitted,
    <<"working">> => working,
    <<"input_required">> => input_required,
    <<"input-required">> => input_required,
    <<"auth_required">> => auth_required,
    <<"auth-required">> => auth_required,
    <<"completed">> => completed,
    <<"failed">> => failed,
    <<"canceled">> => canceled,
    <<"rejected">> => rejected,
    <<"all">> => all
}).

%% @doc 安全地将 binary 转换为 push 事件 atom
%%
%% 使用预定义白名单进行验证，防止恶意输入导致 atom 表耗尽攻击。
%% 未知事件返回 undefined，调用方应过滤掉无效事件。
%%
%% @param Bin 事件名称的 binary 表示
%% @returns 对应的 atom 或 undefined（未知事件）
-spec binary_to_push_event(binary()) -> atom() | undefined.
binary_to_push_event(Bin) when is_binary(Bin) ->
    maps:get(Bin, ?VALID_PUSH_EVENTS, undefined);
binary_to_push_event(_) ->
    undefined.

%% @doc 将 push 事件 atom 转换为 binary
-spec push_event_to_binary(atom()) -> binary().
push_event_to_binary(submitted) -> <<"submitted">>;
push_event_to_binary(working) -> <<"working">>;
push_event_to_binary(input_required) -> <<"input-required">>;
push_event_to_binary(auth_required) -> <<"auth-required">>;
push_event_to_binary(completed) -> <<"completed">>;
push_event_to_binary(failed) -> <<"failed">>;
push_event_to_binary(canceled) -> <<"canceled">>;
push_event_to_binary(rejected) -> <<"rejected">>;
push_event_to_binary(all) -> <<"all">>;
push_event_to_binary(_) -> <<"unknown">>.

%% @doc 检查是否为有效的 push 事件
%%
%% 用于验证事件名称是否在白名单中。
%%
%% @param Event 事件名称（binary 或 atom）
%% @returns true | false
-spec is_valid_push_event(binary() | atom()) -> boolean().
is_valid_push_event(Event) when is_binary(Event) ->
    maps:is_key(Event, ?VALID_PUSH_EVENTS);
is_valid_push_event(Event) when is_atom(Event) ->
    lists:member(Event, maps:values(?VALID_PUSH_EVENTS));
is_valid_push_event(_) ->
    false.
