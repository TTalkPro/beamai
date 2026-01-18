%%%-------------------------------------------------------------------
%%% @doc A2A JSON 转换工具模块
%%%
%%% 提供 A2A 协议中内部数据结构与 JSON 格式之间的转换功能。
%%% 此模块是公共工具模块，被 beamai_a2a_server 和 beamai_a2a_push 共同使用，
%%% 避免了代码重复。
%%%
%%% == 主要功能 ==
%%%
%%% 1. Task 转换
%%%    - task_to_json/1: 将 Task map 转换为 JSON 格式
%%%
%%% 2. 消息转换
%%%    - message_to_json/1: 将消息 map 转换为 JSON 格式
%%%    - normalize_message/1: 规范化输入消息格式
%%%
%%% 3. Artifact 转换
%%%    - artifact_to_json/1: 将 Artifact map 转换为 JSON 格式
%%%
%%% 4. Part 转换
%%%    - part_to_json/1: 将 Part map 转换为 JSON 格式
%%%    - normalize_part/1: 规范化输入 Part 格式
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 将内部 Task 转换为 JSON
%%% JsonTask = beamai_a2a_convert:task_to_json(InternalTask).
%%%
%%% %% 规范化输入消息
%%% NormalizedMsg = beamai_a2a_convert:normalize_message(InputMessage).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_convert).

%% API 导出
-export([
    %% Task 转换
    task_to_json/1,

    %% 消息转换
    message_to_json/1,
    normalize_message/1,

    %% Artifact 转换
    artifact_to_json/1,

    %% Part 转换
    part_to_json/1,
    normalize_part/1,

    %% 状态转换
    build_status_json/2,

    %% 历史条目转换
    history_entry_to_json/1,

    %% 消息 ID 生成
    generate_message_id/0
]).

%%====================================================================
%% Task 转换
%%====================================================================

%% @doc 将内部 Task map 转换为 JSON 格式
%%
%% 转换规则：
%% - id -> <<"id">>
%% - context_id -> <<"contextId">>
%% - status -> <<"status">> (通过 build_status_json 转换)
%% - artifacts -> <<"artifacts">> (列表转换)
%% - history -> <<"history">> (列表转换)
%% - messages -> <<"messages">> (可选，用于调试)
%%
%% @param Task 内部 Task map
%% @returns JSON 格式的 map（binary keys）
-spec task_to_json(map()) -> map().
task_to_json(Task) when is_map(Task) ->
    %% 提取状态信息
    Status = maps:get(status, Task, #{}),
    TaskState = maps:get(state, Status, submitted),
    StatusMessage = maps:get(message, Status, undefined),

    %% 构建基本 Task JSON
    BaseTask = #{
        <<"kind">> => <<"task">>,
        <<"id">> => maps:get(id, Task),
        <<"contextId">> => maps:get(context_id, Task),
        <<"status">> => build_status_json(TaskState, StatusMessage),
        <<"artifacts">> => [artifact_to_json(A) || A <- maps:get(artifacts, Task, [])],
        <<"history">> => [history_entry_to_json(H) || H <- maps:get(history, Task, [])]
    },

    %% 添加消息历史（用于调试和追踪）
    Messages = maps:get(messages, Task, []),
    case Messages of
        [] -> BaseTask;
        _ -> BaseTask#{<<"messages">> => [message_to_json(M) || M <- Messages]}
    end.

%%====================================================================
%% 状态转换
%%====================================================================

%% @doc 构建状态 JSON
%%
%% 根据状态和可选的状态消息构建 JSON 格式的状态对象。
%%
%% @param State 任务状态（atom）
%% @param Message 状态消息（可选，map 或 undefined）
%% @returns JSON 格式的状态 map
-spec build_status_json(atom(), map() | undefined) -> map().
build_status_json(State, undefined) ->
    #{
        <<"state">> => beamai_a2a_types:task_state_to_binary(State)
    };
build_status_json(State, Message) when is_map(Message) ->
    #{
        <<"state">> => beamai_a2a_types:task_state_to_binary(State),
        <<"message">> => message_to_json(Message)
    };
build_status_json(State, _) ->
    #{
        <<"state">> => beamai_a2a_types:task_state_to_binary(State)
    }.

%%====================================================================
%% 历史条目转换
%%====================================================================

%% @doc 将历史条目转换为 JSON 格式
%%
%% @param Entry 历史条目 map
%% @returns JSON 格式的历史条目 map
-spec history_entry_to_json(map()) -> map().
history_entry_to_json(#{state := State, timestamp := Timestamp}) ->
    #{
        <<"state">> => beamai_a2a_types:task_state_to_binary(State),
        <<"timestamp">> => Timestamp
    };
history_entry_to_json(Entry) when is_map(Entry) ->
    %% 已经是 JSON 格式或其他格式，原样返回
    Entry.

%%====================================================================
%% 消息转换
%%====================================================================

%% @doc 将内部消息 map 转换为 JSON 格式
%%
%% @param Msg 消息 map，包含 role 和 parts
%% @returns JSON 格式的消息 map
-spec message_to_json(map()) -> map().
message_to_json(#{role := Role, parts := Parts} = Msg) ->
    Base = #{
        <<"role">> => atom_to_binary(Role, utf8),
        <<"parts">> => [part_to_json(P) || P <- Parts]
    },
    %% 添加消息 ID（如果存在）
    case maps:get(message_id, Msg, undefined) of
        undefined -> Base;
        MsgId -> Base#{<<"messageId">> => MsgId}
    end;
message_to_json(Msg) when is_map(Msg) ->
    %% 已经是 JSON 格式或其他格式，原样返回
    Msg.

%% @doc 规范化输入消息格式
%%
%% 将各种输入格式的消息规范化为内部标准格式：
%% - #{<<"role">> := Role, <<"parts">> := Parts} -> 标准格式
%% - #{<<"text">> := Text} -> 简化格式，转换为标准格式
%%
%% @param Msg 输入消息 map
%% @returns 规范化后的消息 map（atom keys）
-spec normalize_message(map()) -> map().
normalize_message(#{<<"role">> := Role, <<"parts">> := Parts} = Msg) ->
    #{
        role => beamai_a2a_types:binary_to_role(Role),
        parts => [normalize_part(P) || P <- Parts],
        message_id => maps:get(<<"messageId">>, Msg, generate_message_id())
    };
normalize_message(#{<<"text">> := Text}) ->
    %% 简化格式：只有 text，默认为 user 角色
    #{
        role => user,
        parts => [#{kind => text, text => Text}],
        message_id => generate_message_id()
    };
normalize_message(Msg) when is_map(Msg) ->
    %% 已经是内部格式或无法识别，原样返回
    Msg.

%%====================================================================
%% Artifact 转换
%%====================================================================

%% @doc 将 Artifact 转换为 JSON 格式
%%
%% @param Artifact Artifact map
%% @returns JSON 格式的 Artifact map
-spec artifact_to_json(map()) -> map().
artifact_to_json(Artifact) when is_map(Artifact) ->
    #{
        <<"artifactId">> => maps:get(artifact_id, Artifact, <<>>),
        <<"name">> => maps:get(name, Artifact, <<>>),
        <<"parts">> => [part_to_json(P) || P <- maps:get(parts, Artifact, [])]
    }.

%%====================================================================
%% Part 转换
%%====================================================================

%% @doc 将 Part 转换为 JSON 格式
%%
%% 支持的 Part 类型：
%% - text: 文本内容
%% - file: 文件内容
%% - data: 结构化数据
%%
%% @param Part Part map
%% @returns JSON 格式的 Part map
-spec part_to_json(map()) -> map().
part_to_json(#{kind := text, text := Text}) ->
    #{<<"kind">> => <<"text">>, <<"text">> => Text};
part_to_json(#{kind := file, file := File}) ->
    #{<<"kind">> => <<"file">>, <<"file">> => File};
part_to_json(#{kind := data, data := Data}) ->
    #{<<"kind">> => <<"data">>, <<"data">> => Data};
part_to_json(Part) when is_map(Part) ->
    %% 未知类型，原样返回
    Part.

%% @doc 规范化输入 Part 格式
%%
%% 将 JSON 格式的 Part 转换为内部格式（atom keys）。
%%
%% @param Part JSON 格式的 Part map
%% @returns 内部格式的 Part map
-spec normalize_part(map()) -> map().
normalize_part(#{<<"kind">> := Kind} = Part) ->
    case Kind of
        <<"text">> ->
            #{kind => text, text => maps:get(<<"text">>, Part, <<>>)};
        <<"file">> ->
            #{kind => file, file => maps:get(<<"file">>, Part, #{})};
        <<"data">> ->
            #{kind => data, data => maps:get(<<"data">>, Part, #{})}
    end;
normalize_part(Part) when is_map(Part) ->
    %% 已经是内部格式，原样返回
    Part.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 生成唯一的消息 ID
%%
%% 格式: msg-XXXXXXXX（8 位十六进制随机数）
%%
%% @returns 消息 ID 二进制字符串
-spec generate_message_id() -> binary().
generate_message_id() ->
    Random = rand:uniform(16#FFFFFFFF),
    iolist_to_binary(io_lib:format("msg-~.8b", [Random])).
