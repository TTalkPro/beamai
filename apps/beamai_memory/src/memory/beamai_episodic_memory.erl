%%%-------------------------------------------------------------------
%%% @doc Agent Episodic Memory - 情景记忆门面模块
%%%
%%% 情景记忆存储个人经历和事件，具有时间性和情境性。
%%% 包括三类数据：
%%% - 对话片段 (Episodes) - 完整对话的摘要
%%% - 交互事件 (Events) - 具体的交互记录
%%% - 经验总结 (Experiences) - 从多次交互中提炼的经验
%%%
%%% 本模块作为门面模块，委托具体操作给子模块：
%%% - beamai_episode_memory - 对话片段管理
%%% - beamai_event_memory - 交互事件管理
%%% - beamai_experience_memory - 经验总结管理
%%%
%%% == 使用示例 ==
%%%
%%% ```
%%% %% 创建 Memory 实例
%%% {ok, Mem} = beamai_memory:new(#{store => #{backend => ets}}),
%%%
%%% %% 创建对话片段
%%% {ok, Mem1} = beamai_episodic_memory:create_episode(Mem, UserId, #{
%%%     thread_id => <<"thread-1">>,
%%%     title => <<"讨论项目架构">>,
%%%     participants => [UserId, <<"agent-1">>]
%%% }),
%%%
%%% %% 记录事件
%%% {ok, Mem2} = beamai_episodic_memory:record_event(Mem1, UserId, #{
%%%     episode_id => <<"thread-1">>,
%%%     type => message,
%%%     description => <<"用户询问架构建议">>,
%%%     data => #{role => user, content => <<"...如何设计...">>}
%%% }),
%%%
%%% %% 添加经验
%%% {ok, Mem3} = beamai_episodic_memory:add_experience(Mem2, UserId, #{
%%%     type => lesson,
%%%     title => <<"用户偏好简洁回答">>,
%%%     content => <<"该用户倾向于简洁的技术回答，避免冗长解释">>,
%%%     source_episodes => [<<"thread-1">>]
%%% }),
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_episodic_memory).

-include_lib("beamai_memory/include/beamai_store.hrl").
-include_lib("beamai_memory/include/beamai_episodic_memory.hrl").

%% 类型别名
-type memory() :: beamai_memory:memory().
-type user_id() :: binary().
-type episode_id() :: binary().
-type event_id() :: binary().
-type experience_id() :: binary().

%% 类型导出
-export_type([user_id/0, episode_id/0, event_id/0, experience_id/0]).

%% 对话片段 API（委托给 beamai_episode_memory）
-export([
    create_episode/3,
    update_episode/4,
    get_episode/3,
    find_episodes/3,
    find_episodes/2,
    complete_episode/3,
    complete_episode/4,
    delete_episode/3
]).

%% 交互事件 API（委托给 beamai_event_memory）
-export([
    record_event/3,
    get_event/3,
    get_episode_events/3,
    get_episode_events/4,
    find_events/3,
    delete_event/3
]).

%% 经验总结 API（委托给 beamai_experience_memory）
-export([
    add_experience/3,
    get_experience/3,
    find_experiences/3,
    find_experiences/2,
    validate_experience/3,
    update_experience/4,
    delete_experience/3
]).

%% 高级查询 API（本模块实现）
-export([
    get_recent_episodes/2,
    get_recent_episodes/3,
    search_by_topic/3,
    get_timeline/3,
    get_timeline/4
]).

%% 工具函数（委托给子模块）
-export([
    get_episode_namespace/1,
    get_event_namespace/1,
    get_experience_namespace/1
]).

%%====================================================================
%% 对话片段 API（委托给 beamai_episode_memory）
%%====================================================================

%% @doc 创建对话片段
-spec create_episode(memory(), user_id(), map()) ->
    {ok, memory()} | {error, term()}.
create_episode(Memory, UserId, EpisodeData) ->
    beamai_episode_memory:create_episode(Memory, UserId, EpisodeData).

%% @doc 更新对话片段
-spec update_episode(memory(), user_id(), episode_id(), map()) ->
    {ok, memory()} | {error, term()}.
update_episode(Memory, UserId, EpisodeId, Updates) ->
    beamai_episode_memory:update_episode(Memory, UserId, EpisodeId, Updates).

%% @doc 获取对话片段
-spec get_episode(memory(), user_id(), episode_id()) ->
    {ok, #episode{}} | {error, not_found | term()}.
get_episode(Memory, UserId, EpisodeId) ->
    beamai_episode_memory:get_episode(Memory, UserId, EpisodeId).

%% @doc 查找对话片段
-spec find_episodes(memory(), user_id(), map()) ->
    {ok, [#episode{}]} | {error, term()}.
find_episodes(Memory, UserId, Opts) ->
    beamai_episode_memory:find_episodes(Memory, UserId, Opts).

%% @doc 获取所有对话片段
-spec find_episodes(memory(), user_id()) ->
    {ok, [#episode{}]} | {error, term()}.
find_episodes(Memory, UserId) ->
    beamai_episode_memory:find_episodes(Memory, UserId).

%% @doc 完成对话片段
-spec complete_episode(memory(), user_id(), episode_id()) ->
    {ok, memory()} | {error, term()}.
complete_episode(Memory, UserId, EpisodeId) ->
    beamai_episode_memory:complete_episode(Memory, UserId, EpisodeId).

-spec complete_episode(memory(), user_id(), episode_id(), map()) ->
    {ok, memory()} | {error, term()}.
complete_episode(Memory, UserId, EpisodeId, FinalData) ->
    beamai_episode_memory:complete_episode(Memory, UserId, EpisodeId, FinalData).

%% @doc 删除对话片段
-spec delete_episode(memory(), user_id(), episode_id()) ->
    {ok, memory()} | {error, term()}.
delete_episode(Memory, UserId, EpisodeId) ->
    beamai_episode_memory:delete_episode(Memory, UserId, EpisodeId).

%%====================================================================
%% 交互事件 API（委托给 beamai_event_memory）
%%====================================================================

%% @doc 记录交互事件
-spec record_event(memory(), user_id(), map()) ->
    {ok, memory()} | {error, term()}.
record_event(Memory, UserId, EventData) ->
    beamai_event_memory:record_event(Memory, UserId, EventData).

%% @doc 获取单个事件
-spec get_event(memory(), user_id(), event_id()) ->
    {ok, #event{}} | {error, not_found | term()}.
get_event(Memory, UserId, EventId) ->
    beamai_event_memory:get_event(Memory, UserId, EventId).

%% @doc 获取对话片段的所有事件
-spec get_episode_events(memory(), user_id(), episode_id()) ->
    {ok, [#event{}]} | {error, term()}.
get_episode_events(Memory, UserId, EpisodeId) ->
    beamai_event_memory:get_episode_events(Memory, UserId, EpisodeId).

-spec get_episode_events(memory(), user_id(), episode_id(), map()) ->
    {ok, [#event{}]} | {error, term()}.
get_episode_events(Memory, UserId, EpisodeId, Opts) ->
    beamai_event_memory:get_episode_events(Memory, UserId, EpisodeId, Opts).

%% @doc 查找事件
-spec find_events(memory(), user_id(), map()) ->
    {ok, [#event{}]} | {error, term()}.
find_events(Memory, UserId, Opts) ->
    beamai_event_memory:find_events(Memory, UserId, Opts).

%% @doc 删除事件
-spec delete_event(memory(), user_id(), event_id()) ->
    {ok, memory()} | {error, term()}.
delete_event(Memory, UserId, EventId) ->
    beamai_event_memory:delete_event(Memory, UserId, EventId).

%%====================================================================
%% 经验总结 API（委托给 beamai_experience_memory）
%%====================================================================

%% @doc 添加经验
-spec add_experience(memory(), user_id(), map()) ->
    {ok, memory()} | {error, term()}.
add_experience(Memory, UserId, ExperienceData) ->
    beamai_experience_memory:add_experience(Memory, UserId, ExperienceData).

%% @doc 获取经验
-spec get_experience(memory(), user_id(), experience_id()) ->
    {ok, #experience{}} | {error, not_found | term()}.
get_experience(Memory, UserId, ExperienceId) ->
    beamai_experience_memory:get_experience(Memory, UserId, ExperienceId).

%% @doc 查找经验
-spec find_experiences(memory(), user_id(), map()) ->
    {ok, [#experience{}]} | {error, term()}.
find_experiences(Memory, UserId, Opts) ->
    beamai_experience_memory:find_experiences(Memory, UserId, Opts).

%% @doc 获取所有经验
-spec find_experiences(memory(), user_id()) ->
    {ok, [#experience{}]} | {error, term()}.
find_experiences(Memory, UserId) ->
    beamai_experience_memory:find_experiences(Memory, UserId).

%% @doc 验证经验
-spec validate_experience(memory(), user_id(), experience_id()) ->
    {ok, memory()} | {error, term()}.
validate_experience(Memory, UserId, ExperienceId) ->
    beamai_experience_memory:validate_experience(Memory, UserId, ExperienceId).

%% @doc 更新经验
-spec update_experience(memory(), user_id(), experience_id(), map()) ->
    {ok, memory()} | {error, term()}.
update_experience(Memory, UserId, ExperienceId, Updates) ->
    beamai_experience_memory:update_experience(Memory, UserId, ExperienceId, Updates).

%% @doc 删除经验
-spec delete_experience(memory(), user_id(), experience_id()) ->
    {ok, memory()} | {error, term()}.
delete_experience(Memory, UserId, ExperienceId) ->
    beamai_experience_memory:delete_experience(Memory, UserId, ExperienceId).

%%====================================================================
%% 高级查询 API（本模块实现）
%%====================================================================

%% @doc 获取最近的对话片段
-spec get_recent_episodes(memory(), user_id()) ->
    {ok, [#episode{}]} | {error, term()}.
get_recent_episodes(Memory, UserId) ->
    get_recent_episodes(Memory, UserId, #{limit => ?DEFAULT_EPISODE_LIMIT}).

-spec get_recent_episodes(memory(), user_id(), map()) ->
    {ok, [#episode{}]} | {error, term()}.
get_recent_episodes(Memory, UserId, Opts) ->
    case beamai_episode_memory:find_episodes(Memory, UserId, Opts) of
        {ok, Episodes} ->
            %% 按开始时间降序排序
            Sorted = lists:sort(fun(A, B) ->
                A#episode.started_at > B#episode.started_at
            end, Episodes),
            Limit = maps:get(limit, Opts, ?DEFAULT_EPISODE_LIMIT),
            {ok, lists:sublist(Sorted, Limit)};
        {error, _} = Error ->
            Error
    end.

%% @doc 按主题搜索
%%
%% 搜索相关的对话片段和经验
-spec search_by_topic(memory(), user_id(), binary()) ->
    {ok, #{episodes := [#episode{}], experiences := [#experience{}]}} | {error, term()}.
search_by_topic(Memory, UserId, Topic) ->
    EpisodeResult = beamai_episode_memory:find_episodes(Memory, UserId, #{topic => Topic}),
    ExperienceResult = beamai_experience_memory:find_experiences(Memory, UserId, #{topic => Topic}),

    case {EpisodeResult, ExperienceResult} of
        {{ok, Episodes}, {ok, Experiences}} ->
            {ok, #{episodes => Episodes, experiences => Experiences}};
        {{error, _} = Error, _} ->
            Error;
        {_, {error, _} = Error} ->
            Error
    end.

%% @doc 获取时间线
%%
%% 返回指定时间范围内的所有事件，按时间排序。
-spec get_timeline(memory(), user_id(), map()) ->
    {ok, [#event{}]} | {error, term()}.
get_timeline(Memory, UserId, TimeRange) ->
    get_timeline(Memory, UserId, TimeRange, #{}).

-spec get_timeline(memory(), user_id(), map(), map()) ->
    {ok, [#event{}]} | {error, term()}.
get_timeline(Memory, UserId, TimeRange, Opts) ->
    case beamai_event_memory:find_events(Memory, UserId, Opts) of
        {ok, Events} ->
            Since = maps:get(since, TimeRange, 0),
            Until = maps:get(until, TimeRange, beamai_memory_utils:current_timestamp()),
            %% 过滤时间范围
            Filtered = [E || E <- Events,
                             E#event.timestamp >= Since,
                             E#event.timestamp =< Until],
            %% 按时间排序
            Sorted = lists:sort(fun(A, B) ->
                A#event.timestamp =< B#event.timestamp
            end, Filtered),
            {ok, Sorted};
        {error, _} = Error ->
            Error
    end.

%%====================================================================
%% 命名空间工具函数（委托给子模块）
%%====================================================================

%% @doc 获取用户对话片段命名空间
-spec get_episode_namespace(user_id()) -> [binary()].
get_episode_namespace(UserId) ->
    beamai_episode_memory:get_episode_namespace(UserId).

%% @doc 获取用户事件命名空间
-spec get_event_namespace(user_id()) -> [binary()].
get_event_namespace(UserId) ->
    beamai_event_memory:get_event_namespace(UserId).

%% @doc 获取用户经验命名空间
-spec get_experience_namespace(user_id()) -> [binary()].
get_experience_namespace(UserId) ->
    beamai_experience_memory:get_experience_namespace(UserId).
