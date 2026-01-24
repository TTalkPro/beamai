%%%-------------------------------------------------------------------
%%% @doc beamai_memory_forgetting 模块单元测试
%%%
%%% 测试覆盖：
%%% - 基于重要性的遗忘策略
%%% - LRU 遗忘策略
%%% - 时间衰减遗忘策略
%%% - FIFO 遗忘策略
%%% - 混合遗忘策略
%%% - 时间衰减计算
%%% - 访问跟踪
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_forgetting_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamai_memory/include/beamai_episodic_memory.hrl").

%%====================================================================
%%% 测试函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 测试基于重要性的遗忘策略
%%--------------------------------------------------------------------
importance_forgetting_test() ->
    Now = erlang:system_time(millisecond),

    %% 创建测试记忆：不同重要性
    Memories = [
        #{id => <<"1">>, importance => 0.3, created_at => Now},
        #{id => <<"2">>, importance => 0.9, created_at => Now},
        #{id => <<"3">>, importance => 0.6, created_at => Now},
        #{id => <<"4">>, importance => 0.2, created_at => Now}
    ],

    %% 应用遗忘策略，保留前 2 个
    Filtered = beamai_memory_forgetting:forget(Memories, 2, importance),

    %% 验证：应该保留 importance=0.9 和 0.6 的记忆
    ?assertEqual(2, length(Filtered)),
    Ids = [maps:get(id, M) || M <- Filtered],
    ?assert(lists:member(<<"2">>, Ids)),  %% importance=0.9
    ?assert(lists:member(<<"3">>, Ids)),  %% importance=0.6
    ?assertNot(lists:member(<<"1">>, Ids)), %% importance=0.3
    ?assertNot(lists:member(<<"4">>, Ids)), %% importance=0.2

    ok.

%%--------------------------------------------------------------------
%% @doc 测试 LRU 遗忘策略
%%--------------------------------------------------------------------
lru_forgetting_test() ->
    Now = erlang:system_time(millisecond),
    OneHourAgo = Now - (60 * 60 * 1000),
    TwoHoursAgo = Now - (2 * 60 * 60 * 1000),

    %% 创建测试记忆：不同访问时间
    Memories = [
        #{id => <<"1">>, last_accessed_at => TwoHoursAgo, created_at => Now},
        #{id => <<"2">>, last_accessed_at => Now, created_at => Now},
        #{id => <<"3">>, last_accessed_at => OneHourAgo, created_at => Now}
    ],

    %% 应用 LRU 策略，保留前 2 个
    Filtered = beamai_memory_forgetting:forget(Memories, 2, lru),

    %% 验证：应该保留最近访问的记忆
    ?assertEqual(2, length(Filtered)),
    Ids = [maps:get(id, M) || M <- Filtered],
    ?assert(lists:member(<<"2">>, Ids)),  %% 最近访问
    ?assert(lists:member(<<"3">>, Ids)),  %% 1小时前访问
    ?assertNot(lists:member(<<"1">>, Ids)), %% 2小时前访问

    ok.

%%--------------------------------------------------------------------
%% @doc 测试时间衰减遗忘策略
%%--------------------------------------------------------------------
time_decay_forgetting_test() ->
    Now = erlang:system_time(millisecond),
    OneDayAgo = Now - (24 * 60 * 60 * 1000),
    TwoDaysAgo = Now - (2 * 24 * 60 * 60 * 1000),

    %% 创建测试记忆：不同创建时间
    Memories = [
        #{id => <<"1">>, created_at => TwoDaysAgo},
        #{id => <<"2">>, created_at => Now},
        #{id => <<"3">>, created_at => OneDayAgo}
    ],

    %% 应用时间衰减策略，保留前 2 个
    Filtered = beamai_memory_forgetting:forget(Memories, 2, time_decay),

    %% 验证：应该保留最新的记忆
    ?assertEqual(2, length(Filtered)),
    Ids = [maps:get(id, M) || M <- Filtered],
    ?assert(lists:member(<<"2">>, Ids)),  %% 最新
    ?assert(lists:member(<<"3">>, Ids)),  %% 1天前
    ?assertNot(lists:member(<<"1">>, Ids)), %% 2天前

    ok.

%%--------------------------------------------------------------------
%% @doc 测试 FIFO 遗忘策略
%%--------------------------------------------------------------------
fifo_forgetting_test() ->
    Now = erlang:system_time(millisecond),

    %% 创建测试记忆：不同创建时间
    Memories = [
        #{id => <<"1">>, created_at => Now - 3000},
        #{id => <<"2">>, created_at => Now - 2000},
        #{id => <<"3">>, created_at => Now - 1000},
        #{id => <<"4">>, created_at => Now}
    ],

    %% 应用 FIFO 策略，保留最后 2 个
    Filtered = beamai_memory_forgetting:forget(Memories, 2, fifo),

    %% 验证：应该保留最后加入的记忆
    ?assertEqual(2, length(Filtered)),
    Ids = [maps:get(id, M) || M <- Filtered],
    ?assert(lists:member(<<"3">>, Ids)),
    ?assert(lists:member(<<"4">>, Ids)),
    ?assertNot(lists:member(<<"1">>, Ids)),
    ?assertNot(lists:member(<<"2">>, Ids)),

    ok.

%%--------------------------------------------------------------------
%% @doc 测试混合遗忘策略
%%--------------------------------------------------------------------
hybrid_forgetting_test() ->
    Now = erlang:system_time(millisecond),

    %% 创建测试记忆：综合考虑重要性和时间
    Memories = [
        #{id => <<"1">>, importance => 0.9, created_at => Now - (2 * 60 * 60 * 1000), last_accessed_at => Now - (1 * 60 * 60 * 1000)},
        #{id => <<"2">>, importance => 0.5, created_at => Now, last_accessed_at => Now},
        #{id => <<"3">>, importance => 0.7, created_at => Now, last_accessed_at => Now},
        #{id => <<"4">>, importance => 0.2, created_at => Now, last_accessed_at => Now}
    ],

    %% 应用混合策略，保留前 2 个
    Filtered = beamai_memory_forgetting:forget(Memories, 2, hybrid),

    %% 验证：应该保留综合评分最高的记忆
    ?assertEqual(2, length(Filtered)),
    Ids = [maps:get(id, M) || M <- Filtered],
    %% M3 (0.7 + 新) 和 M1 (0.9，虽然稍旧但重要性高) 应该被保留
    %% 计算评分：
    %% M1: 0.6*0.9 + 0.3*0.5 + 0.1*0.92 = 0.54 + 0.15 + 0.092 = 0.782
    %% M3: 0.6*0.7 + 0.3*1 + 0.1*1 = 0.42 + 0.3 + 0.1 = 0.82
    %% M2: 0.6*0.5 + 0.3*1 + 0.1*1 = 0.3 + 0.3 + 0.1 = 0.7
    ?assert(lists:member(<<"3">>, Ids)),  %% 最高综合评分
    ?assert(lists:member(<<"1">>, Ids)),  %% 次高综合评分

    ok.

%%--------------------------------------------------------------------
%% @doc 测试时间衰减计算
%%--------------------------------------------------------------------
decay_importance_test() ->
    %% 测试 map 类型记忆
    Memory1 = #{importance => 0.9, content => <<"test">>},
    Decayed1 = beamai_memory_forgetting:decay_importance(Memory1, 10),
    NewImp1 = maps:get(importance, Decayed1),
    ?assert(NewImp1 < 0.9),  %% 应该衰减
    ?assert(NewImp1 > 0.0),   %% 但不会完全消失

    %% 测试 episode 记录
    Episode = #episode{importance = 0.8},
    DecayedEpisode = beamai_memory_forgetting:decay_importance(Episode, 10),
    ?assert(DecayedEpisode#episode.importance < 0.8),

    %% 测试 event 记录
    Event = #event{importance = 0.7},
    DecayedEvent = beamai_memory_forgetting:decay_importance(Event, 10),
    ?assert(DecayedEvent#event.importance < 0.7),

    %% 测试批量衰减
    Memories = [
        #{importance => 0.9},
        #{importance => 0.5}
    ],
    DecayedMemories = beamai_memory_forgetting:batch_decay_importance(Memories, 10),
    ?assertEqual(2, length(DecayedMemories)),
    [Imp1, Imp2] = [maps:get(importance, M) || M <- DecayedMemories],
    ?assert(Imp1 < 0.9),
    ?assert(Imp2 < 0.5),

    ok.

%%--------------------------------------------------------------------
%% @doc 测试访问跟踪
%%--------------------------------------------------------------------
access_tracking_test() ->
    MemoryId = <<"test_memory">>,

    %% 测试1：首次访问
    ok = beamai_memory_forgetting:record_access(MemoryId),
    Info1 = beamai_memory_forgetting:get_access_info(MemoryId),
    ?assertMatch(#{access_count := 1}, Info1),
    ?assert(maps:get(last_accessed_at, Info1) > 0),

    %% 测试2：多次访问
    timer:sleep(10),  %% 确保时间有差异
    ok = beamai_memory_forgetting:record_access(MemoryId),
    Info2 = beamai_memory_forgetting:get_access_info(MemoryId),
    ?assertEqual(2, maps:get(access_count, Info2)),

    %% 测试3：自定义时间戳
    CustomTime = 1234567890,
    ok = beamai_memory_forgetting:record_access(MemoryId, CustomTime),
    Info3 = beamai_memory_forgetting:get_access_info(MemoryId),
    ?assertEqual(CustomTime, maps:get(last_accessed_at, Info3)),

    %% 测试4：不存在的记忆
    ?assertEqual(undefined, beamai_memory_forgetting:get_access_info(<<"non_existent">>)),

    ok.

%%--------------------------------------------------------------------
%% @doc 测试未超过容量限制时不删除
%%--------------------------------------------------------------------
no_forget_when_under_limit_test() ->
    Memories = [
        #{id => <<"1">>, importance => 0.3},
        #{id => <<"2">>, importance => 0.5}
    ],

    %% 容量为 5，当前只有 2 个记忆
    Filtered = beamai_memory_forgetting:forget(Memories, 5, importance),

    %% 应该保留所有记忆
    ?assertEqual(2, length(Filtered)),
    ?assertEqual([<<"1">>, <<"2">>], [maps:get(id, M) || M <- Filtered]),

    ok.

%%--------------------------------------------------------------------
%% @doc 测试批量遗忘
%%--------------------------------------------------------------------
batch_forget_test() ->
    Now = erlang:system_time(millisecond),

    %% 创建多个记忆组
    Groups = #{
        <<"prefs">> => [
            #{id => <<"p1">>, importance => 0.3, created_at => Now},
            #{id => <<"p2">>, importance => 0.7, created_at => Now},
            #{id => <<"p3">>, importance => 0.5, created_at => Now}
        ],
        <<"history">> => [
            #{id => <<"h1">>, importance => 0.2, created_at => Now},
            #{id => <<"h2">>, importance => 0.8, created_at => Now},
            #{id => <<"h3">>, importance => 0.4, created_at => Now},
            #{id => <<"h4">>, importance => 0.6, created_at => Now}
        ]
    },

    %% 每个组应用不同容量限制
    MaxCounts = #{<<"prefs">> => 2, <<"history">> => 2},

    FilteredGroups = beamai_memory_forgetting:batch_forget(
        Groups,
        MaxCounts,
        importance,
        #{}
    ),

    %% 验证 prefs 组
    Prefs = maps:get(<<"prefs">>, FilteredGroups),
    ?assertEqual(2, length(Prefs)),
    PrefsIds = [maps:get(id, M) || M <- Prefs],
    ?assert(lists:member(<<"p2">>, PrefsIds)),  %% 0.7
    ?assert(lists:member(<<"p3">>, PrefsIds)),  %% 0.5

    %% 验证 history 组
    History = maps:get(<<"history">>, FilteredGroups),
    ?assertEqual(2, length(History)),
    HistoryIds = [maps:get(id, M) || M <- History],
    ?assert(lists:member(<<"h2">>, HistoryIds)), %% 0.8
    ?assert(lists:member(<<"h4">>, HistoryIds)), %% 0.6

    ok.

%%--------------------------------------------------------------------
%% @doc 测试 episode 和 event 记录的遗忘
%%--------------------------------------------------------------------
record_forgetting_test() ->
    Now = erlang:system_time(millisecond),

    %% 创建 episode 记录
    Episodes = [
        #episode{id = <<"ep1">>, importance = 0.3, created_at = Now},
        #episode{id = <<"ep2">>, importance = 0.8, created_at = Now},
        #episode{id = <<"ep3">>, importance = 0.5, created_at = Now}
    ],

    FilteredEpisodes = beamai_memory_forgetting:forget(Episodes, 2, importance),
    ?assertEqual(2, length(FilteredEpisodes)),
    ?assertEqual([<<"ep2">>, <<"ep3">>], [E#episode.id || E <- FilteredEpisodes]),

    %% 创建 event 记录
    Events = [
        #event{id = <<"ev1">>, importance = 0.2, timestamp = Now},
        #event{id = <<"ev2">>, importance = 0.9, timestamp = Now},
        #event{id = <<"ev3">>, importance = 0.6, timestamp = Now}
    ],

    FilteredEvents = beamai_memory_forgetting:forget(Events, 2, importance),
    ?assertEqual(2, length(FilteredEvents)),
    ?assertEqual([<<"ev2">>, <<"ev3">>], [E#event.id || E <- FilteredEvents]),

    ok.

%%--------------------------------------------------------------------
%% @doc 测试边界条件
%%--------------------------------------------------------------------
edge_cases_test() ->
    %% 测试空列表
    ?assertEqual([], beamai_memory_forgetting:forget([], 5, importance)),

    %% 测试单个记忆
    Single = [#{id => <<"1">>, importance => 0.5}],
    ?assertEqual(1, length(beamai_memory_forgetting:forget(Single, 5, importance))),

    %% 测试 MaxCount = 0
    Multiple = [
        #{id => <<"1">>, importance => 0.3},
        #{id => <<"2">>, importance => 0.7}
    ],
    ?assertEqual(0, length(beamai_memory_forgetting:forget(Multiple, 0, importance))),

    %% 测试 MaxCount = 1
    Filtered = beamai_memory_forgetting:forget(Multiple, 1, importance),
    ?assertEqual(1, length(Filtered)),
    ?assertEqual(<<"2">>, maps:get(id, hd(Filtered))),

    ok.
