%%%-------------------------------------------------------------------
%%% @doc beamai_memory_importance 模块单元测试
%%%
%%% 测试覆盖：
%%% - 关键词匹配评分
%%% - 情感分析评分
%%% - 用户反馈评分
%%% - 混合策略评分
%%% - 批量评分
%%% - 重要性更新
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_importance_test).

-include_lib("eunit/include/eunit.hrl").
-include_lib("beamai_memory/include/beamai_episodic_memory.hrl").

%%====================================================================
%%% 测试函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 测试关键词匹配评分
%%--------------------------------------------------------------------
keyword_match_test() ->
    %% 测试1：匹配单个关键词
    Memory1 = #{content => <<"用户设置了 API 密钥">>},
    Opts1 = #{keywords => [<<"密钥">>, <<"密码">>]},
    {ok, Score1} = beamai_memory_importance:calculate_importance(Memory1, keyword_match, Opts1),
    ?assert(Score1 > 0.5),  %% 应该有较高评分

    %% 测试2：匹配多个关键词
    Memory2 = #{content => <<"用户密码是 123456，这是重要密钥">>},
    Opts2 = #{keywords => [<<"密码">>, <<"密钥">>, <<"重要">>]},
    {ok, Score2} = beamai_memory_importance:calculate_importance(Memory2, keyword_match, Opts2),
    ?assert(Score2 > Score1),  %% 匹配更多关键词，评分更高

    %% 测试3：无匹配
    Memory3 = #{content => <<"今天天气不错">>},
    {ok, Score3} = beamai_memory_importance:calculate_importance(Memory3, keyword_match, Opts1),
    ?assert(Score3 < 0.5),  %% 无匹配，评分较低

    %% 测试4：带权重的关键词
    Opts4 = #{
        keywords => [<<"密钥">>, <<"密码">>],
        keyword_weights => #{<<"密钥">> => 2.0}
    },
    {ok, Score4} = beamai_memory_importance:calculate_importance(Memory1, keyword_match, Opts4),
    ?assert(Score4 > Score1),  %% 加权后评分更高

    ok.

%%--------------------------------------------------------------------
%% @doc 测试情感分析评分
%%--------------------------------------------------------------------
sentiment_test() ->
    %% 测试1：强烈积极情感
    Memory1 = #{content => <<"用户非常满意，非常喜欢这个功能！">>},
    {ok, Score1} = beamai_memory_importance:calculate_importance(Memory1, sentiment, #{}),
    ?assert(Score1 > 0.6),

    %% 测试2：强烈消极情感（修正：避免包含"非常"）
    Memory2 = #{content => <<"用户不满，讨厌这个错误！讨厌，失败！">>},
    {ok, Score2} = beamai_memory_importance:calculate_importance(Memory2, sentiment, #{}),
    ?assert(Score2 > 0.6),

    %% 测试3：紧急关键词
    Memory3 = #{content => <<"这是一个紧急问题，必须立即处理！">>},
    {ok, Score3} = beamai_memory_importance:calculate_importance(Memory3, sentiment, #{}),
    ?assert(Score3 > 0.8),  %% 紧急关键词应该有最高评分

    %% 测试4：中性情感
    Memory4 = #{content => <<"这是一个普通的对话">>},
    {ok, Score4} = beamai_memory_importance:calculate_importance(Memory4, sentiment, #{}),
    ?assert(Score4 < 0.6),

    ok.

%%--------------------------------------------------------------------
%% @doc 测试用户反馈评分
%%--------------------------------------------------------------------
user_feedback_test() ->
    %% 测试1：从 metadata 获取评分
    Memory1 = #{
        content => <<"普通内容">>,
        metadata => #{importance => 0.9}
    },
    {ok, Score1} = beamai_memory_importance:calculate_importance(Memory1, user_feedback, #{}),
    ?assertEqual(0.9, Score1),

    %% 测试2：无 metadata，返回默认值
    Memory2 = #{content => <<"普通内容">>},
    {ok, Score2} = beamai_memory_importance:calculate_importance(Memory2, user_feedback, #{}),
    ?assertEqual(0.5, Score2),

    ok.

%%--------------------------------------------------------------------
%% @doc 测试混合策略评分
%%--------------------------------------------------------------------
hybrid_test() ->
    %% 测试1：组合关键词和情感分析
    Memory = #{content => <<"用户紧急设置了 API 密钥！">>},
    Opts = #{
        strategies => [
            {keyword_match, 0.6},
            {sentiment, 0.4}
        ],
        keywords => [<<"密钥">>, <<"紧急">>]
    },
    {ok, Score} = beamai_memory_importance:calculate_importance(Memory, hybrid, Opts),
    %% 关键词匹配（高）+ 紧急情感（高）= 综合高评分
    ?assert(Score > 0.6),

    ok.

%%--------------------------------------------------------------------
%% @doc 测试批量评分
%%--------------------------------------------------------------------
batch_calculate_test() ->
    %% 测试批量关键词评分
    Memories = [
        #{id => <<"1">>, content => <<"包含密钥">>},
        #{id => <<"2">>, content => <<"普通内容">>},
        #{id => <<"3">>, content => <<"包含密码">>}
    ],
    Opts = #{keywords => [<<"密钥">>, <<"密码">>]},
    {ok, Scores} = beamai_memory_importance:batch_calculate_importance(Memories, keyword_match, Opts),
    ?assertEqual(3, length(Scores)),
    ?assert(lists:nth(1, Scores) > 0.5),  %% 包含密钥
    ?assert(lists:nth(2, Scores) < 0.5),  %% 普通
    ?assert(lists:nth(3, Scores) > 0.5),  %% 包含密码

    ok.

%%--------------------------------------------------------------------
%% @doc 测试重要性更新
%%--------------------------------------------------------------------
update_importance_test() ->
    %% 测试1：更新 map 类型记忆
    Memory1 = #{content => <<"test">>},
    {ok, Updated1} = beamai_memory_importance:update_importance(Memory1, 0.9, #{}),
    ?assertEqual(0.9, maps:get(importance, Updated1)),

    %% 测试2：更新 episode 记录
    Episode = #episode{
        id = <<"ep1">>,
        importance = 0.5
    },
    {ok, UpdatedEpisode} = beamai_memory_importance:update_importance(Episode, 0.8, #{}),
    ?assertEqual(0.8, UpdatedEpisode#episode.importance),

    %% 测试3：更新 event 记录
    Event = #event{
        id = <<"ev1">>,
        importance = 0.3
    },
    {ok, UpdatedEvent} = beamai_memory_importance:update_importance(Event, 0.7, #{}),
    ?assertEqual(0.7, UpdatedEvent#event.importance),

    ok.

%%--------------------------------------------------------------------
%% @doc 测试评分规范化
%%--------------------------------------------------------------------
normalize_score_test() ->
    %% 测试超出范围的评分
    {ok, Score1} = beamai_memory_importance:calculate_importance(
        #{content => <<"test">>},
        user_feedback,
        #{default_score => 1.5}
    ),
    ?assert(Score1 =< 1.0),  %% 应该被截断到 1.0

    {ok, Score2} = beamai_memory_importance:calculate_importance(
        #{content => <<"test">>},
        user_feedback,
        #{default_score => -0.5}
    ),
    ?assert(Score2 >= 0.0),  %% 应该被截断到 0.0

    ok.

%%--------------------------------------------------------------------
%% @doc 测试关键词管理
%%--------------------------------------------------------------------
keywords_management_test() ->
    Category = <<"test_category">>,

    %% 测试设置和获取关键词
    Keywords = [<<"密钥">>, <<"密码">>, <<"偏好">>],
    ok = beamai_memory_importance:set_keywords(Category, Keywords),
    ?assertEqual(Keywords, beamai_memory_importance:get_keywords(Category)),

    %% 测试添加关键词
    ok = beamai_memory_importance:add_keywords(Category, [<<"重要">>]),
    Updated = beamai_memory_importance:get_keywords(Category),
    ?assertEqual(4, length(Updated)),
    ?assert(lists:member(<<"重要">>, Updated)),

    %% 测试清空关键词
    ok = beamai_memory_importance:clear_keywords(Category),
    ?assertEqual([], beamai_memory_importance:get_keywords(Category)),

    ok.
