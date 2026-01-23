%%%-------------------------------------------------------------------
%%% @doc Memory 重要性评分模块
%%%
%%% 提供多种策略评估 Memory 的重要性：
%%% - LLM 评估：使用大模型分析内容重要性
%%% - 关键词匹配：基于预设关键词评分
%%% - 情感分析：基于情感强度评分
%%% - 用户反馈：手动指定重要性
%%%
%%% == 使用场景 ==
%%%
%%% 1. **智能遗忘决策**：优先保留高重要性记忆
%%% 2. **语义搜索排序**：按重要性加权搜索结果
%%% 3. **Memory 优先级管理**：识别关键信息
%%%
%%% == 评分范围 ==
%%%
%%% 所有评分策略返回 0.0 - 1.0 之间的浮点数：
%%% - 0.0 - 0.3: 低重要性（可丢弃）
%%% - 0.3 - 0.7: 中等重要性（保留）
%%% - 0.7 - 1.0: 高重要性（必须保留）
%%%
%%% == 示例 ==
%%%
%%% ```
%%% %% LLM 评分
%%% Memory = #{content => <<"用户偏好：喜欢深色主题">>},
%%% {ok, Score} = beamai_memory_importance:calculate_importance(
%%%     Memory,
%%%     llm_eval,
%%%     #{llm_config => LLMConfig}
%%% ).
%%%
%%% %% 关键词评分
%%% {ok, Score} = beamai_memory_importance:calculate_importance(
%%%     Memory,
%%%     keyword_match,
%%%     #{keywords => [<<"密码">>, <<"密钥">>, <<"偏好">>]}
%%% ).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_importance).

-include_lib("beamai_memory/include/beamai_episodic_memory.hrl").

%%====================================================================
%%% API 导出
%%====================================================================

%% 评分策略
-export([
    calculate_importance/3,
    batch_calculate_importance/3,
    update_importance/3
]).

%% 预设关键词管理
-export([
    set_keywords/2,
    get_keywords/1,
    add_keywords/2,
    clear_keywords/1
]).

%%====================================================================
%%% 类型定义
%%====================================================================

%% 评分策略
-type score_strategy() ::
    llm_eval            %% LLM 评估：使用大模型分析重要性
    | keyword_match     %% 关键词匹配：基于关键词评分
    | sentiment         %% 情感分析：基于情感强度评分
    | user_feedback     %% 用户反馈：手动指定重要性
    | hybrid.           %% 混合策略：组合多种策略

%% 记忆数据
-type memory() :: #{
    id => binary(),
    content => binary(),
    metadata => map()
} | #episode{} | #event{}.

%% 评分选项
-type importance_opts() :: #{
    llm_config => map(),           %% LLM 配置（用于 llm_eval）
    keywords => [binary()],        %% 关键词列表（用于 keyword_match）
    keyword_weights => map(),      %% 关键词权重映射 #{<<"keyword">> => Weight}
    min_score => float(),          %% 最低分数（默认 0.0）
    max_score => float(),          %% 最高分数（默认 1.0）
    strategies => [score_strategy()] %% 混合策略列表
}.

%% 评分结果
-type importance_result() :: {ok, float()} | {error, term()}.

%%====================================================================
%%% API 函数
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 计算单个记忆的重要性评分
%%
%% 参数：
%% - Memory: 记忆数据（map 或 record）
%% - Strategy: 评分策略
%% - Opts: 评分选项
%%
%% 返回：{ok, Score} | {error, Reason}
%%
%% 示例：
%% ```
%% Memory = #{content => <<"用户设置了 API 密钥">>},
%% {ok, 0.9} = beamai_memory_importance:calculate_importance(
%%     Memory,
%%     keyword_match,
%%     #{keywords => [<<"密钥">>, <<"密码">>]}
%% ).
%% '''
%% @end
%%--------------------------------------------------------------------
-spec calculate_importance(memory(), score_strategy(), importance_opts()) ->
    importance_result().
calculate_importance(Memory, Strategy, Opts) ->
    try
        Score = case Strategy of
            llm_eval ->
                calculate_by_llm(Memory, Opts);
            keyword_match ->
                calculate_by_keyword(Memory, Opts);
            sentiment ->
                calculate_by_sentiment(Memory, Opts);
            user_feedback ->
                calculate_by_user_feedback(Memory, Opts);
            hybrid ->
                calculate_hybrid(Memory, Opts)
        end,
        {ok, normalize_score(Score, Opts)}
    catch
        Type:Error:Stack ->
            logger:error("计算重要性评分失败: ~p:~p~n~p", [Type, Error, Stack]),
            {error, {calculation_failed, {Type, Error}}}
    end.

%%--------------------------------------------------------------------
%% @doc 批量计算记忆的重要性评分
%%
%% 参数：
%% - Memories: 记忆列表
%% - Strategy: 评分策略
%% - Opts: 评分选项
%%
%% 返回：{ok, [Score]} | {error, Reason}
%%
%% 注意：批量处理时，LLM 评分会合并为单个请求以提高性能
%% @end
%%--------------------------------------------------------------------
-spec batch_calculate_importance([memory()], score_strategy(), importance_opts()) ->
    {ok, [float()]} | {error, term()}.
batch_calculate_importance(Memories, Strategy, Opts) ->
    try
        Scores = case Strategy of
            llm_eval ->
                %% LLM 批量评分：合并内容为单个请求
                batch_calculate_by_llm(Memories, Opts);
            keyword_match ->
                %% 关键词批量评分：逐个处理
                [calculate_by_keyword(M, Opts) || M <- Memories];
            sentiment ->
                %% 情感批量评分：逐个处理
                [calculate_by_sentiment(M, Opts) || M <- Memories];
            user_feedback ->
                %% 用户反馈批量评分：逐个处理
                [calculate_by_user_feedback(M, Opts) || M <- Memories];
            hybrid ->
                %% 混合策略批量评分
                [calculate_hybrid(M, Opts) || M <- Memories]
        end,
        NormalizedScores = [normalize_score(S, Opts) || S <- Scores],
        {ok, NormalizedScores}
    catch
        Type:Error:Stack ->
            logger:error("批量计算重要性评分失败: ~p:~p~n~p", [Type, Error, Stack]),
            {error, {batch_calculation_failed, {Type, Error}}}
    end.

%%--------------------------------------------------------------------
%% @doc 更新记忆的重要性评分
%%
%% 参数：
%% - Memory: 记忆数据
%% - NewScore: 新的重要性评分（0.0 - 1.0）
%% - Opts: 更新选项
%%
%% 返回：{ok, UpdatedMemory} | {error, Reason}
%%
%% 示例：
%% ```
%% %% 更新 episode 记录的重要性
%% {ok, UpdatedEpisode} = beamai_memory_importance:update_importance(
%%     Episode#episode{importance = 0.5},
%%     0.9,
%%     #{}
%% ).
%% '''
%% @end
%%--------------------------------------------------------------------
-spec update_importance(memory(), float(), map()) ->
    {ok, memory()} | {error, term()}.
update_importance(Memory, NewScore, _Opts) when is_map(Memory) ->
    %% map 类型：更新 metadata 中的 importance
    NormalizedScore = normalize_score(NewScore, #{}),
    UpdatedMemory = maps:put(importance, NormalizedScore, Memory),
    {ok, UpdatedMemory};

update_importance(#episode{} = Episode, NewScore, _Opts) ->
    %% episode 记录：更新 importance 字段
    NormalizedScore = normalize_score(NewScore, #{}),
    {ok, Episode#episode{importance = NormalizedScore}};

update_importance(#event{} = Event, NewScore, _Opts) ->
    %% event 记录：更新 importance 字段
    NormalizedScore = normalize_score(NewScore, #{}),
    {ok, Event#event{importance = NormalizedScore}};

update_importance(Memory, _NewScore, _Opts) ->
    {error, {unsupported_memory_type, Memory}}.

%%--------------------------------------------------------------------
%% @doc 设置预设关键词列表
%%
%% 用于集中管理关键词配置，避免每次调用时传递
%% @end
%%--------------------------------------------------------------------
-spec set_keywords(binary(), [binary()]) -> ok.
set_keywords(Category, Keywords) when is_binary(Category), is_list(Keywords) ->
    %% 存储到进程字典
    put({?MODULE, keywords, Category}, Keywords),
    ok.

%%--------------------------------------------------------------------
%% @doc 获取预设关键词列表
%% @end
%%--------------------------------------------------------------------
-spec get_keywords(binary()) -> [binary()].
get_keywords(Category) when is_binary(Category) ->
    case get({?MODULE, keywords, Category}) of
        undefined -> [];
        Keywords -> Keywords
    end.

%%--------------------------------------------------------------------
%% @doc 添加关键词到预设列表
%% @end
%%--------------------------------------------------------------------
-spec add_keywords(binary(), [binary()]) -> ok.
add_keywords(Category, NewKeywords) when is_binary(Category), is_list(NewKeywords) ->
    Existing = get_keywords(Category),
    Combined = lists:usort(Existing ++ NewKeywords),
    set_keywords(Category, Combined),
    ok.

%%--------------------------------------------------------------------
%% @doc 清空预设关键词列表
%% @end
%%--------------------------------------------------------------------
-spec clear_keywords(binary()) -> ok.
clear_keywords(Category) when is_binary(Category) ->
    erase({?MODULE, keywords, Category}),
    ok.

%%====================================================================
%%% 内部函数 - 评分策略实现
%%====================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc 使用 LLM 评估重要性
%%
%% 策略：
%% 1. 构造评分提示词
%% 2. 调用 LLM 获取评分和理由
%% 3. 解析评分结果（期望返回 JSON 格式）
%%
%% LLM 返回格式：
%% ```json
%% {
%%   "score": 0.85,
%%   "reason": "包含用户重要偏好设置"
%% }
%% ```
%%--------------------------------------------------------------------
calculate_by_llm(Memory, Opts) ->
    Content = extract_content(Memory),

    %% 构造评分提示词
    Prompt = build_llm_prompt(Content),

    %% 获取 LLM 配置
    LLMConfig = maps:get(llm_config, Opts, #{}),

    %% 调用 LLM（需要确保 beamai_chat_completion 可用）
    Messages = [#{role => user, content => Prompt}],
    case catch beamai_chat_completion:chat(LLMConfig, Messages) of
        {ok, #{content := Response}} when is_binary(Response) ->
            %% 解析 LLM 响应提取评分
            parse_llm_score(Response);
        {error, Reason} ->
            logger:warning("LLM 评分失败: ~p，使用默认评分", [Reason]),
            0.5;  %% 默认中等重要性
        _ ->
            logger:warning("LLM 评分异常，使用默认评分"),
            0.5
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 批量使用 LLM 评估重要性
%%
%% 优化：将多个记忆合并为单个请求，减少 LLM 调用次数
%%--------------------------------------------------------------------
batch_calculate_by_llm(Memories, Opts) ->
    %% 提取所有内容
    Contents = [{extract_id(M), extract_content(M)} || M <- Memories],

    %% 构造批量评分提示词
    Prompt = build_batch_llm_prompt(Contents),

    %% 获取 LLM 配置
    LLMConfig = maps:get(llm_config, Opts, #{}),

    %% 调用 LLM
    Messages = [#{role => user, content => Prompt}],
    case catch beamai_chat_completion:chat(LLMConfig, Messages) of
        {ok, #{content := Response}} when is_binary(Response) ->
            %% 解析批量评分结果
            parse_batch_llm_scores(Response, length(Memories));
        {error, Reason} ->
            logger:warning("LLM 批量评分失败: ~p，使用默认评分", [Reason]),
            [0.5 || _ <- Memories];
        _ ->
            [0.5 || _ <- Memories]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 基于关键词匹配评分
%%
%% 策略：
%% 1. 提取记忆内容
%% 2. 匹配关键词列表
%% 3. 根据匹配数量和权重计算评分
%%
%% 评分公式：
%% Score = base + (matched_count * weight) / total_keywords
%%
%% 示例：
%% - 关键词: [<<"密钥">>, <<"密码">>, <<"偏好">>]
%% - 内容: "用户设置了 API 密钥"
%% - 匹配: "密钥" (1个)
%% - 评分: 0.3 + 1 * 0.7 = 1.0
%%--------------------------------------------------------------------
calculate_by_keyword(Memory, Opts) ->
    Content = extract_content(Memory),
    Keywords = maps:get(keywords, Opts, []),
    Weights = maps:get(keyword_weights, Opts, #{}),

    %% 基础分数
    BaseScore = 0.3,

    %% 统计匹配的关键词
    MatchedKeywords = lists:filter(fun(Keyword) ->
        binary:match(Content, Keyword) =/= nomatch
    end, Keywords),

    %% 计算加权评分
    _MatchCount = length(MatchedKeywords),
    TotalKeywords = max(length(Keywords), 1),

    %% 计算权重加权和
    WeightSum = lists:foldl(fun(Keyword, Acc) ->
        Weight = maps:get(Keyword, Weights, 1.0),
        Acc + Weight
    end, 0.0, MatchedKeywords),

    %% 最终评分：基础分 + (权重和 / 总关键词数 * 0.7)
    Score = BaseScore + (WeightSum / TotalKeywords * 0.7),
    min(Score, 1.0).

%%--------------------------------------------------------------------
%% @private
%% @doc 基于情感分析评分
%%
%% 策略：
%% - 强烈情感（positive/negative）→ 高重要性（0.7+）
%% - 中性情感 → 中等重要性（0.4-0.6）
%% - 混合情感 → 低重要性（0.3-0.5）
%%
%% 注意：此功能需要集成情感分析库或 LLM
%%--------------------------------------------------------------------
calculate_by_sentiment(Memory, _Opts) ->
    Content = extract_content(Memory),

    %% 简单实现：基于情感关键词启发式评分
    %% 生产环境应使用专业的情感分析模型
    SentimentKeywords = #{
        positive => [<<"非常">>, <<"喜欢">>, <<"满意">>, <<"优秀">>, <<"感谢">>],
        negative => [<<"讨厌">>, <<"不满">>, <<"错误">>, <<"失败">>, <<"问题">>],
        urgent => [<<"紧急">>, <<"立即">>, <<"重要">>, <<"必须">>, <<"关键">>]
    },

    %% 检测情感强度
    Score = case detect_sentiment(Content, SentimentKeywords) of
        urgent -> 0.9;
        strong_positive -> 0.8;
        strong_negative -> 0.8;
        weak_positive -> 0.6;
        weak_negative -> 0.6;
        neutral -> 0.4;
        mixed -> 0.3
    end,

    Score.

%%--------------------------------------------------------------------
%% @private
%% @doc 基于用户反馈评分
%%
%% 直接从 metadata 中获取用户指定的重要性评分
%%--------------------------------------------------------------------
calculate_by_user_feedback(Memory, _Opts) ->
    case extract_importance_from_metadata(Memory) of
        {ok, Score} -> Score;
        error -> 0.5  %% 默认中等重要性
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc 混合评分策略
%%
%% 组合多种评分策略，计算加权平均
%%
%% 示例配置：
%% ```
%% Opts => #{
%%     strategies => [
%%         {llm_eval, 0.5},
%%         {keyword_match, 0.3},
%%         {sentiment, 0.2}
%%     ]
%% }
%% '''
%%--------------------------------------------------------------------
calculate_hybrid(Memory, Opts) ->
    Strategies = maps:get(strategies, Opts, [{keyword_match, 1.0}]),

    %% 计算各策略评分和权重
    {TotalScore, TotalWeight} = lists:foldl(fun
        ({Strategy, Weight}, {AccScore, AccWeight}) ->
            case calculate_importance(Memory, Strategy, Opts) of
                {ok, Score} ->
                    {AccScore + Score * Weight, AccWeight + Weight};
                {error, _} ->
                    %% 策略失败，跳过
                    {AccScore, AccWeight}
            end;
        (Strategy, {AccScore, AccWeight}) when is_atom(Strategy) ->
            %% 未指定权重，默认为 1.0
            case calculate_importance(Memory, Strategy, Opts) of
                {ok, Score} ->
                    {AccScore + Score * 1.0, AccWeight + 1.0};
                {error, _} ->
                    {AccScore, AccWeight}
            end
    end, {+0.0, +0.0}, Strategies),

    %% 计算加权平均
    case TotalWeight of
        +0.0 -> 0.5;  %% 所有策略失败，返回默认值
        _ -> TotalScore / TotalWeight
    end.

%%====================================================================
%%% 内部函数 - 工具函数
%%====================================================================

%%--------------------------------------------------------------------
%% @private 提取记忆内容
%%--------------------------------------------------------------------
extract_content(#{content := Content}) -> Content;
extract_content(#episode{summary = Summary}) ->
    case Summary of
        undefined -> <<"">>;
        _ -> Summary
    end;
extract_content(#event{description = Desc}) -> Desc;
extract_content(_) -> <<"">>.

%%--------------------------------------------------------------------
%% @private 提取记忆 ID
%%--------------------------------------------------------------------
extract_id(#{id := Id}) -> Id;
extract_id(#episode{id = Id}) -> Id;
extract_id(#event{id = Id}) -> Id;
extract_id(_) -> <<"unknown">>.

%%--------------------------------------------------------------------
%% @private 从 metadata 提取重要性评分
%%--------------------------------------------------------------------
extract_importance_from_metadata(#{metadata := Meta}) ->
    case maps:get(importance, Meta, undefined) of
        undefined -> error;
        Score when is_float(Score) -> {ok, Score};
        Score when is_integer(Score) -> {ok, Score * 1.0};
        _ -> error
    end;
extract_importance_from_metadata(_) ->
    error.

%%--------------------------------------------------------------------
%% @private 规范化评分到 [0.0, 1.0]
%%--------------------------------------------------------------------
normalize_score(Score, _Opts) when is_float(Score), Score >= 0.0, Score =< 1.0 ->
    Score;
normalize_score(Score, _Opts) when is_float(Score) ->
    max(0.0, min(1.0, Score));
normalize_score(Score, _Opts) when is_integer(Score) ->
    normalize_score(Score * 1.0, _Opts);
normalize_score(_Score, Opts) ->
    %% 使用选项中的默认值
    maps:get(default_score, Opts, 0.5).

%%--------------------------------------------------------------------
%% @private 构造 LLM 评分提示词
%%--------------------------------------------------------------------
build_llm_prompt(Content) ->
    <<
        "请评估以下内容的重要性（0.0-1.0）：\n\n",
        Content/binary,
        "\n\n请以 JSON 格式返回评分和理由：\n",
        "{\"score\": 0.85, \"reason\": \"理由\"}"
    >>.

%%--------------------------------------------------------------------
%% @private 构造批量 LLM 评分提示词
%%--------------------------------------------------------------------
build_batch_llm_prompt(Contents) ->
    IndexedContents = lists:map(fun({Id, Content}) ->
        io_lib:format("~s. ~s", [Id, Content])
    end, Contents),

    Prompt = io_lib:format(
        "请评估以下 ~p 条内容的重要性（0.0-1.0）：~n~n~s~n~n"
        "请以 JSON 数组格式返回评分：~n"
        "[0.85, 0.62, 0.45, ...]",
        [length(Contents), string:join(IndexedContents, "\n\n")]
    ),

    iolist_to_binary(Prompt).

%%--------------------------------------------------------------------
%% @private 解析 LLM 评分响应
%%--------------------------------------------------------------------
parse_llm_score(Response) ->
    %% 尝试从响应中提取 JSON 评分
    try
        %% 简化实现：查找 "score": 后面的数字
        case re:run(Response, "\"score\"\\s*:\\s*([0-9.]+)", [{capture, all_but_first, list}]) of
            {match, [ScoreStr]} ->
                Score = list_to_float(ScoreStr),
                normalize_score(Score, #{});
            nomatch ->
                logger:warning("无法解析 LLM 评分响应: ~p", [Response]),
                0.5
        end
    catch
        _:_ ->
            logger:warning("解析 LLM 评分失败: ~p", [Response]),
            0.5
    end.

%%--------------------------------------------------------------------
%% @private 解析批量 LLM 评分响应
%%--------------------------------------------------------------------
parse_batch_llm_scores(Response, Count) ->
    try
        %% 尝试解析 JSON 数组
        case re:run(Response, "\\[([0-9.,\\s]+)\\]", [{capture, all_but_first, list}]) of
            {match, [ScoresStr]} ->
                %% 分割并转换
                ScoreStrings = string:tokens(ScoresStr, ","),
                Scores = [list_to_float(string:trim(S)) || S <- ScoreStrings],

                %% 填充或截断到指定数量
                case length(Scores) of
                    N when N < Count ->
                        %% 不足，用默认值填充
                        Scores ++ [0.5 || _ <- lists:seq(1, Count - N)];
                    N when N > Count ->
                        %% 超出，截断
                        lists:sublist(Scores, Count);
                    _ ->
                        Scores
                end;
            nomatch ->
                [0.5 || _ <- lists:seq(1, Count)]
        end
    catch
        _:_ ->
            [0.5 || _ <- lists:seq(1, Count)]
    end.

%%--------------------------------------------------------------------
%% @private 简单情感检测
%%--------------------------------------------------------------------
detect_sentiment(Content, SentimentKeywords) ->
    %% 检测紧急关键词
    UrgentKeywords = maps:get(urgent, SentimentKeywords, []),
    UrgentMatches = [K || K <- UrgentKeywords, binary:match(Content, K) =/= nomatch],
    case UrgentMatches of
        [_|_] -> urgent;
        [] -> detect_other_sentiment(Content, SentimentKeywords)
    end.

%%--------------------------------------------------------------------
%% @private 检测其他情感类型
%%--------------------------------------------------------------------
detect_other_sentiment(Content, SentimentKeywords) ->
    PosKeywords = maps:get(positive, SentimentKeywords, []),
    NegKeywords = maps:get(negative, SentimentKeywords, []),

    PosMatches = [K || K <- PosKeywords, binary:match(Content, K) =/= nomatch],
    NegMatches = [K || K <- NegKeywords, binary:match(Content, K) =/= nomatch],

    case {PosMatches, NegMatches} of
        {[], []} -> neutral;
        {[_|_], []} ->
            case length(PosMatches) > 1 of
                true -> strong_positive;
                false -> weak_positive
            end;
        {[], [_|_]} ->
            case length(NegMatches) > 1 of
                true -> strong_negative;
                false -> weak_negative
            end;
        {_, _} -> mixed
    end.
