%%%-------------------------------------------------------------------
%%% @doc Memory 模块类型转换工具
%%%
%%% 提供安全的 binary ↔ atom 转换，避免 atom 泄漏。
%%%
%%% == 设计原则 ==
%%%
%%% 使用混合方案：
%%% - 内部使用 atom：保持代码清晰和性能
%%% - 外部使用 binary：避免 atom 泄漏
%%% - 边界转换：使用映射表，永不调用 binary_to_atom
%%%
%%% == 为什么安全 ==
%%%
%%% 传统的 binary_to_atom 会动态创建 atom，可能导致 atom 表溢出。
%%% 本模块使用映射表（map）进行转换：
%%% - binary → atom: maps:get(Bin, TypeMap, Default)
%%% - atom → binary: 直接映射或函数匹配
%%%
%%% 这样永远不会创建新的 atom，完全安全。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_memory_types).

%% API 导出
-export([
    %% Entity 类型
    binary_to_entity_type/1,
    entity_type_to_binary/1,

    %% Knowledge 类型
    binary_to_knowledge_type/1,
    knowledge_type_to_binary/1,

    %% Source
    binary_to_source/1,
    source_to_binary/1,

    %% Episode sentiment
    binary_to_sentiment/1,
    sentiment_to_binary/1,

    %% Episode outcome
    binary_to_outcome/1,
    outcome_to_binary/1,

    %% Event type
    binary_to_event_type/1,
    event_type_to_binary/1,

    %% Skill level
    binary_to_skill_level/1,
    skill_level_to_binary/1,

    %% Skill status
    binary_to_skill_status/1,
    skill_status_to_binary/1,

    %% Skill source
    binary_to_skill_source/1,
    skill_source_to_binary/1
]).

%%====================================================================
%% Entity 类型转换
%%====================================================================

%% Entity 类型映射表
-define(ENTITY_TYPES_MAP, #{
    <<"person">> => person,
    <<"organization">> => organization,
    <<"location">> => location,
    <<"product">> => product,
    <<"concept">> => concept,
    <<"event">> => event,
    <<"custom">> => custom
}).

%% @doc 将 binary 转换为 entity_type atom
%%
%% 使用映射表，永不失败。未知类型返回 default。
-spec binary_to_entity_type(binary()) -> atom().
binary_to_entity_type(Bin) ->
    maps:get(Bin, ?ENTITY_TYPES_MAP, custom).

%% @doc 将 entity_type atom 转换为 binary
-spec entity_type_to_binary(atom()) -> binary().
entity_type_to_binary(person) -> <<"person">>;
entity_type_to_binary(organization) -> <<"organization">>;
entity_type_to_binary(location) -> <<"location">>;
entity_type_to_binary(product) -> <<"product">>;
entity_type_to_binary(concept) -> <<"concept">>;
entity_type_to_binary(event) -> <<"event">>;
entity_type_to_binary(custom) -> <<"custom">>;
entity_type_to_binary(_) -> <<"custom">>.

%%====================================================================
%% Knowledge 类型转换
%%====================================================================

%% Knowledge 类型映射表
-define(KNOWLEDGE_TYPES_MAP, #{
    <<"fact">> => fact,
    <<"rule">> => rule,
    <<"definition">> => definition,
    <<"relation">> => relation,
    <<"custom">> => custom
}).

%% @doc 将 binary 转换为 knowledge_type atom
-spec binary_to_knowledge_type(binary()) -> atom().
binary_to_knowledge_type(Bin) ->
    maps:get(Bin, ?KNOWLEDGE_TYPES_MAP, custom).

%% @doc 将 knowledge_type atom 转换为 binary
-spec knowledge_type_to_binary(atom()) -> binary().
knowledge_type_to_binary(fact) -> <<"fact">>;
knowledge_type_to_binary(rule) -> <<"rule">>;
knowledge_type_to_binary(definition) -> <<"definition">>;
knowledge_type_to_binary(relation) -> <<"relation">>;
knowledge_type_to_binary(custom) -> <<"custom">>;
knowledge_type_to_binary(_) -> <<"custom">>.

%%====================================================================
%% Source 转换
%%====================================================================

%% Source 映射表
-define(SOURCE_MAP, #{
    <<"explicit">> => explicit,
    <<"inferred">> => inferred,
    <<"extracted">> => extracted,
    <<"default">> => default
}).

%% @doc 将 binary 转换为 source atom
-spec binary_to_source(binary()) -> atom().
binary_to_source(Bin) ->
    maps:get(Bin, ?SOURCE_MAP, explicit).

%% @doc 将 source atom 转换为 binary
-spec source_to_binary(atom()) -> binary().
source_to_binary(explicit) -> <<"explicit">>;
source_to_binary(inferred) -> <<"inferred">>;
source_to_binary(extracted) -> <<"extracted">>;
source_to_binary(default) -> <<"default">>;
source_to_binary(_) -> <<"explicit">>.

%%====================================================================
%% Episode Sentiment 转换
%%====================================================================

%% Sentiment 映射表
-define(SENTIMENT_MAP, #{
    <<"positive">> => positive,
    <<"negative">> => negative,
    <<"neutral">> => neutral,
    <<"mixed">> => mixed
}).

%% @doc 将 binary 转换为 sentiment atom
-spec binary_to_sentiment(binary()) -> atom() | undefined.
binary_to_sentiment(<<>>) -> undefined;
binary_to_sentiment(Bin) ->
    maps:get(Bin, ?SENTIMENT_MAP, neutral).

%% @doc 将 sentiment atom 转换为 binary
-spec sentiment_to_binary(atom()) -> binary() | undefined.
sentiment_to_binary(undefined) -> undefined;
sentiment_to_binary(positive) -> <<"positive">>;
sentiment_to_binary(negative) -> <<"negative">>;
sentiment_to_binary(neutral) -> <<"neutral">>;
sentiment_to_binary(mixed) -> <<"mixed">>;
sentiment_to_binary(_) -> undefined.

%%====================================================================
%% Episode Outcome 转换
%%====================================================================

%% Outcome 映射表
-define(OUTCOME_MAP, #{
    <<"completed">> => completed,
    <<"abandoned">> => abandoned,
    <<"ongoing">> => ongoing
}).

%% @doc 将 binary 转换为 outcome atom
-spec binary_to_outcome(binary()) -> atom().
binary_to_outcome(Bin) ->
    maps:get(Bin, ?OUTCOME_MAP, ongoing).

%% @doc 将 outcome atom 转换为 binary
-spec outcome_to_binary(atom()) -> binary().
outcome_to_binary(completed) -> <<"completed">>;
outcome_to_binary(abandoned) -> <<"abandoned">>;
outcome_to_binary(ongoing) -> <<"ongoing">>;
outcome_to_binary(_) -> <<"ongoing">>.

%%====================================================================
%% Event Type 转换
%%====================================================================

%% Event type 映射表
-define(EVENT_TYPES_MAP, #{
    <<"message">> => message,
    <<"tool_call">> => tool_call,
    <<"error">> => error,
    <<"milestone">> => milestone,
    <<"custom">> => custom
}).

%% @doc 将 binary 转换为 event_type atom
-spec binary_to_event_type(binary()) -> atom().
binary_to_event_type(Bin) ->
    maps:get(Bin, ?EVENT_TYPES_MAP, custom).

%% @doc 将 event_type atom 转换为 binary
-spec event_type_to_binary(atom()) -> binary().
event_type_to_binary(message) -> <<"message">>;
event_type_to_binary(tool_call) -> <<"tool_call">>;
event_type_to_binary(error) -> <<"error">>;
event_type_to_binary(milestone) -> <<"milestone">>;
event_type_to_binary(custom) -> <<"custom">>;
event_type_to_binary(_) -> <<"custom">>.

%%====================================================================
%% Skill Level 转换
%%====================================================================

%% Skill level 映射表
-define(SKILL_LEVELS_MAP, #{
    <<"novice">> => novice,
    <<"beginner">> => beginner,
    <<"intermediate">> => intermediate,
    <<"advanced">> => advanced,
    <<"expert">> => expert
}).

%% @doc 将 binary 转换为 skill_level atom
-spec binary_to_skill_level(binary()) -> atom().
binary_to_skill_level(Bin) ->
    maps:get(Bin, ?SKILL_LEVELS_MAP, beginner).

%% @doc 将 skill_level atom 转换为 binary
-spec skill_level_to_binary(atom()) -> binary().
skill_level_to_binary(novice) -> <<"novice">>;
skill_level_to_binary(beginner) -> <<"beginner">>;
skill_level_to_binary(intermediate) -> <<"intermediate">>;
skill_level_to_binary(advanced) -> <<"advanced">>;
skill_level_to_binary(expert) -> <<"expert">>;
skill_level_to_binary(_) -> <<"beginner">>.

%%====================================================================
%% Skill Status 转换
%%====================================================================

%% @doc 将 binary 转换为 skill_status atom
%% 支持两种状态集：通用状态（learning/practicing/mastered/archived）
%% 和 beamai_skill_memory 状态（learning/proficient/expert/deprecated）
-spec binary_to_skill_status(binary()) -> atom().
binary_to_skill_status(<<"learning">>) -> learning;
binary_to_skill_status(<<"practicing">>) -> practicing;
binary_to_skill_status(<<"mastered">>) -> mastered;
binary_to_skill_status(<<"archived">>) -> archived;
binary_to_skill_status(<<"proficient">>) -> proficient;
binary_to_skill_status(<<"expert">>) -> expert;
binary_to_skill_status(<<"deprecated">>) -> deprecated;
binary_to_skill_status(_) -> learning.

%% @doc 将 skill_status atom 转换为 binary
-spec skill_status_to_binary(atom()) -> binary().
skill_status_to_binary(learning) -> <<"learning">>;
skill_status_to_binary(practicing) -> <<"practicing">>;
skill_status_to_binary(mastered) -> <<"mastered">>;
skill_status_to_binary(archived) -> <<"archived">>;
skill_status_to_binary(proficient) -> <<"proficient">>;
skill_status_to_binary(expert) -> <<"expert">>;
skill_status_to_binary(deprecated) -> <<"deprecated">>;
skill_status_to_binary(_) -> <<"learning">>.

%%====================================================================
%% Skill Source 转换
%%====================================================================

%% @doc 将 binary 转换为 skill_source atom
-spec binary_to_skill_source(binary()) -> atom().
binary_to_skill_source(<<"learned">>) -> learned;
binary_to_skill_source(<<"predefined">>) -> predefined;
binary_to_skill_source(<<"imported">>) -> imported;
binary_to_skill_source(_) -> learned.

%% @doc 将 skill_source atom 转换为 binary
-spec skill_source_to_binary(atom()) -> binary().
skill_source_to_binary(learned) -> <<"learned">>;
skill_source_to_binary(predefined) -> <<"predefined">>;
skill_source_to_binary(imported) -> <<"imported">>;
skill_source_to_binary(_) -> <<"learned">>.
