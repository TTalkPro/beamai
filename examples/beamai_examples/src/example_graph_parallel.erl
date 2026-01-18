%%%-------------------------------------------------------------------
%%% @doc 多专家并行分析系统
%%%
%%% 使用图结构实现多个专家并行分析问题。
%%% 支持 Pregel 引擎进行真正的并行执行。
%%%
%%% 架构:
%%% <pre>
%%% [输入] -> [解析] -> [技术专家]  \
%%%                 -> [业务专家]  -> [综合] -> [输出]
%%%                 -> [UX专家]   /
%%% </pre>
%%%
%%% 特性:
%%%   - 支持并行/顺序执行模式
%%%   - 可配置专家列表
%%%   - 使用 fanout 边实现并行分发
%%%
%%% 配置要求:
%%%   - 必须提供 LLM API 配置
%%%   - 使用 llm_helper 模块调用 LLM
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_graph_parallel).

%% API 导出
-export([build/0, build/1]).
-export([run/2]).
-export([default_experts/0]).
-export([tech_expert_config/0, biz_expert_config/0, ux_expert_config/0]).

%%====================================================================
%% 类型定义
%%====================================================================

-type expert_config() :: #{
    id := atom(),
    name := binary(),
    system_prompt := binary()
}.
-type build_opts() :: #{
    parallel => boolean(),
    experts => [expert_config()]
}.
-type run_config() :: #{
    parallel => boolean(),
    llm_config => map()
}.

%%====================================================================
%% 公共 API
%%====================================================================

%% @doc 使用默认选项构建图
-spec build() -> {ok, map()} | {error, term()}.
build() ->
    build(#{}).

%% @doc 使用指定选项构建图
%% Opts:
%%   - parallel: 是否并行执行专家 (默认 true)
%%   - experts: 专家配置列表 (默认使用 default_experts/0)
-spec build(build_opts()) -> {ok, map()} | {error, term()}.
build(Opts) ->
    Parallel = maps:get(parallel, Opts, true),
    Experts = maps:get(experts, Opts, default_experts()),

    %% 创建图构建器
    B0 = graph:builder(#{max_iterations => 20}),

    %% 添加解析节点
    B1 = graph:add_node(B0, parse, make_parse_node()),

    %% 添加专家节点
    B2 = lists:foldl(fun(Expert, Acc) ->
        ExpertId = maps:get(id, Expert),
        graph:add_node(Acc, ExpertId, make_expert_node(Expert))
    end, B1, Experts),

    %% 添加综合节点
    B3 = graph:add_node(B2, synthesize, make_synthesize_node(Experts)),

    %% 添加边
    ExpertIds = [maps:get(id, E) || E <- Experts],
    B4 = add_expert_edges(B3, ExpertIds, Parallel),

    %% 设置入口并编译
    B5 = graph:set_entry(B4, parse),
    graph:compile(B5).

%% @doc 运行专家分析
%% Question: 待分析的问题
%% Config: 运行配置，必须包含 llm_config
-spec run(binary(), run_config()) -> {ok, map()} | {error, term()}.
run(Question, Config) ->
    %% 验证配置
    case validate_config(Config) of
        ok ->
            Parallel = maps:get(parallel, Config, true),

            %% 构建图
            BuildOpts = #{
                parallel => Parallel
            },
            {ok, Graph} = build(BuildOpts),

            %% 初始化状态
            MockMode = maps:get(mock_mode, Config, false),
            InitState = graph:state(#{
                input => Question,
                mock_mode => MockMode,
                llm_config => maps:get(llm_config, Config, #{})
            }),

            %% 运行图 (使用 Pregel 引擎)
            %% Parallel 选项控制是否使用多 Worker
            Workers = case Parallel of
                true -> 4;
                false -> 1
            end,
            Result = graph:run(Graph, InitState, #{workers => Workers}),

            case maps:get(status, Result) of
                completed ->
                    FinalState = maps:get(final_state, Result),
                    {ok, extract_results(FinalState)};
                Status ->
                    {error, {execution_failed, Status, Result}}
            end;
        {error, Reason} ->
            {error, {config_error, Reason}}
    end.

%% @doc 验证配置
-spec validate_config(run_config()) -> ok | {error, term()}.
validate_config(Config) ->
    %% mock 模式下不需要 LLM 配置
    case maps:get(mock_mode, Config, false) of
        true ->
            ok;
        false ->
            case maps:find(llm_config, Config) of
                {ok, LLMConfig} when is_map(LLMConfig), map_size(LLMConfig) > 0 ->
                    ok;
                _ ->
                    {error, missing_llm_config}
            end
    end.

%%====================================================================
%% 专家配置
%%====================================================================

%% @doc 获取默认专家列表
-spec default_experts() -> [expert_config()].
default_experts() ->
    [tech_expert_config(), biz_expert_config(), ux_expert_config()].

%% @doc 技术专家配置
-spec tech_expert_config() -> expert_config().
tech_expert_config() ->
    #{
        id => tech_expert,
        name => <<"技术专家"/utf8>>,
        system_prompt => <<"你是一位资深技术专家。请从技术架构、性能、可扩展性、安全性等角度分析问题。
提供具体的技术建议和潜在风险评估。"/utf8>>
    }.

%% @doc 业务专家配置
-spec biz_expert_config() -> expert_config().
biz_expert_config() ->
    #{
        id => biz_expert,
        name => <<"业务专家"/utf8>>,
        system_prompt => <<"你是一位经验丰富的业务分析师。请从商业价值、成本效益、市场竞争、
ROI等角度分析问题。提供可行的商业策略建议。"/utf8>>
    }.

%% @doc UX专家配置
-spec ux_expert_config() -> expert_config().
ux_expert_config() ->
    #{
        id => ux_expert,
        name => <<"用户体验专家"/utf8>>,
        system_prompt => <<"你是一位用户体验设计专家。请从用户需求、易用性、可访问性、
用户旅程等角度分析问题。提供以用户为中心的设计建议。"/utf8>>
    }.

%%====================================================================
%% 内部函数 - 节点创建
%%====================================================================

%% @doc 创建解析节点
%% 解析输入并准备分发给专家
-spec make_parse_node() -> fun((map()) -> {ok, map()}).
make_parse_node() ->
    fun(State) ->
        Input = graph:get(State, input, <<>>),

        %% 记录解析结果
        State1 = graph:set(State, parsed_input, Input),
        State2 = graph:set(State1, parse_timestamp, erlang:system_time(millisecond)),

        {ok, State2}
    end.

%% @doc 创建专家节点
%% Expert: 专家配置
-spec make_expert_node(expert_config()) -> fun((map()) -> {ok, map()}).
make_expert_node(Expert) ->
    ExpertId = maps:get(id, Expert),
    ExpertName = maps:get(name, Expert),
    SystemPrompt = maps:get(system_prompt, Expert),

    fun(State) ->
        Input = graph:get(State, parsed_input, graph:get(State, input, <<>>)),
        MockMode = graph:get(State, mock_mode, false),
        LLMConfig = graph:get(State, llm_config, #{}),

        %% 生成分析结果（根据模式）
        Analysis = case MockMode of
            true ->
                %% Mock 模式返回模拟数据
                iolist_to_binary([ExpertName, <<" 的模拟分析结果：对于问题 \""/utf8>>,
                                  Input, <<"\"，建议..."/utf8>>]);
            false ->
                call_llm_for_analysis(LLMConfig, SystemPrompt, Input)
        end,

        %% 存储分析结果
        ResultKey = result_key(ExpertId),
        State1 = graph:set(State, ResultKey, Analysis),

        {ok, State1}
    end.

%% @doc 创建综合节点
%% 汇总所有专家分析并生成建议
-spec make_synthesize_node([expert_config()]) -> fun((map()) -> {ok, map()}).
make_synthesize_node(Experts) ->
    ExpertIds = [maps:get(id, E) || E <- Experts],

    fun(State) ->
        %% 收集所有专家分析
        Analyses = lists:map(fun(ExpertId) ->
            ResultKey = result_key(ExpertId),
            {ExpertId, graph:get(State, ResultKey, <<"无分析结果"/utf8>>)}
        end, ExpertIds),

        %% 生成综合建议（根据模式）
        MockMode = graph:get(State, mock_mode, false),
        LLMConfig = graph:get(State, llm_config, #{}),
        Recommendation = case MockMode of
            true ->
                %% Mock 模式返回模拟建议
                <<"综合各专家意见，模拟建议：基于技术、业务和用户体验的综合考量，建议采取..."/utf8>>;
            false ->
                call_llm_for_synthesis(LLMConfig, Analyses)
        end,

        State1 = graph:set(State, recommendation, Recommendation),
        {ok, State1}
    end.

%%====================================================================
%% 内部函数 - 边连接
%%====================================================================

%% @doc 添加专家相关的边
%% 根据并行模式选择边类型
-spec add_expert_edges(map(), [atom()], boolean()) -> map().
add_expert_edges(Builder, ExpertIds, true) ->
    %% 并行模式：使用 fanout 边
    B1 = graph:add_fanout_edge(Builder, parse, ExpertIds),

    %% 所有专家连接到综合节点
    lists:foldl(fun(ExpertId, Acc) ->
        graph:add_edge(Acc, ExpertId, synthesize)
    end, B1, ExpertIds);

add_expert_edges(Builder, ExpertIds, false) ->
    %% 顺序模式：链式连接
    B1 = graph:add_edge(Builder, parse, hd(ExpertIds)),

    %% 链式连接专家
    {B2, _} = lists:foldl(fun(ExpertId, {Acc, Prev}) ->
        case Prev of
            undefined -> {Acc, ExpertId};
            _ -> {graph:add_edge(Acc, Prev, ExpertId), ExpertId}
        end
    end, {B1, undefined}, ExpertIds),

    %% 最后一个专家连接到综合节点
    LastExpert = lists:last(ExpertIds),
    graph:add_edge(B2, LastExpert, synthesize).

%%====================================================================
%% 内部函数 - LLM 调用
%%====================================================================

%% @doc 调用 LLM 进行分析
-spec call_llm_for_analysis(map(), binary(), binary()) -> binary().
call_llm_for_analysis(LLMConfig, SystemPrompt, Input) ->
    Messages = [
        #{role => system, content => SystemPrompt},
        #{role => user, content => Input}
    ],

    case llm_helper:call_llm(Messages, LLMConfig, 500) of
        {error, Reason} ->
            iolist_to_binary(io_lib:format("分析失败: ~p", [Reason]));
        Content ->
            Content
    end.

%% @doc 调用 LLM 进行综合
-spec call_llm_for_synthesis(map(), [{atom(), binary()}]) -> binary().
call_llm_for_synthesis(LLMConfig, Analyses) ->
    AnalysisText = lists:map(fun({ExpertId, Analysis}) ->
        [atom_to_binary(ExpertId), <<":\n"/utf8>>, Analysis, <<"\n\n"/utf8>>]
    end, Analyses),

    SystemPrompt = <<"你是一位资深顾问。请综合以下多位专家的分析，提供统一的建议。"/utf8>>,
    UserPrompt = iolist_to_binary([
        <<"请综合以下专家分析，给出最终建议:\n\n"/utf8>>,
        AnalysisText
    ]),

    Messages = [
        #{role => system, content => SystemPrompt},
        #{role => user, content => UserPrompt}
    ],

    case llm_helper:call_llm(Messages, LLMConfig, 800) of
        {error, Reason} ->
            iolist_to_binary(io_lib:format("综合失败: ~p", [Reason]));
        Content ->
            Content
    end.

%%====================================================================
%% 内部函数 - 工具函数
%%====================================================================

%% @doc 生成结果键名
-spec result_key(atom()) -> atom().
result_key(ExpertId) ->
    list_to_atom(atom_to_list(ExpertId) ++ "_analysis").

%% @doc 提取最终结果
-spec extract_results(map()) -> map().
extract_results(State) ->
    Keys = [tech_expert_analysis, biz_expert_analysis, ux_expert_analysis, recommendation],
    lists:foldl(fun(Key, Acc) ->
        case graph:get(State, Key, undefined) of
            undefined -> Acc;
            Value -> Acc#{Key => Value}
        end
    end, #{}, Keys).
