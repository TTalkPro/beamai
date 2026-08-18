%%%-------------------------------------------------------------------
%%% @doc Rerank（重排序）Provider 行为定义模块
%%%
%%% 定义文档重排序 Provider 的统一接口，与 beamai_llm_provider_behaviour
%%% （对话）、beamai_embedding_provider_behaviour（向量化）并列，
%%% 由 beamai_rerank 统一调度。
%%%
%%% == 典型用途 ==
%%%
%%% RAG 检索的第二阶段：向量召回一批候选后，用 Cross-Encoder 重排序模型
%%% 按 query 与文档的相关性重新打分，取 top_n 送入上下文。
%%%
%%% == 支持的 Provider ==
%%%
%%%   - SiliconFlow 硅基流动 (BAAI/bge-reranker-v2-m3 等)
%%%   - DashScope 通义千问 (gte-rerank-v2)
%%%   - Jina AI (jina-reranker-v2-base-multilingual)
%%%   - Cohere (rerank-v3.5)
%%%   - Voyage AI (rerank-2.5)
%%%   - Mock（确定性词重叠打分，测试用）
%%%
%%% == 设计原则 ==
%%%
%%% 1. 统一请求：query + documents（文本列表）
%%% 2. 统一响应：results 按相关性降序，index 指回原始文档下标
%%% 3. 条数上限：由 max_documents/0 声明，超出由 beamai_rerank 显式报错
%%%    （重排序是全局比较，不能像向量化那样分批）
%%% 4. 错误统一：所有错误以 {error, term()} 返回
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_rerank_provider_behaviour).

-export_type([config/0, request/0, response/0, result/0, usage/0]).

%%====================================================================
%% 类型定义
%%====================================================================

%% Provider 配置
-type config() :: #{
    provider => atom(),
    api_key => binary(),
    base_url => binary(),
    endpoint => binary(),
    model => binary(),
    timeout => pos_integer(),
    atom() => term()
}.

%% 重排序请求
-type request() :: #{
    query := binary(),
    documents := [binary()],
    top_n => pos_integer(),
    return_documents => boolean(),
    extra => map()
}.

%% 单条重排序结果
-type result() :: #{
    index := non_neg_integer(),   %% 原始文档下标
    score := float(),             %% 相关性得分
    document => binary()          %% 文档原文（由 beamai_rerank 回填）
}.

%% Token 使用统计
-type usage() :: #{
    input_tokens := non_neg_integer(),
    total_tokens := non_neg_integer()
}.

%% 重排序响应（统一格式）
-type response() :: #{
    model := binary(),
    provider := atom(),
    results := [result()],
    usage := usage(),
    raw := map(),
    metadata := map()
}.

%%====================================================================
%% 回调函数定义
%%====================================================================

%% @doc Provider 显示名称
-callback name() -> binary().

%% @doc 默认配置（模型、超时、站点等）
-callback default_config() -> map().

%% @doc 校验配置（如必填 api_key）
-callback validate_config(config()) -> ok | {error, term()}.

%% @doc 执行一次重排序请求
-callback rerank(config(), request()) -> {ok, response()} | {error, term()}.

%% @doc 单次请求可携带的最大文档条数
-callback max_documents() -> pos_integer().
