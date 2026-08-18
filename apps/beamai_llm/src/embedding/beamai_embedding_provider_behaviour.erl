%%%-------------------------------------------------------------------
%%% @doc Embedding Provider 行为定义模块
%%%
%%% 定义文本向量化（Embedding）Provider 的统一接口，与
%%% beamai_llm_provider_behaviour（对话）并列，由 beamai_embedding 统一调度。
%%%
%%% == 支持的 Provider ==
%%%
%%%   - OpenAI (text-embedding-3-small / large)
%%%   - DashScope 通义千问 (text-embedding-v4)
%%%   - Zhipu 智谱 (embedding-3)
%%%   - SiliconFlow 硅基流动 (BAAI/bge-m3, Qwen3-Embedding 等)
%%%   - Ollama 本地模型 (nomic-embed-text 等)
%%%   - Mock（确定性伪向量，测试用）
%%%
%%% == 设计原则 ==
%%%
%%% 1. 统一请求：input 恒为文本列表，单条文本由上层包成单元素列表
%%% 2. 统一响应：embeddings 顺序与 input 一一对应
%%% 3. 批量上限：由 max_batch_size/0 声明，超出部分由 beamai_embedding 自动分批
%%% 4. 错误统一：所有错误以 {error, term()} 返回
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_embedding_provider_behaviour).

-export_type([config/0, request/0, response/0, vector/0, usage/0]).

%%====================================================================
%% 类型定义
%%====================================================================

%% 向量
-type vector() :: [float()].

%% Provider 配置
-type config() :: #{
    provider => atom(),
    api_key => binary(),
    base_url => binary(),
    endpoint => binary(),
    model => binary(),
    timeout => pos_integer(),
    dimensions => pos_integer(),
    atom() => term()
}.

%% 向量化请求
-type request() :: #{
    input := [binary()],
    dimensions => pos_integer(),
    encoding_format => binary(),
    text_type => binary(),     %% DashScope：query | document
    user => binary(),
    extra => map()
}.

%% Token 使用统计
-type usage() :: #{
    input_tokens := non_neg_integer(),
    total_tokens := non_neg_integer()
}.

%% 向量化响应（统一格式）
-type response() :: #{
    model := binary(),
    provider := atom(),
    embeddings := [vector()],
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

%% @doc 执行一次向量化请求
%%
%% Request 的 input 已由调用方保证不超过 max_batch_size/0。
-callback embed(config(), request()) -> {ok, response()} | {error, term()}.

%% @doc 单次请求可携带的最大文本条数
-callback max_batch_size() -> pos_integer().

%% @doc 是否支持指定输出维度（dimensions 参数）
-callback supports_dimensions() -> boolean().
