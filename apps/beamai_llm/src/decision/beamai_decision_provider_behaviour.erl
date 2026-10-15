%%%-------------------------------------------------------------------
%%% @doc 决策（Decision）Provider 行为定义模块
%%%
%%% 定义结构化决策 Provider 的统一接口，与 beamai_llm_provider_behaviour（对话）、
%%% beamai_embedding_provider_behaviour（向量化）并列，由 beamai_decision 统一调度。
%%%
%%% 协议是 TypeSafe 的 **System One**：给模型一份 `state'（待判断的消息及上下文）
%%% 和一组带 id 的 `questions'，模型对每道题给出**带概率的结构化回答**，而不是
%%% 自由文本——这使它适合做路由 / 分类 / 打分这类「决策支持」：几十毫秒内回答，
%%% 答案可直接比较阈值。
%%%
%%% == 三种题型 ==
%%%
%%%   - `choice'：多选一。`criteria' 为 选项名 → 说明 的 map（说明可为 null），
%%%     回答 `choice' + `confidence' + 各选项 `probabilities'
%%%   - `noul'：是 / 否。回答 `noul' 为「是」的概率（0 ~ 1）
%%%   - `score'：有序等级打分。`criteria' 为按序排列的等级说明列表，回答 `score'
%%%     （0 ~ N-1 的连续值）+ `confidence' + 各等级 `probabilities' + `legend'
%%%
%%% == 支持的 Provider ==
%%%
%%%   - TypeSafe（jev-latest 等官方模型）
%%%   - 阿里云百炼 decision-model-preview（同一 System One 协议）
%%%   - 任意 System One 兼容服务（LiteGate 网关、本地 Laya / vLLM 等）
%%%   - Mock（确定性回答，测试用）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_decision_provider_behaviour).

-export_type([config/0, request/0, response/0, question/0, question_type/0,
              questions/0, answer/0, usage/0]).

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

-type question_type() :: noul | choice | score.

%% 一道题（统一格式，id 在外层 map 的键上）
-type question() :: #{
    type := question_type(),
    instructions := binary(),
    %% choice：#{选项名 => 说明 | null} 或 [选项名]；score：[等级说明]
    criteria => #{binary() => binary() | null} | [binary()]
}.

-type questions() :: #{binary() | atom() => question()}.

%% 决策请求（已归一：state / questions 均为 binary 键的 wire 结构）
-type request() :: #{
    state := map(),
    questions := #{binary() => map()},
    extra => map()
}.

%% 一道题的回答（统一格式）
-type answer() ::
    #{type := choice, choice := binary(), confidence := float(),
      probabilities := #{binary() => float()}} |
    #{type := noul, probability := float()} |
    #{type := score, score := float(), confidence := float() | undefined,
      probabilities := #{binary() => float()}, legend := #{binary() => binary()}} |
    #{type := unknown, raw := term()}.

%% Token 使用统计（System One 只计输入 token；没有回答 token 的概念）
-type usage() :: #{
    input_tokens := non_neg_integer(),
    output_tokens := non_neg_integer(),
    total_tokens := non_neg_integer()
}.

%% 决策响应（统一格式）
-type response() :: #{
    model := binary(),
    provider := atom(),
    answers := #{binary() => answer()},
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

%% @doc 校验配置（如必填 api_key / base_url）
-callback validate_config(config()) -> ok | {error, term()}.

%% @doc 执行一次决策请求
%%
%% Request 的 questions 已由调用方校验过题型与必填字段。
-callback decide(config(), request()) -> {ok, response()} | {error, term()}.
