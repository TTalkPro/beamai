%%%-------------------------------------------------------------------
%%% @doc 阿里云百炼决策模型 Provider（decision-model-preview）
%%%
%%% 百炼的决策模型走的也是 TypeSafe System One 协议，但**不在**
%%% dashscope.aliyuncs.com 上，而在 MaaS 域名的 OpenAI 兼容路径下：
%%%
%%%   - 工作空间：`https://{WorkspaceId}.cn-beijing.maas.aliyuncs.com/compatible-mode/v1'
%%%     （国际站区域为 ap-southeast-1）
%%%   - Token Plan 套餐：`https://token-plan.cn-beijing.maas.aliyuncs.com/compatible-mode/v1'（默认）
%%%
%%% 用工作空间 Key 时需在 Config 里给出带 WorkspaceId 的 base_url。
%%% 该模型不出现在百炼的 /models 列表里，模型名固定 decision-model-preview。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_decision_provider_dashscope).
-behaviour(beamai_decision_provider_behaviour).

-export([name/0, default_config/0, validate_config/1, decide/2]).
-export([workspace_base_url/1, workspace_base_url/2]).

-define(BASE_URL, <<"https://token-plan.cn-beijing.maas.aliyuncs.com/compatible-mode/v1">>).
-define(MODEL, <<"decision-model-preview">>).

name() -> <<"DashScope Decision">>.

default_config() ->
    #{
        base_url => ?BASE_URL,
        model => ?MODEL,
        timeout => beamai_llm_provider_common:default_timeout(dashscope_decision)
    }.

validate_config(#{api_key := Key}) when is_binary(Key), byte_size(Key) > 0 ->
    ok;
validate_config(_) ->
    {error, missing_api_key}.

decide(Config, Request) ->
    beamai_decision_common:decide_http(dashscope, Config, Request, ?BASE_URL, ?MODEL).

%% @doc 工作空间形态的 base_url（默认北京区域）
-spec workspace_base_url(binary()) -> binary().
workspace_base_url(WorkspaceId) ->
    workspace_base_url(WorkspaceId, <<"cn-beijing">>).

-spec workspace_base_url(binary(), binary()) -> binary().
workspace_base_url(WorkspaceId, Region) ->
    <<"https://", WorkspaceId/binary, ".", Region/binary, ".maas.aliyuncs.com/compatible-mode/v1">>.
