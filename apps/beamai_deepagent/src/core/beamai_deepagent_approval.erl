%%%-------------------------------------------------------------------
%%% @doc 工具审批机制
%%%
%%% 提供工具执行前的审批检查功能：
%%% - 根据配置规则判断工具是否需要审批
%%% - 支持条件审批（基于参数判断）
%%% - 支持自定义审批规则
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_approval).

%% API
-export([check_approval/3]).
-export([default_rules/0]).
-export([merge_rules/2]).

%%====================================================================
%% 类型定义
%%====================================================================

-type approval_rule() :: #{
    tool := binary(),
    message := binary(),
    condition => fun((map()) -> boolean())
}.

-type approval_result() ::
    approved |
    {needs_approval, binary()} |
    {auto_approved, binary()}.

-export_type([approval_rule/0, approval_result/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 检查工具是否需要审批
%%
%% @param ToolName 工具名称
%% @param Args 工具参数
%% @param Config 配置（包含 human_in_loop 设置）
%% @returns approved | {needs_approval, Message}
-spec check_approval(binary(), map(), map()) -> approval_result().
check_approval(ToolName, Args, Config) ->
    HumanConfig = maps:get(human_in_loop, Config, #{}),

    %% 检查是否启用审批
    case maps:get(approval_enabled, HumanConfig, true) of
        false ->
            approved;
        true ->
            %% 获取审批规则
            UserRules = maps:get(approval_rules, HumanConfig, []),
            Rules = merge_rules(default_rules(), UserRules),

            %% 检查是否在自动批准列表中
            AutoApprove = maps:get(auto_approve, HumanConfig, []),
            case lists:member(ToolName, AutoApprove) of
                true ->
                    {auto_approved, <<"工具在自动批准列表中"/utf8>>};
                false ->
                    find_and_check_rule(ToolName, Args, Rules)
            end
    end.

%% @doc 获取默认审批规则
%%
%% 默认对文件写入、编辑、目录创建等操作需要审批
-spec default_rules() -> [approval_rule()].
default_rules() ->
    [
        #{
            tool => <<"write_file">>,
            message => <<"即将写入文件，是否确认？"/utf8>>
        },
        #{
            tool => <<"edit_file">>,
            message => <<"即将编辑文件，是否确认？"/utf8>>
        },
        #{
            tool => <<"mkdir">>,
            message => <<"即将创建目录，是否确认？"/utf8>>
        }
    ].

%% @doc 合并用户规则和默认规则
%%
%% 用户规则优先，相同工具名的规则以用户规则为准
-spec merge_rules([approval_rule()], [approval_rule()]) -> [approval_rule()].
merge_rules(DefaultRules, UserRules) ->
    UserToolNames = [maps:get(tool, R) || R <- UserRules],
    FilteredDefaults = [R || R <- DefaultRules,
                        not lists:member(maps:get(tool, R), UserToolNames)],
    UserRules ++ FilteredDefaults.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 查找并检查规则
find_and_check_rule(ToolName, Args, Rules) ->
    case find_rule(ToolName, Rules) of
        {ok, Rule} ->
            case check_condition(Args, Rule) of
                true ->
                    Message = maps:get(message, Rule),
                    {needs_approval, Message};
                false ->
                    approved
            end;
        not_found ->
            approved
    end.

%% @private 查找工具对应的规则
find_rule(_ToolName, []) ->
    not_found;
find_rule(ToolName, [#{tool := T} = Rule | _Rest]) when T =:= ToolName ->
    {ok, Rule};
find_rule(ToolName, [_Rule | Rest]) ->
    find_rule(ToolName, Rest).

%% @private 检查条件（如果有）
check_condition(_Args, #{condition := CondFun}) when is_function(CondFun, 1) ->
    try
        CondFun(_Args)
    catch
        _:_ -> true  %% 条件检查失败时默认需要审批
    end;
check_condition(_Args, _Rule) ->
    true.  %% 没有条件时默认需要审批
