%%%-------------------------------------------------------------------
%%% @doc DeepAgent 公共工具模块
%%%
%%% 提供跨模块共享的工具函数，避免代码冗余。
%%% 包含：二进制截断、步骤结果格式化、子代理运行、安全错误格式化等。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_utils).

-export([
    truncate/2,
    format_step_result/1,
    format_step_results/1,
    run_agent/3,
    make_step_result/4,
    format_error/1,
    status_to_binary/1,
    available_plugins/0
]).

%%====================================================================
%% API
%%====================================================================

%% @doc 截断二进制字符串到指定最大长度
%%
%% 如果超过 Max 字节，截断并追加 "..." 后缀。
%% 对非二进制输入返回占位文本。
-spec truncate(term(), pos_integer()) -> binary().
truncate(Bin, Max) when is_binary(Bin), byte_size(Bin) =< Max ->
    Bin;
truncate(Bin, Max) when is_binary(Bin), Max > 3 ->
    <<(binary:part(Bin, 0, Max - 3))/binary, "...">>;
truncate(Bin, _Max) when is_binary(Bin) ->
    Bin;
truncate(_, _) ->
    <<"(non-binary)">>.

%% @doc 将步骤执行结果格式化为可读的单行文本
%%
%% 输出格式: "- Step N [status]: 截断后的结果"
%% 用于构建反思和执行器的上下文提示词。
-spec format_step_result(map()) -> binary().
format_step_result(#{step_id := Id, status := Status, result := Result}) ->
    StatusBin = status_to_binary(Status),
    TruncResult = truncate(Result, 300),
    iolist_to_binary([
        <<"- Step ">>, integer_to_binary(Id),
        <<" [">>, StatusBin, <<"]: ">>,
        TruncResult
    ]);
format_step_result(_) ->
    <<"- (unknown step result)">>.

%% @doc 将步骤结果列表格式化为多行文本
%%
%% 每个结果调用 format_step_result/1，用换行符连接。
-spec format_step_results([map()]) -> binary().
format_step_results(Results) ->
    Lines = [format_step_result(R) || R <- Results],
    iolist_to_binary(lists:join(<<"\n">>, Lines)).

%% @doc 创建并运行子代理，提取文本回复
%%
%% 封装 beamai_agent 的 new + run 流程，返回 LLM 回复文本。
%% AgentConfig: 代理配置（必须包含 llm）
%% Message: 发送给代理的用户消息
%% ErrorTag: 错误标签（用于区分调用来源，使用已有 atom 如 planner/executor/reflector）
-spec run_agent(map(), binary(), atom()) -> {ok, binary()} | {error, term()}.
run_agent(AgentConfig, Message, ErrorTag) ->
    case beamai_agent:new(AgentConfig) of
        {ok, Agent} ->
            case beamai_agent:run(Agent, Message) of
                {ok, #{content := Content}, _} ->
                    {ok, Content};
                {error, Reason} ->
                    {error, {ErrorTag, run_failed, Reason}}
            end;
        {error, Reason} ->
            {error, {ErrorTag, init_failed, Reason}}
    end.

%% @doc 构造步骤执行结果 map
%%
%% 统一创建 step_result，避免各模块重复构建相同结构。
%% StepId: 步骤编号
%% Status: 执行状态（completed | failed | crashed | timeout）
%% Result: 结果文本
%% TimeMsOrToolCalls: 附加信息 map（可包含 time_ms, tool_calls_made）
-spec make_step_result(pos_integer(), atom(), binary(), map()) -> map().
make_step_result(StepId, Status, Result, Extra) ->
    Base = #{
        step_id => StepId,
        status => Status,
        result => Result,
        tool_calls_made => maps:get(tool_calls_made, Extra, []),
        time_ms => maps:get(time_ms, Extra, 0)
    },
    Base.

%% @doc 安全地将错误项格式化为二进制字符串
%%
%% 不使用 binary_to_atom 或其他可能生成新 atom 的操作。
%% 直接使用 io_lib:format 将任意 term 转为字符串。
-spec format_error(term()) -> binary().
format_error(Reason) when is_binary(Reason) ->
    Reason;
format_error(Reason) ->
    iolist_to_binary(io_lib:format("~p", [Reason])).

%% @doc 将步骤状态 atom 转换为二进制字符串
%%
%% 使用固定映射表，不依赖 atom_to_binary 的动态行为。
%% 仅处理已知的状态 atom，未知状态返回 <<"unknown">>。
-spec status_to_binary(atom()) -> binary().
status_to_binary(completed) -> <<"completed">>;
status_to_binary(failed) -> <<"failed">>;
status_to_binary(crashed) -> <<"crashed">>;
status_to_binary(timeout) -> <<"timeout">>;
status_to_binary(pending) -> <<"pending">>;
status_to_binary(in_progress) -> <<"in_progress">>;
status_to_binary(skipped) -> <<"skipped">>;
status_to_binary(_) -> <<"unknown">>.

%% @doc 获取系统中可用的工具插件列表
%%
%% 检测候选插件模块是否已加载，返回可用模块列表。
%% 候选列表为硬编码的已知插件模块名。
-spec available_plugins() -> [module()].
available_plugins() ->
    Candidates = [beamai_plugin_file, beamai_plugin_shell, beamai_plugin_todo],
    [M || M <- Candidates, code:ensure_loaded(M) =:= {module, M}].
