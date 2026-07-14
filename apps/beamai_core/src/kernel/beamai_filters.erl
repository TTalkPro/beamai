%%%-------------------------------------------------------------------
%%% @doc 内置 filter 集（三链示范 + 常用能力）
%%%
%%% 对照 clj-agent filter-chain-design.md §3。均为纯构造器，返回 beamai_filter
%%% 的 filter map，用户经 beamai_kernel:add_filter/2 挂载。
%%%
%%%   tool 链：logging_filter/0、timeout_filter/1、approval_filter/1
%%%   turn 链：validation_turn_filter/2
%%%   token 链：token_redact_filter/2、hold_release_filter/1
%%%     （对照 clj-agent token-stream-filter-design.md §4）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_filters).

-export([logging_filter/0, timeout_filter/1, approval_filter/1,
         validation_turn_filter/2,
         token_redact_filter/2, hold_release_filter/1]).

%%====================================================================
%% tool 链
%%====================================================================

%% @doc 日志 filter（tool 链）：调用前后记录（debug 级），不改结果
-spec logging_filter() -> beamai_filter:filter().
logging_filter() ->
    beamai_filter:new(<<"logging">>, #{
        around_tool => fun(#{tool := ToolSpec, args := Args} = Req, _FCtx, Next) ->
            Name = beamai_tool:get_name(ToolSpec),
            logger:debug("beamai tool call: ~ts args=~p", [Name, Args]),
            Resp = Next(Req),
            logger:debug("beamai tool done: ~ts", [Name]),
            Resp
        end
    }).

%% @doc 超时 filter（tool 链）：单个工具执行超过 Ms 毫秒则超时短路
%%
%% 超时经 throw 报错 → 工具链归一为 `{error, timeout}` → 屏障处按
%% beamai_tool_error 分类为 **transient**（超时正是重试有意义的类别）。
%% 声明了 `retry` 的幂等工具，其内部 handler 级重试对 handler 自身超时无效；
%% 本 filter 是整个工具执行（含 filter 链内层）的墙钟超时。
-spec timeout_filter(pos_integer()) -> beamai_filter:filter().
timeout_filter(Ms) when is_integer(Ms), Ms > 0 ->
    beamai_filter:new(<<"timeout">>, #{
        around_tool => fun(Req, _FCtx, Next) ->
            Parent = self(),
            {Pid, Ref} = spawn_monitor(fun() -> Parent ! {tf_ok, self(), Next(Req)} end),
            receive
                {tf_ok, Pid, Resp} ->
                    erlang:demonitor(Ref, [flush]),
                    Resp;
                {'DOWN', Ref, process, Pid, Reason} ->
                    throw({tool_filter_crash, Reason})
            after Ms ->
                exit(Pid, kill),
                throw(timeout)
            end
        end
    }).

%% @doc 审批 filter（tool 链）：ApproveFun(Name, Args) 返回 false 则拒绝短路
%%
%% 拒绝结果作为**正常工具结果**回模型（policy 类 errors-are-data），不执行工具、
%% 无 writes。**非交互式**——交互式审批请用 gate（callbacks on_tool_call，批前串行
%% 预判，避免在并行任务里并发弹提示）。搭配 `sensitive` 工具标注见
%% design/turn_filter_hitl_hardening.md §5（approval_filter 仅拦截 sensitive 工具）。
-spec approval_filter(fun((binary(), map()) -> boolean())) -> beamai_filter:filter().
approval_filter(ApproveFun) when is_function(ApproveFun, 2) ->
    beamai_filter:new(<<"approval">>, #{
        around_tool => fun(#{tool := ToolSpec, args := Args, context := Ctx} = Req, _FCtx, Next) ->
            Name = beamai_tool:get_name(ToolSpec),
            %% 仅对 sensitive 工具审批；非 sensitive 直接放行
            case beamai_tool:is_sensitive(ToolSpec) andalso not ApproveFun(Name, Args) of
                true ->
                    Rejection = beamai_tool:encode_result(
                        #{error => #{type => not_approved,
                                     message => <<"已拒绝执行（未获批准）">>}}),
                    #{result => Rejection, writes => #{}, context => Ctx};
                false ->
                    Next(Req)
            end
        end
    }).

%%====================================================================
%% turn 链
%%====================================================================

%% @doc 最终答案校验 filter（turn 链）
%%
%% ValidateFun(Response) -> ok | {invalid, Reason}。对 `{ok, Response, _, _}` 的
%% completed 结果校验：不合格把 Reason 作反馈消息**重入循环**（全新循环，
%% resume=false）；重试 MaxRetries 次耗尽则原样返回；`{interrupt,_,_}` /
%% `{error,_}` 透传（硬规则，不得在暂停/错误态重入）。
%%
%% 对标 Spring AI StructuredOutputValidationAdvisor，配 provider 原生
%% 结构化输出（json_schema）使用。
-spec validation_turn_filter(fun((map()) -> ok | {invalid, term()}), non_neg_integer()) ->
    beamai_filter:filter().
validation_turn_filter(ValidateFun, MaxRetries)
  when is_function(ValidateFun, 1), is_integer(MaxRetries), MaxRetries >= 0 ->
    beamai_filter:new(<<"validation">>, #{
        around_turn => fun(Req, _FCtx, Next) ->
            validate_loop(Req, Next, ValidateFun, MaxRetries)
        end
    }).

%%====================================================================
%% token 链（token_xf，流式专用）
%%====================================================================

%% @doc token 脱敏 filter（token_xf）：无状态逐 token 正则替换
%%
%% **已知限制**：秘密被切在两个 chunk 之间时漏检（跨 chunk 检测需有状态
%% 缓冲，按需自写 token_xf 或用 hold_release_filter 整流后审）。
%% 只改送 sink 的出站流；最终归一化响应（memory 落库/turn 结果）不受影响。
-spec token_redact_filter(iodata(), binary()) -> beamai_filter:filter().
token_redact_filter(Pattern, Replacement) when is_binary(Replacement) ->
    {ok, MP} = re:compile(Pattern),
    beamai_filter:new(<<"token_redact">>, #{
        token_xf => #{
            step => fun(#{token := T} = TD, S) ->
                Redacted = re:replace(T, MP, Replacement,
                                      [global, {return, binary}]),
                {[TD#{token => Redacted}], S}
            end
        }
    }).

%% @doc 先审后放 filter（token_xf）：缓冲整流不外泄，完流时全文审查
%%
%% CheckFun(全文 binary) -> `ok`（通过：缓冲按原序放行）|
%% `{block, Text}`（不通过：只 emit 一个替换 token）。
%%
%% **代价即语义**：用户在流结束前看不到任何 token——"完整答案没成形就无法审"
%% 这一根本矛盾任何机制都消不掉，本 filter 只是把缓冲逻辑标准化。异常完流
%% 不 flush，缓冲直接丢弃（半截答案不外泄）。
-spec hold_release_filter(fun((binary()) -> ok | {block, binary()})) ->
    beamai_filter:filter().
hold_release_filter(CheckFun) when is_function(CheckFun, 1) ->
    beamai_filter:new(<<"hold_release">>, #{
        token_xf => #{
            init => [],   %% 倒序缓冲
            step => fun(TD, Buf) -> {[], [TD | Buf]} end,
            flush => fun(Buf) ->
                Ordered = lists:reverse(Buf),
                FullText = iolist_to_binary([T || #{token := T} <- Ordered]),
                case CheckFun(FullText) of
                    ok -> Ordered;
                    {block, Text} -> [#{token => Text, meta => #{}}]
                end
            end
        }
    }).

%%====================================================================
%% 内部
%%====================================================================

validate_loop(Req, Next, ValidateFun, Retries) ->
    case Next(Req) of
        {ok, Response, _TCM, _Iter} = R ->
            case ValidateFun(Response) of
                ok ->
                    R;
                {invalid, Reason} when Retries > 0 ->
                    Feedback = #{role => user, content => feedback_text(Reason)},
                    %% 重入：全新循环（有真实入口消息，resume=false 让请求侧 filter 照常）
                    validate_loop(Req#{messages => [Feedback], resume => false},
                                  Next, ValidateFun, Retries - 1);
                {invalid, _Reason} ->
                    R   %% 重试耗尽，原样返回
            end;
        Other ->
            Other   %% interrupt / error 透传
    end.

feedback_text(Reason) when is_binary(Reason) ->
    <<"上一次回答未通过校验：", Reason/binary, "。请修正后重新作答。">>;
feedback_text(Reason) ->
    iolist_to_binary(io_lib:format("上一次回答未通过校验：~p。请修正后重新作答。", [Reason])).
