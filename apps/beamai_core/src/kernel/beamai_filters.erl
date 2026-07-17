%%%-------------------------------------------------------------------
%%% @doc 内置 filter 集（三链示范 + 常用能力）
%%%
%%% 对照 clj-agent filter-chain-design.md §3。均为纯构造器，返回 beamai_filter
%%% 的 filter map，构建 kernel 时经 beamai_kernel:new/2 的 filters 列表一次性给出。
%%%
%%%   chat 链：safeguard_filter/1,2
%%%   tool 链：timeout_filter/1、approval_filter/1
%%%   turn 链：validation_turn_filter/2、schema_validation_turn_filter/2,3
%%%   三链通吃：logging_filter/0
%%%   token 链：token_redact_filter/2、hold_release_filter/1
%%%     （对照 clj-agent token-stream-filter-design.md §4）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_filters).

-export([logging_filter/0, timeout_filter/1, approval_filter/1,
         safeguard_filter/1, safeguard_filter/2,
         validation_turn_filter/2,
         schema_validation_turn_filter/2, schema_validation_turn_filter/3,
         token_redact_filter/2, hold_release_filter/1]).

%% 敏感词命中时的缺省答复（对照 Spring AI SafeGuardAdvisor 的 DEFAULT_FAILURE_RESPONSE）
-define(DEFAULT_SAFEGUARD_RESPONSE,
        <<"抱歉，由于涉及敏感内容，我无法回应。换个话题或换种问法好吗？"/utf8>>).

%%====================================================================
%% 三链通吃
%%====================================================================

%% @doc 日志 filter（三链）：每链调用前后记录（debug 级），不改结果
%%
%% 对标 Spring AI SimpleLoggerAdvisor（它只记 chat 请求/响应）——这里三个 hook
%% 全带上：放哪条链就记哪条，一个 filter 覆盖 turn/chat/tool 三个粒度。
%% 放在 filters 列表**首位**记全景，放在某个 filter 之后则只看得到那层之内的
%% 改写结果（洋葱层序即可见范围，这正是排查 filter 改写的用法）。
-spec logging_filter() -> beamai_filter:filter().
logging_filter() ->
    beamai_filter:new(<<"logging">>, #{
        around_turn => fun(#{messages := Msgs} = Req, _FCtx, Next) ->
            logger:debug("beamai turn start: ~p messages", [length(Msgs)]),
            Resp = Next(Req),
            logger:debug("beamai turn done: ~ts", [turn_outcome(Resp)]),
            Resp
        end,
        around_chat => fun(#{messages := Msgs} = Req, _FCtx, Next) ->
            logger:debug("beamai chat call: ~p messages", [length(Msgs)]),
            #{response := Response} = Resp = Next(Req),
            logger:debug("beamai chat done: finish_reason=~p tool_calls=~p",
                         [beamai_llm_response:finish_reason(Response),
                          length(beamai_llm_response:tool_calls(Response))]),
            Resp
        end,
        around_tool => fun(#{tool := ToolSpec, args := Args} = Req, _FCtx, Next) ->
            Name = beamai_tool:get_name(ToolSpec),
            logger:debug("beamai tool call: ~ts args=~p", [Name, Args]),
            Resp = Next(Req),
            logger:debug("beamai tool done: ~ts", [Name]),
            Resp
        end
    }).

%% @private turn 结果的一句话摘要（turn 链响应是工具循环结果 tuple）
turn_outcome({ok, _Resp, TCM, Iter, _Msgs}) ->
    io_lib:format("ok tool_calls=~p iterations=~p", [length(TCM), Iter]);
turn_outcome({interrupt, Type, _Ctx}) ->
    io_lib:format("interrupt ~p", [Type]);
turn_outcome({error, Reason}) ->
    io_lib:format("error ~p", [Reason]);
turn_outcome(_) ->
    "unknown".

%%====================================================================
%% chat 链
%%====================================================================

%% @doc 敏感词拦截 filter（chat 链）：命中即短路，不调用 LLM
%%
%% 对标 Spring AI SafeGuardAdvisor。命中时**不调 Next**，直接合成一个
%% finish_reason=content_filtered 的答复返回——不产生 tool_calls，故工具循环
%% 见之即正常收尾。
%%
%% 放 chat 链（而非 turn 链）与 Spring 的层序一致（其 SafeGuardAdvisor order=0
%% 落在 ToolCallingAdvisor 之内），效果是**循环内每次 LLM 调用都过一遍**：不止
%% 拦用户输入，工具结果回灌时带出的敏感内容同样拦得住。
%%
%% Opts：
%% - `failure_response` —— 命中时的答复文本（缺省见 ?DEFAULT_SAFEGUARD_RESPONSE）
%% - `case_sensitive` —— 是否区分大小写（**缺省 false**）
%%
%% **能力边界要认清**：这是子串匹配，不是内容安全。Spring 的实现是区分大小写的
%% `String.contains`（"bomb" 拦不住 "BOMB"），本实现缺省不区分大小写算是修了这一处；
%% 但变形、拼音、Unicode 同形字、跨消息拼接一概拦不住。真要做内容安全请接专门的
%% 审核模型，本 filter 只当粗筛与兜底。
-spec safeguard_filter([binary()]) -> beamai_filter:filter().
safeguard_filter(Words) ->
    safeguard_filter(Words, #{}).

-spec safeguard_filter([binary()], map()) -> beamai_filter:filter().
safeguard_filter(Words, Opts) when is_list(Words), is_map(Opts) ->
    Failure = maps:get(failure_response, Opts, ?DEFAULT_SAFEGUARD_RESPONSE),
    CaseSensitive = maps:get(case_sensitive, Opts, false),
    Patterns = case CaseSensitive of
        true -> Words;
        false -> [string:lowercase(W) || W <- Words]
    end,
    beamai_filter:new(<<"safeguard">>, #{
        around_chat => fun(#{messages := Msgs, context := Ctx} = Req, _FCtx, Next) ->
            case sensitive_hit(Patterns, Msgs, CaseSensitive) of
                true -> #{response => refusal_response(Failure), context => Ctx};
                false -> Next(Req)
            end
        end
    }).

%% @private 敏感词是否命中（全部消息文本拼一起后子串匹配）
sensitive_hit([], _Msgs, _CaseSensitive) ->
    false;
sensitive_hit(Patterns, Msgs, CaseSensitive) ->
    Text0 = iolist_to_binary([message_text(M) || M <- Msgs]),
    Text = case CaseSensitive of
        true -> Text0;
        false -> string:lowercase(Text0)
    end,
    lists:any(fun(<<>>) -> false;
                 (W) -> binary:match(Text, W) =/= nomatch
              end, Patterns).

%% @private 抽消息文本（多模态 content 列表只取 text 块；图片/音频等忽略）
message_text(#{content := C}) when is_binary(C) ->
    C;
message_text(#{content := Blocks}) when is_list(Blocks) ->
    iolist_to_binary([block_text(B) || B <- Blocks]);
message_text(_) ->
    <<>>.

%% @private 抽 content 块文本（兼容 atom / binary 键）
block_text(#{text := T}) when is_binary(T) -> T;
block_text(#{<<"text">> := T}) when is_binary(T) -> T;
block_text(_) -> <<>>.

%% @private 合成拦截答复（不经 LLM）
refusal_response(Text) ->
    beamai_llm_response:new(#{
        content => Text,
        finish_reason => content_filtered,
        metadata => #{safeguard => blocked}
    }).

%%====================================================================
%% tool 链
%%====================================================================

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
    new_validation_filter(<<"validation">>, ValidateFun, MaxRetries).

%% @doc 结构化输出 JSON Schema 校验 filter（turn 链）
%%
%% validation_turn_filter 的开箱即用特化：把「取文本 → 解 JSON → 过 Schema」
%% 组成 ValidateFun，不合格则把 Schema 错误当反馈重入循环。配 provider 原生
%% 结构化输出（json_schema）使用效果最好——原生约束负责大多数情形，本 filter
%% 兜住漏网的（尤其不支持原生结构化输出的 provider）。
%%
%% Schema 直接复用 beamai_tool 的参数 Schema 形态（atom 键亦可），校验器见
%% beamai_json_schema（DRAFT 2020-12 的实用子集）。
%%
%% Opts：
%% - `max_errors` —— 单次最多收集几条错误（缺省全收；错误全塞进反馈会撑爆
%%   提示词，字段多的 Schema 建议设 5~10）
%% - `code_fence` —— 是否剥离 ```json 围栏再解析（**缺省 true**）。模型即便被
%%   要求只输出 JSON 也爱裹围栏，Spring 靠原生结构化输出免了这一遭，我们没这
%%   前提，故缺省宽容。
%%
%% **与 Spring 的一处分歧**：Spring 把错误追加到原 user 消息文本上重发，故错误
%% 不跨重试累积；这里沿用 validation_turn_filter 的语义——反馈作为新 user 消息
%% **重入全新循环**（resume=false）。重试耗尽则原样返回最后一次（仍不合格的）
%% 响应，不抛错——与 Spring 一致，失败在下游解析时才浮现。
-spec schema_validation_turn_filter(beamai_json_schema:schema(), non_neg_integer()) ->
    beamai_filter:filter().
schema_validation_turn_filter(Schema, MaxRetries) ->
    schema_validation_turn_filter(Schema, MaxRetries, #{}).

-spec schema_validation_turn_filter(beamai_json_schema:schema(), non_neg_integer(), map()) ->
    beamai_filter:filter().
schema_validation_turn_filter(Schema, MaxRetries, Opts)
  when is_map(Schema), is_integer(MaxRetries), MaxRetries >= 0, is_map(Opts) ->
    new_validation_filter(<<"schema_validation">>, schema_validator(Schema, Opts), MaxRetries).

%% @private 组装「取文本 → 解 JSON → 过 Schema」的 ValidateFun
schema_validator(Schema, Opts) ->
    Fence = maps:get(code_fence, Opts, true),
    ValOpts = maps:with([max_errors], Opts),
    fun(Response) ->
        case beamai_llm_response:content(Response) of
            Content when is_binary(Content), Content =/= <<>> ->
                validate_json(Schema, ValOpts, strip_fence(Fence, Content));
            _ ->
                {invalid, <<"响应没有文本内容，无法校验结构化输出"/utf8>>}
        end
    end.

%% @private 解 JSON 后过 Schema
validate_json(Schema, ValOpts, Text) ->
    case decode_json(Text) of
        {ok, Term} ->
            case beamai_json_schema:validate(Schema, Term, ValOpts) of
                ok -> ok;
                {error, Errors} -> {invalid, beamai_json_schema:error_message(Errors)}
            end;
        error ->
            {invalid, <<"输出不是合法 JSON"/utf8>>}
    end.

%% @private 宽容解码（json:decode 出错即非法 JSON）
decode_json(Text) ->
    try
        {ok, json:decode(Text)}
    catch
        _:_ -> error
    end.

%% @private 剥离 ```json ... ``` 围栏（无围栏则原样；仅取首个围栏内容）
%%
%% 换行不强求（```json{...}``` 也认），围栏内残留的空白交给 json:decode。
strip_fence(false, Text) ->
    Text;
strip_fence(true, Text) ->
    case re:run(Text, "```(?:json)?\\s*(.*?)```",
                [dotall, caseless, {capture, [1], binary}]) of
        {match, [Inner]} -> Inner;
        nomatch -> Text
    end.

%% @private 构造校验 turn filter（validation / schema_validation 共用）
new_validation_filter(Name, ValidateFun, MaxRetries) when is_function(ValidateFun, 1) ->
    beamai_filter:new(Name, #{
        around_turn => fun(Req, _FCtx, Next) ->
            validate_loop(Req, Next, ValidateFun, MaxRetries)
        end
    }).

%%====================================================================
%% token 链（token_transform，流式专用）
%%====================================================================

%% @doc token 脱敏 filter（token_transform）：无状态逐 token 正则替换
%%
%% **已知限制**：秘密被切在两个 chunk 之间时漏检（跨 chunk 检测需有状态
%% 缓冲，按需自写 token_transform 或用 hold_release_filter 整流后审）。
%% 只改送 sink 的出站流；最终归一化响应（memory 落库/turn 结果）不受影响。
-spec token_redact_filter(iodata(), binary()) -> beamai_filter:filter().
token_redact_filter(Pattern, Replacement) when is_binary(Replacement) ->
    {ok, MP} = re:compile(Pattern),
    beamai_filter:new(<<"token_redact">>, #{
        token_transform => #{
            step => fun(#{token := T} = TD, S) ->
                Redacted = re:replace(T, MP, Replacement,
                                      [global, {return, binary}]),
                {[TD#{token => Redacted}], S}
            end
        }
    }).

%% @doc 先审后放 filter（token_transform）：缓冲整流不外泄，完流时全文审查
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
        token_transform => #{
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
        {ok, Response, _TCM, _Iter, Messages} = R ->
            case ValidateFun(Response) of
                ok ->
                    R;
                {invalid, Reason} when Retries > 0 ->
                    Feedback = #{role => user, content => feedback_text(Reason)},
                    %% 重入：拿上一跑的**完整消息序列**接着走，并关掉 load_history
                    %% ——上下文完全由这里重建，不依赖记忆是否开启。
                    %%
                    %% 曾经只传 [Feedback]、指望 loop 载入历史把原问题带回来：
                    %% memory 开启时对，`memory => false` 时历史为空 → 模型只收到
                    %% 一句「上次没通过请修正」、原始问题丢失，于是胡编（真实模型
                    %% 实测确认）。而 memory=false + 结构化抽取恰恰是最自然的组合。
                    validate_loop(Req#{messages => Messages ++ [Feedback],
                                       load_history => false,
                                       resume => false},
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
