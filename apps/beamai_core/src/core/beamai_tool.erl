%%%-------------------------------------------------------------------
%%% @doc 工具定义：处理器、参数、超时、重试、Schema 生成
%%%
%%% 定义 ChatClient 可调用的工具单元，支持：
%%% - 多种处理器形式（fun/1, fun/2, {M,F}, {M,F,A}）
%%% - 参数 Schema 声明与 JSON Schema 转换
%%% - 超时和重试策略
%%% - 生成 OpenAI / Anthropic 格式的 tool schema
%%% - tag 字段用于工具分组
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool).

%% handler 执行的内置缺省：**不设超时**。
%%
%% 框架不替用户决定何时放弃一个工具——它无从知道某个调用「多久算太久」。
%% 只有用户声明了超时元信息（tool_spec 的 timeout，或 ToolCallingManager 构造时
%% 的 tool_timeout）才真的限时。缺省下工具跑多久就等多久，要不要杀由用户决定。
-define(DEFAULT_TOOL_TIMEOUT, infinity).

%% API - 创建
-export([new/2, new/3]).
-export([validate/1]).

%% API - 调用
-export([invoke/2, invoke/3]).

%% API - Schema 转换
-export([to_tool_spec/1]).
-export([to_tool_schema/1, to_tool_schema/2]).

%% API - 查询
-export([get_name/1]).
-export([get_tag/1, has_tag/2]).
-export([is_serial/1]).
-export([is_sensitive/1]).
-export([is_return_direct/1]).

%% API - 工具调用协议
-export([parse_tool_call/1, encode_result/1, result_content/1, result_text/1]).
-export([redact_result/2, result_size/2]).

%% API - 从模块加载
-export([from_module/1]).

%% Types
-export_type([tool_spec/0, handler/0, tool_result/0,
              args/0, parameters_schema/0, param_spec/0, content_part/0,
              result_content/0]).

-type tool_spec() :: #{
    name := binary(),
    handler := handler(),
    description => binary(),
    parameters => parameters_schema(),
    tag => binary() | [binary()],
    %% timeout：handler 执行的硬上限（毫秒），**缺省 infinity（不限时）**。
    %% 声明了才限时——到点 kill 执行进程、归一为 transient 的 tool_timeout 错误。
    %% 不声明即无限等待：框架不替调用者判断「多久算太久」。
    timeout => pos_integer() | infinity,
    %% retry：仅对 transient 类错误重试（见 beamai_tool_error），指数退避。
    %% true 用缺省 #{max_retries=>2, initial_delay_ms=>200}；opt-in 即承诺幂等。
    %% 兼容旧 #{max, delay} 形态。
    retry => boolean()
           | #{max_retries => non_neg_integer(), initial_delay_ms => non_neg_integer()}
           | #{max => integer(), delay => integer()},
    %% serial：标记有副作用、需顺序执行的工具。批内任一工具 serial → 整批退化串行
    %% （副作用要顺序，部分并行会改变相对时序）。缺省 false。
    serial => boolean(),
    %% sensitive：标记敏感工具，beamai_filters:approval_filter 据此拦截审批。缺省 false。
    sensitive => boolean(),
    %% strict_args=true 时，执行前按 parameters 生成 JSON Schema 校验入参。
    %% 缺省 false，保持既有工具零迁移成本；新工具建议开启。
    strict_args => boolean(),
    %% 执行权限声明（当前是元数据/审计约定；运行时 filter 可据此拦截）。
    permissions => #{
        filesystem => boolean(),
        network => boolean(),
        process => boolean(),
        env => boolean()
    },
    %% 工具结果送回模型前的硬上限（按 JSON/文本编码后的 byte size）。
    max_result_bytes => pos_integer(),
    %% 结果 redaction：正则 + replacement，顺序应用于 encode_result 后的文本。
    result_redaction => [{binary() | string(), binary()}],
    %% return_direct：工具结果**直接作为最终答案**返回，不再回灌模型续跑（对标
    %% Spring AI ToolExecutionResult.returnDirect）。缺省 false。
    %% 语义为整批 AND：一批 tool_calls 全部标注才直返（见 beamai_agent_tool_loop），
    %% 混批时任一未标注即照常回灌——否则未标注工具的结果会被静默丢弃。
    return_direct => boolean(),
    filters => [filter_ref()],
    metadata => map()
}.

-type handler() ::
    fun((args()) -> tool_result())
    | fun((args(), beamai_context:t()) -> tool_result())
    | {module(), atom()}
    | {module(), atom(), [term()]}.

%% 工具返回：`{ok, V}` 不写状态；`{ok, V, Writes}` 携带写意图（纯数据 map，
%% 由 tool 批次在屏障处按原始序折叠进 state，见 beamai_context:apply_writes/3）；
%% `{error, R}` 失败（不参与折叠）。第三元语义为 **Writes**，非可变 context——
%% context 是只读运行环境（见 design/context_split_parallel_tools.md）。
-type tool_result() ::
    {ok, term()}
    | {ok, term(), beamai_context:writes()}
    | {error, term()}.

%% 多模态工具结果部件。`{parts, Parts}` 是 handler 返回值的显式标记：
%% 当前回合发给模型时保留图片等内容部件；持久化 / 直返 / 进度回调用
%% result_text/1 只提取文本，避免截图这类单步有效的负载进入历史。
-type content_part() :: #{type := atom() | binary(), _ => _}.
-type result_content() :: binary() | [content_part()].

-type args() :: map().

-type parameters_schema() :: #{
    atom() | binary() => param_spec()
}.

-type param_spec() :: #{
    type := string | integer | float | boolean | array | object,
    description => binary(),
    required => boolean(),
    default => term(),
    enum => [term()],
    items => param_spec(),
    properties => parameters_schema()
}.

-type filter_ref() :: binary() | atom().

%%====================================================================
%% API - 创建
%%====================================================================

%% @doc 创建工具定义（最小形式）
%%
%% 仅指定名称和处理器，不包含描述、参数声明等可选信息。
%%
%% @param Name 工具名称（如 <<"get_weather">>）
%% @param Handler 处理器（fun/1、fun/2、{M,F} 或 {M,F,A}）
%% @returns 工具定义 Map
-spec new(binary(), handler()) -> tool_spec().
new(Name, Handler) ->
    #{name => Name, handler => Handler}.

%% @doc 创建工具定义（带额外选项）
%%
%% 通过 Opts 可指定 description、parameters、tag、timeout、retry 等配置。
%%
%% @param Name 工具名称
%% @param Handler 处理器
%% @param Opts 额外选项（如 #{description => ..., parameters => ..., tag => ...}）
%% @returns 工具定义 Map
-spec new(binary(), handler(), map()) -> tool_spec().
new(Name, Handler, Opts) ->
    maps:merge(Opts, #{name => Name, handler => Handler}).

%% @doc 验证工具定义的合法性
%%
%% 检查 name 必须为非空二进制，handler 必须是有效的处理器形式。
%%
%% @param ToolSpec 待验证的工具定义
%% @returns ok 或 {error, 错误列表}
-spec validate(tool_spec()) -> ok | {error, [term()]}.
validate(#{name := Name, handler := Handler} = ToolSpec) ->
    Errors = lists:flatten([
        validate_name(Name),
        validate_handler(Handler),
        validate_optional(maps:get(strict_args, ToolSpec, undefined),
                          fun validate_boolean/1, invalid_strict_args),
        validate_permissions(maps:get(permissions, ToolSpec, undefined)),
        validate_optional(maps:get(max_result_bytes, ToolSpec, undefined),
                          fun validate_pos_integer/1, invalid_max_result_bytes),
        validate_redaction(maps:get(result_redaction, ToolSpec, undefined))
    ]),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end;
validate(_) ->
    {error, [missing_required_fields]}.

%%====================================================================
%% API - 调用
%%====================================================================

%% @doc 调用工具（使用空上下文）
-spec invoke(tool_spec(), args()) -> tool_result().
invoke(ToolSpec, Args) ->
    invoke(ToolSpec, Args, beamai_context:new()).

%% @doc 调用工具（带上下文）
%%
%% Context 为**只读运行环境**（env：ChatClient/conversation_id/vars）；handler 若
%% 需写状态经返回值 `{ok, V, Writes}` 表达，不改 Context。
%% 根据工具定义中的 timeout 和 retry 配置执行处理器。
%% **缺省不限时**——只有声明了 timeout 才强制执行（handler 跑在受监控子进程中，
%% 到点 kill）。默认不重试。超时归为 transient 类——声明了 retry 的工具会被重试。
%%
%% handler 在子进程中执行，故**不得依赖调用者的进程身份**（self()、进程字典、
%% 发往调用者的消息）。并发路径下本就如此，这里只是让所有路径一致。
-spec invoke(tool_spec(), args(), beamai_context:t()) -> tool_result().
invoke(#{handler := Handler} = ToolSpec, Args, Context) ->
    case validate_args(ToolSpec, Args) of
        ok ->
            Timeout = resolve_timeout(ToolSpec, Context),
            {MaxRetries, InitialDelay} = retry_conf(maps:get(retry, ToolSpec, false)),
            invoke_with_retry(Handler, Args, Context, MaxRetries, InitialDelay, Timeout);
        {error, Errors} ->
            {error, #{error_class => semantic,
                      reason => invalid_tool_args,
                      errors => Errors,
                      stacktrace => []}}
    end.

%% @private 超时优先级：工具自己声明 > ToolCallingManager 注入的缺省 > 不限时
%%
%% 工具声明最优先——它最了解自己要跑多久。manager 级缺省次之，让部署方能整体
%% 收紧而不必逐个改工具。两者都没声明 → infinity，无限等待。
resolve_timeout(ToolSpec, Context) ->
    case maps:get(timeout, ToolSpec, undefined) of
        undefined ->
            case beamai_context:default_tool_timeout(Context) of
                undefined -> ?DEFAULT_TOOL_TIMEOUT;
                Timeout -> Timeout
            end;
        Declared ->
            Declared
    end.

%%====================================================================
%% API - Schema 转换
%%====================================================================

%% @doc 将工具定义转换为统一 tool spec 格式
%%
%% 返回包含 name、description、parameters（JSON Schema）的中间表示。
-spec to_tool_spec(tool_spec()) -> map().
to_tool_spec(ToolSpec) ->
    #{
        name => maps:get(name, ToolSpec),
        description => maps:get(description, ToolSpec, <<"">>),
        parameters => build_json_schema(maps:get(parameters, ToolSpec, #{}))
    }.

%% @doc 将工具定义转换为 OpenAI 格式的 tool schema（默认格式）
-spec to_tool_schema(tool_spec()) -> map().
to_tool_schema(ToolSpec) ->
    to_tool_schema(ToolSpec, openai).

%% @doc 将工具定义转换为指定提供商的 tool schema
%%
%% 支持 openai（含 ollama、zhipu、deepseek 等兼容格式）和 anthropic 格式。
-spec to_tool_schema(tool_spec(), openai | anthropic | atom()) -> map().
to_tool_schema(ToolSpec, Provider) ->
    Spec = to_tool_spec(ToolSpec),
    tool_spec_to_provider(Spec, Provider).

%%--------------------------------------------------------------------
%% 提供商格式转换
%%--------------------------------------------------------------------

%% @private 根据提供商将统一 tool spec 转为对应格式
tool_spec_to_provider(ToolSpec, anthropic) ->
    to_anthropic_schema(ToolSpec);
tool_spec_to_provider(ToolSpec, _) ->
    to_openai_schema(ToolSpec).

%% @private 转换为 OpenAI function calling 格式
to_openai_schema(#{name := Name, description := Desc, parameters := Params}) ->
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => Name,
            <<"description">> => Desc,
            <<"parameters">> => Params
        }
    }.

%% @private 转换为 Anthropic tool use 格式
to_anthropic_schema(#{name := Name, description := Desc, parameters := Params}) ->
    #{
        <<"name">> => Name,
        <<"description">> => Desc,
        <<"input_schema">> => Params
    }.

%%====================================================================
%% API - 查询
%%====================================================================

%% @doc 获取工具名称
-spec get_name(tool_spec()) -> binary().
get_name(#{name := Name}) -> Name.

%% @doc 获取工具标签
-spec get_tag(tool_spec()) -> binary() | [binary()] | undefined.
get_tag(ToolSpec) ->
    maps:get(tag, ToolSpec, undefined).

%% @doc 检查工具是否包含指定标签
-spec has_tag(tool_spec(), binary()) -> boolean().
has_tag(#{tag := Tags}, Tag) when is_list(Tags) ->
    lists:member(Tag, Tags);
has_tag(#{tag := ToolTag}, Tag) ->
    ToolTag =:= Tag;
has_tag(_, _) ->
    false.

%% @doc 工具是否标记为串行（有副作用、需顺序执行）
-spec is_serial(tool_spec()) -> boolean().
is_serial(#{serial := true}) -> true;
is_serial(_) -> false.

%% @doc 工具是否标记为敏感（需审批；approval_filter 据此拦截）
-spec is_sensitive(tool_spec()) -> boolean().
is_sensitive(#{sensitive := true}) -> true;
is_sensitive(_) -> false.

%% @doc 工具结果是否直接作为最终答案返回（不回灌模型续跑）
-spec is_return_direct(tool_spec()) -> boolean().
is_return_direct(#{return_direct := true}) -> true;
is_return_direct(_) -> false.

%%====================================================================
%% API - 工具调用协议
%%====================================================================

%% @doc 解析 LLM 返回的 tool_call 结构
%%
%% 支持 atom-key 和 binary-key 两种格式。
%% 返回 {Id, ToolName, ParsedArgs}。
-spec parse_tool_call(map()) -> {binary(), binary(), map()}.
parse_tool_call(TC) ->
    Id = extract_id(TC),
    Name = extract_name(TC),
    Args = extract_args(TC),
    {Id, Name, Args}.

%% @doc 对结果文本应用 redaction 规则
-spec redact_result(term(), [{binary() | string(), binary()}]) ->
    {ok, binary() | {parts, [content_part()]}} |
    {error, {invalid_redaction_pattern, term()}}.
redact_result(Value, []) ->
    {ok, result_content(Value)};
redact_result(Value, Rules) when is_list(Rules) ->
    case result_content(Value) of
        Parts when is_list(Parts) ->
            {ok, {parts, redact_parts(Parts, Rules)}};
        Content when is_binary(Content) ->
            apply_redaction(Content, Rules)
    end.

redact_parts(Parts, Rules) ->
    [case Part of
         #{type := text, text := Text} = P ->
             case apply_redaction(Text, Rules) of
                 {ok, Redacted} -> P#{text => Redacted};
                 {error, _} = Error -> exit(Error)
             end;
         Other ->
             Other
     end || Part <- Parts].

%% @doc 将工具执行结果编码为 LLM 可读的二进制
-spec encode_result(term()) -> binary().
encode_result(Value) when is_binary(Value) -> Value;
encode_result(Value) when is_map(Value) ->
    try beamai_utils:encode_json(Value)
    catch _:_ -> iolist_to_binary(io_lib:format("~p", [Value]))
    end;
encode_result(Value) when is_list(Value) ->
    try beamai_utils:encode_json(Value)
    catch _:_ -> iolist_to_binary(io_lib:format("~p", [Value]))
    end;
encode_result(Value) when is_number(Value) ->
    iolist_to_binary(io_lib:format("~p", [Value]));
encode_result(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
encode_result(Value) ->
    iolist_to_binary(io_lib:format("~p", [Value])).

%% @doc 提取将要放进 tool 消息 content 的负载
%%
%% handler 返回 `{parts, Parts}` 且 Parts 是非空部件列表时保留结构；
%% 其它返回值保持 encode_result/1 的文本契约。
-spec result_content(term()) -> result_content().
result_content({parts, Parts}) when is_list(Parts), Parts =/= [] ->
    Parts;
result_content(Value) ->
    encode_result(Value).

%% @doc 提取工具结果的人类可读文本（多模态结果只取 text 部件）
%%
%% 用于持久化、return_direct 合成答案和进度回调：截图只在当前这一次
%% LLM 调用有效，不应作为长期历史或事件负载反复复制。
-spec result_text(result_content() | term()) -> binary().
result_text({parts, Parts}) when is_list(Parts) ->
    texts_of_parts(Parts);
result_text(Parts) when is_list(Parts) ->
    texts_of_parts(Parts);
result_text(Content) when is_binary(Content) ->
    Content;
result_text(Value) ->
    encode_result(Value).

texts_of_parts(Parts) ->
    Texts = [Text || #{type := text, text := Text} <- Parts, is_binary(Text)],
    iolist_to_binary(lists:join(<<"\n">>, Texts)).

%% @doc 检查编码后的结果大小是否超过上限
-spec result_size(term(), pos_integer() | undefined) ->
    ok | {error, tool_result_too_large, non_neg_integer(), pos_integer()}.
result_size(Value, undefined) ->
    _ = result_content(Value),
    ok;
result_size(Value, MaxBytes) when is_integer(MaxBytes), MaxBytes > 0 ->
    Encoded = encoded_result_size(result_content(Value)),
    case byte_size(Encoded) of
        Size when Size =< MaxBytes -> ok;
        Size -> {error, tool_result_too_large, Size, MaxBytes}
    end.

encoded_result_size(Content) when is_binary(Content) ->
    Content;
encoded_result_size(Parts) when is_list(Parts) ->
    iolist_to_binary([encode_result(Part) || Part <- Parts]).

%%====================================================================
%% API - 从模块加载
%%====================================================================

%% @doc 从模块自动加载工具列表
%%
%% 模块需实现 beamai_tool_behaviour，至少实现 tools/0 回调。
%%
%% @param Module 实现了工具回调的模块
%% @returns {ok, [tool_spec()]} | {error, Reason}
-spec from_module(module()) -> {ok, [tool_spec()]} | {error, term()}.
from_module(Module) ->
    try
        Tools = Module:tools(),
        %% 如果模块实现了 tool_info/0，提取默认 tags
        DefaultTags = case erlang:function_exported(Module, tool_info, 0) of
            true ->
                Info = Module:tool_info(),
                maps:get(tags, Info, []);
            false ->
                []
        end,
        %% 为没有 tag 的工具添加默认 tags
        TaggedTools = [maybe_add_default_tags(T, DefaultTags) || T <- Tools],
        {ok, TaggedTools}
    catch
        error:undef ->
            {error, {module_not_found, Module}};
        Class:Reason ->
            {error, {Class, Reason}}
    end.

%% @private 为没有 tag 的工具添加默认 tags
maybe_add_default_tags(#{tag := _} = Tool, _DefaultTags) ->
    Tool;
maybe_add_default_tags(Tool, []) ->
    Tool;
maybe_add_default_tags(Tool, DefaultTags) ->
    Tool#{tag => DefaultTags}.

%%====================================================================
%% 内部函数 - 验证
%%====================================================================

%% @private 验证名称：必须为非空二进制
validate_name(Name) when is_binary(Name), byte_size(Name) > 0 -> [];
validate_name(_) -> [{invalid_name, <<"name must be a non-empty binary">>}].

%% @private 验证处理器：必须为 fun/1、fun/2、{M,F} 或 {M,F,A}
validate_handler(Fun) when is_function(Fun, 1) -> [];
validate_handler(Fun) when is_function(Fun, 2) -> [];
validate_handler({M, F}) when is_atom(M), is_atom(F) -> [];
validate_handler({M, F, A}) when is_atom(M), is_atom(F), is_list(A) -> [];
validate_handler(_) -> [{invalid_handler, <<"handler must be fun/1, fun/2, {M,F}, or {M,F,A}">>}].

validate_optional(undefined, _Validator, _Tag) -> [];
validate_optional(Value, Validator, Tag) ->
    case Validator(Value) of
        true -> [];
        false -> [{Tag, Value}]
    end.

validate_boolean(Value) -> is_boolean(Value).

validate_pos_integer(Value) -> is_integer(Value) andalso Value > 0.

validate_permissions(undefined) -> [];
validate_permissions(Permissions) when is_map(Permissions) ->
    AllowedKeys = [filesystem, network, process, env],
    BadKeys = maps:keys(Permissions) -- AllowedKeys,
    BadValues = [{K, V} || {K, V} <- maps:to_list(Permissions),
                           lists:member(K, AllowedKeys), not is_boolean(V)],
    case {BadKeys, BadValues} of
        {[], []} -> [];
        _ -> [{invalid_permissions, #{unknown_keys => BadKeys,
                                      invalid_values => BadValues}}]
    end;
validate_permissions(Value) -> [{invalid_permissions, Value}].

validate_redaction(undefined) -> [];
validate_redaction(Rules) when is_list(Rules) ->
    Bad = [Rule || Rule <- Rules, not validate_redaction_rule(Rule)],
    case Bad of
        [] -> [];
        _ -> [{invalid_result_redaction, Bad}]
    end;
validate_redaction(Value) -> [{invalid_result_redaction, Value}].

validate_redaction_rule({Pattern, Replacement}) when
      (is_binary(Pattern) orelse is_list(Pattern)), is_binary(Replacement) ->
    case re:compile(Pattern) of
        {ok, _} -> true;
        {error, _} -> false
    end;
validate_redaction_rule(_) -> false.

%% @private 执行前的参数 schema 校验
validate_args(#{strict_args := true} = ToolSpec, Args) when is_map(Args) ->
    Schema = build_json_schema(maps:get(parameters, ToolSpec, #{})),
    case beamai_json_schema:validate(Schema, Args) of
        ok -> ok;
        {error, Errors} -> {error, Errors}
    end;
validate_args(#{strict_args := true}, Args) when not is_map(Args) ->
    {error, [args_not_object]};
validate_args(_, _) ->
    ok.

apply_redaction(Text, []) ->
    {ok, Text};
apply_redaction(Text, [{Pattern, Replacement} | Rest]) ->
    case re:compile(Pattern) of
        {ok, MP} ->
            Redacted = re:replace(Text, MP, Replacement,
                                  [global, {return, binary}]),
            apply_redaction(Redacted, Rest);
        {error, {Reason, _}} ->
            {error, {invalid_redaction_pattern, Reason}}
    end.

%%====================================================================
%% 内部函数 - 调用
%%====================================================================

%% @private 解析 retry 配置为 {MaxRetries, InitialDelayMs}
%%   false / 未配置          → {0, 0}（不重试）
%%   true                    → {2, 200}（缺省）
%%   #{max_retries, initial_delay_ms}
%%   #{max, delay}（旧形态，兼容）
retry_conf(false) -> {0, 0};
retry_conf(true) -> {2, 200};
retry_conf(#{max_retries := M} = C) -> {M, maps:get(initial_delay_ms, C, 200)};
retry_conf(#{max := M} = C) -> {M, maps:get(delay, C, 0)};
retry_conf(_) -> {0, 0}.

%% @private 带重试策略的工具调用递归实现
%%
%% **仅对 transient 类错误重试**（超时/网络抖动等，见 beamai_tool_error），
%% 指数退避（Delay, 2x, 4x…）。语义/环境类错误不重试（重试无用或有害）。
%% opt-in retry 即承诺工具幂等。
invoke_with_retry(Handler, Args, Context, RetriesLeft, Delay, Timeout) ->
    case call_handler(Handler, Args, Context, Timeout) of
        {error, Reason} when RetriesLeft > 0 ->
            case beamai_tool_error:classify(Reason) of
                transient ->
                    timer:sleep(Delay),
                    invoke_with_retry(Handler, Args, Context, RetriesLeft - 1,
                                      Delay * 2, Timeout);
                _ ->
                    {error, Reason}
            end;
        Result ->
            Result
    end.

%% @private 调用处理器：在受监控的子进程中执行，强制 timeout
%%
%% **为什么必须起进程**：BEAM 无法中断同进程内的内联调用——要让 `timeout' 声明
%% 真正可执行，handler 就得跑在一个可以 kill 的进程里。try/catch 与时间无关，
%% 挡不住卡死的 handler。
%%
%% 这不与「隔离在 manager 级」冲突（见 beamai_tool_batch_worker）：那一层的
%% 职责是保护**调用者**、故障单元是「批」；这里的子进程是 timeout 的**执行
%% 机制**，故障单元是「单个工具」。串行语义不受影响——串行路径依然是起一个、
%% 等一个、再起下一个。
%%
%% 顺带收窄故障单元：工具内 spawn_link 的子进程崩溃（退出信号绕过 try/catch）
%% 现在只打死该工具的子进程，经 DOWN 归一为该工具的 error，同批其它工具不受
%% 牵连。批级 worker 仍是兜底（filter 崩溃等 handler 之外的故障）。
call_handler(Handler, Args, Context, Timeout) ->
    Parent = self(),
    Tag = make_ref(),
    {Pid, MRef} = spawn_monitor(fun() ->
        Parent ! {Tag, apply_handler(Handler, Args, Context)}
    end),
    receive
        {Tag, Result} ->
            erlang:demonitor(MRef, [flush]),
            Result;
        {'DOWN', MRef, process, Pid, Reason} ->
            %% handler 自身 raise 的异常已被 apply_handler 的 try/catch 归一；
            %% 走到这里的是逃出 try/catch 的退出信号（如 link 传播、外部 kill）
            {error, #{class => exit, reason => Reason, stacktrace => []}}
    after Timeout ->
        erlang:demonitor(MRef, [flush]),
        exit(Pid, kill),
        drain_handler_reply(Tag),
        %% reason 用裸原子 tool_timeout：beamai_tool_error:classify/1 据此归为
        %% transient（超时重试有意义），毫秒数另置一键，不破坏分类模式匹配
        {error, #{class => timeout, reason => tool_timeout,
                  timeout_ms => Timeout, stacktrace => []}}
    end.

%% @private 丢弃被 kill 的 handler 进程留在信箱里的迟到回复（Tag 唯一，不误伤）
drain_handler_reply(Tag) ->
    receive
        {Tag, _} -> ok
    after 0 -> ok
    end.

%% @private 四种 handler 形态的实际调用（跑在子进程内）
%%
%% try/catch 留在子进程内，保持既有错误形状 #{class, reason, stacktrace} 不变。
apply_handler(Fun, Args, _Context) when is_function(Fun, 1) ->
    try Fun(Args)
    catch Class:Reason:Stack ->
        {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end;
apply_handler(Fun, Args, Context) when is_function(Fun, 2) ->
    try Fun(Args, Context)
    catch Class:Reason:Stack ->
        {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end;
apply_handler({M, F}, Args, Context) ->
    try M:F(Args, Context)
    catch Class:Reason:Stack ->
        {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end;
apply_handler({M, F, ExtraArgs}, Args, Context) ->
    try erlang:apply(M, F, [Args, Context | ExtraArgs])
    catch Class:Reason:Stack ->
        {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end.

%%====================================================================
%% 内部函数 - JSON Schema 构建
%%====================================================================

%% @private 将参数声明转换为 JSON Schema 格式
build_json_schema(Params) when map_size(Params) =:= 0 ->
    #{type => object, properties => #{}, required => []};
build_json_schema(Params) ->
    Properties = maps:fold(fun(K, Spec, Acc) ->
        Key = beamai_utils:to_binary(K),
        Acc#{Key => param_to_json_schema(Spec)}
    end, #{}, Params),
    Required = maps:fold(fun(K, #{required := true}, Acc) ->
        [beamai_utils:to_binary(K) | Acc];
    (_, _, Acc) ->
        Acc
    end, [], Params),
    #{type => object, properties => Properties, required => Required}.

%% @private 将单个参数 spec 转换为 JSON Schema 属性
param_to_json_schema(#{type := Type} = Spec) ->
    Base = #{type => type_to_schema(Type)},
    OptionalFields = [
        {description, fun(V) -> #{description => V} end},
        {enum, fun(V) -> #{enum => V} end},
        {items, fun(V) -> #{items => param_to_json_schema(V)} end},
        {properties, fun(V) ->
            SubSchema = build_json_schema(V),
            #{properties => maps:get(properties, SubSchema)}
        end}
    ],
    lists:foldl(fun({Key, Transform}, Acc) ->
        case maps:find(Key, Spec) of
            {ok, Val} -> maps:merge(Acc, Transform(Val));
            error -> Acc
        end
    end, Base, OptionalFields).

%% @private 内部类型到 JSON Schema 类型的映射
type_to_schema(string) -> string;
type_to_schema(integer) -> integer;
type_to_schema(float) -> number;
type_to_schema(boolean) -> boolean;
type_to_schema(array) -> array;
type_to_schema(object) -> object;
type_to_schema(Other) -> Other.

%%====================================================================
%% 内部函数 - tool_call 解析
%%====================================================================

%% @private 提取 tool_call 的 ID
extract_id(#{id := Id}) -> Id;
extract_id(#{<<"id">> := Id}) -> Id;
extract_id(_) -> <<"unknown">>.

%% @private 提取 tool_call 的工具名
extract_name(#{function := #{name := N}}) -> N;
extract_name(#{<<"function">> := #{<<"name">> := N}}) -> N;
extract_name(#{name := N}) -> N;
extract_name(_) -> <<"unknown">>.

%% @private 提取 tool_call 的参数
extract_args(#{function := #{arguments := A}}) -> parse_args(A);
extract_args(#{<<"function">> := #{<<"arguments">> := A}}) -> parse_args(A);
extract_args(#{arguments := A}) -> parse_args(A);
extract_args(_) -> #{}.

%% @private 解析参数（JSON 字符串或已解码的 map）
parse_args(Args) when is_map(Args) -> Args;
parse_args(Args) when is_binary(Args) ->
    try json:decode(Args)
    catch _:_ -> #{<<"raw">> => Args}
    end;
parse_args(_) -> #{}.
