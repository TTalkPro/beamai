%%%-------------------------------------------------------------------
%%% @doc 函数定义：处理器、参数、超时、重试、Schema 生成
%%%
%%% 定义 Kernel 可调用的函数单元，支持：
%%% - 多种处理器形式（fun/1, fun/2, {M,F}, {M,F,A}）
%%% - 参数 Schema 声明与 JSON Schema 转换
%%% - 超时和重试策略
%%% - 生成 OpenAI / Anthropic 格式的 tool schema
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_function).

%% API
-export([new/2, new/3]).
-export([validate/1]).
-export([invoke/2, invoke/3]).
-export([to_tool_spec/1]).
-export([to_tool_schema/1, to_tool_schema/2]).
-export([get_name/1, get_full_name/1]).
-export([parse_tool_call/1, encode_result/1]).

%% Types
-export_type([function_def/0, handler/0, function_result/0,
              args/0, parameters_schema/0, param_spec/0]).

-type function_def() :: #{
    name := binary(),
    handler := handler(),
    description => binary(),
    parameters => parameters_schema(),
    return_type => return_schema(),
    plugin => binary(),
    timeout => pos_integer(),
    retry => #{max => integer(), delay => integer()},
    filters => [filter_ref()],
    metadata => map()
}.

-type handler() ::
    fun((args()) -> function_result())
    | fun((args(), beamai_context:t()) -> function_result())
    | {module(), atom()}
    | {module(), atom(), [term()]}.

-type function_result() ::
    {ok, term()}
    | {ok, term(), beamai_context:t()}
    | {error, term()}.

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

-type return_schema() :: #{
    type => atom(),
    description => binary()
}.

-type filter_ref() :: binary() | atom().

%%====================================================================
%% API
%%====================================================================

%% @doc 创建函数定义（最小形式）
%%
%% 仅指定名称和处理器，不包含描述、参数声明等可选信息。
%%
%% @param Name 函数名称（如 <<"get_weather">>）
%% @param Handler 函数处理器（fun/1、fun/2、{M,F} 或 {M,F,A}）
%% @returns 函数定义 Map
-spec new(binary(), handler()) -> function_def().
new(Name, Handler) ->
    #{name => Name, handler => Handler}.

%% @doc 创建函数定义（带额外选项）
%%
%% 通过 Opts 可指定 description、parameters、timeout、retry 等配置。
%% Opts 中的同名键会被 name/handler 覆盖。
%%
%% @param Name 函数名称
%% @param Handler 函数处理器
%% @param Opts 额外选项（如 #{description => ..., parameters => ...}）
%% @returns 函数定义 Map
-spec new(binary(), handler(), map()) -> function_def().
new(Name, Handler, Opts) ->
    maps:merge(Opts, #{name => Name, handler => Handler}).

%% @doc 验证函数定义的合法性
%%
%% 检查 name 必须为非空二进制，handler 必须是有效的处理器形式。
%% 缺少 name 或 handler 字段时返回 missing_required_fields 错误。
%%
%% @param FuncDef 待验证的函数定义
%% @returns ok 或 {error, 错误列表}
-spec validate(function_def()) -> ok | {error, [term()]}.
validate(#{name := Name, handler := Handler}) ->
    Errors = lists:flatten([
        validate_name(Name),
        validate_handler(Handler)
    ]),
    case Errors of
        [] -> ok;
        _ -> {error, Errors}
    end;
validate(_) ->
    {error, [missing_required_fields]}.

%% @doc 调用函数（使用空上下文）
%%
%% 等价于 invoke(FuncDef, Args, beamai_context:new())。
%%
%% @param FuncDef 函数定义
%% @param Args 调用参数 Map
%% @returns {ok, 结果} | {ok, 结果, 新上下文} | {error, 原因}
-spec invoke(function_def(), args()) -> function_result().
invoke(FuncDef, Args) ->
    invoke(FuncDef, Args, beamai_context:new()).

%% @doc 调用函数（带上下文）
%%
%% 根据函数定义中的 timeout 和 retry 配置执行处理器。
%% 默认超时 30 秒，默认不重试。
%%
%% @param FuncDef 函数定义
%% @param Args 调用参数 Map
%% @param Context 执行上下文
%% @returns {ok, 结果} | {ok, 结果, 新上下文} | {error, 原因}
-spec invoke(function_def(), args(), beamai_context:t()) -> function_result().
invoke(#{handler := Handler} = FuncDef, Args, Context) ->
    Timeout = maps:get(timeout, FuncDef, 30000),
    RetryConf = maps:get(retry, FuncDef, #{max => 0, delay => 0}),
    invoke_with_retry(Handler, Args, Context, RetryConf, Timeout).

%% @doc 将函数定义转换为统一 tool spec 格式
%%
%% 返回包含 name、description、parameters（JSON Schema）的中间表示，
%% 可进一步转换为各提供商格式。
%%
%% @param FuncDef 函数定义
%% @returns 统一 tool spec Map
-spec to_tool_spec(function_def()) -> map().
to_tool_spec(FuncDef) ->
    #{
        name => full_name(FuncDef),
        description => maps:get(description, FuncDef, <<"">>),
        parameters => build_json_schema(maps:get(parameters, FuncDef, #{}))
    }.

%% @doc 将函数定义转换为 OpenAI 格式的 tool schema（默认格式）
-spec to_tool_schema(function_def()) -> map().
to_tool_schema(FuncDef) ->
    to_tool_schema(FuncDef, openai).

%% @doc 将函数定义转换为指定提供商的 tool schema
%%
%% 支持 openai（含 ollama、zhipu、deepseek 等兼容格式）和 anthropic 格式。
%%
%% @param FuncDef 函数定义
%% @param Provider 提供商标识（openai | anthropic）
%% @returns 提供商格式的 tool schema Map
-spec to_tool_schema(function_def(), openai | anthropic | atom()) -> map().
to_tool_schema(FuncDef, Provider) ->
    ToolSpec = to_tool_spec(FuncDef),
    tool_spec_to_provider(ToolSpec, Provider).

%%--------------------------------------------------------------------
%% 提供商格式转换
%%--------------------------------------------------------------------

%% @private 根据提供商将统一 tool spec 转为对应格式
tool_spec_to_provider(ToolSpec, anthropic) ->
    to_anthropic_schema(ToolSpec);
tool_spec_to_provider(ToolSpec, _) ->
    to_openai_schema(ToolSpec).

%% @private 转换为 OpenAI function calling 格式
%% 同时适用于 ollama、zhipu、deepseek 等兼容 API
to_openai_schema(#{name := Name, description := Desc, parameters := Params}) ->
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => Name,
            <<"description">> => Desc,
            <<"parameters">> => Params
        }
    };
to_openai_schema(#{name := Name, description := Desc}) ->
    to_openai_schema(#{name => Name, description => Desc, parameters => default_params()});
to_openai_schema(#{name := Name}) ->
    to_openai_schema(#{name => Name, description => <<"Tool: ", Name/binary>>, parameters => default_params()}).

%% @private 转换为 Anthropic tool use 格式
to_anthropic_schema(#{name := Name, description := Desc, parameters := Params}) ->
    #{
        <<"name">> => Name,
        <<"description">> => Desc,
        <<"input_schema">> => Params
    };
to_anthropic_schema(#{name := Name, description := Desc}) ->
    to_anthropic_schema(#{name => Name, description => Desc, parameters => default_params()}).

%% @private 默认空参数 schema
default_params() ->
    #{type => object, properties => #{}, required => []}.

%% @doc 获取函数名称（不含插件前缀）
-spec get_name(function_def()) -> binary().
get_name(#{name := Name}) -> Name.

%% @doc 获取函数全名（含插件前缀）
%%
%% 若函数归属某插件，返回 <<"plugin.name">> 格式；否则返回 name。
-spec get_full_name(function_def()) -> binary().
get_full_name(FuncDef) -> full_name(FuncDef).

%%====================================================================
%% 工具调用协议
%%====================================================================

%% @doc 解析 LLM 返回的 tool_call 结构
%%
%% 支持 atom-key 和 binary-key 两种格式。
%% 返回 {Id, FunctionName, ParsedArgs}。
%%
-spec parse_tool_call(map()) -> {binary(), binary(), map()}.
parse_tool_call(TC) ->
    Id = extract_id(TC),
    Name = extract_name(TC),
    Args = extract_args(TC),
    {Id, Name, Args}.

%% @doc 将函数执行结果编码为 LLM 可读的二进制
-spec encode_result(term()) -> binary().
encode_result(Value) when is_binary(Value) -> Value;
encode_result(Value) when is_map(Value) ->
    try jsx:encode(Value)
    catch _:_ -> iolist_to_binary(io_lib:format("~p", [Value]))
    end;
encode_result(Value) when is_list(Value) ->
    try jsx:encode(Value)
    catch _:_ -> iolist_to_binary(io_lib:format("~p", [Value]))
    end;
encode_result(Value) when is_number(Value) ->
    iolist_to_binary(io_lib:format("~p", [Value]));
encode_result(Value) when is_atom(Value) ->
    atom_to_binary(Value, utf8);
encode_result(Value) ->
    iolist_to_binary(io_lib:format("~p", [Value])).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 拼接插件名和函数名为全限定名（plugin.name）
full_name(#{plugin := Plugin, name := Name}) ->
    <<Plugin/binary, ".", Name/binary>>;
full_name(#{name := Name}) ->
    Name.

%% @private 验证名称：必须为非空二进制
validate_name(Name) when is_binary(Name), byte_size(Name) > 0 -> [];
validate_name(_) -> [{invalid_name, <<"name must be a non-empty binary">>}].

%% @private 验证处理器：必须为 fun/1、fun/2、{M,F} 或 {M,F,A}
validate_handler(Fun) when is_function(Fun, 1) -> [];
validate_handler(Fun) when is_function(Fun, 2) -> [];
validate_handler({M, F}) when is_atom(M), is_atom(F) -> [];
validate_handler({M, F, A}) when is_atom(M), is_atom(F), is_list(A) -> [];
validate_handler(_) -> [{invalid_handler, <<"handler must be fun/1, fun/2, {M,F}, or {M,F,A}">>}].

%% @private 带重试策略的函数调用入口
%% 从 retry 配置中解析 max 和 delay，委托给递归实现
invoke_with_retry(Handler, Args, Context, #{max := Max, delay := Delay}, Timeout) ->
    invoke_with_retry(Handler, Args, Context, Max, Delay, Timeout).

%% @private 带重试策略的函数调用递归实现
%% 失败时等待 Delay 毫秒后重试，直到 RetriesLeft 为 0
invoke_with_retry(Handler, Args, Context, RetriesLeft, Delay, Timeout) ->
    case call_handler(Handler, Args, Context, Timeout) of
        {error, _Reason} when RetriesLeft > 0 ->
            timer:sleep(Delay),
            invoke_with_retry(Handler, Args, Context, RetriesLeft - 1, Delay, Timeout);
        Result ->
            Result
    end.

%% @private 调用处理器：fun/1 形式（仅接收参数）
call_handler(Fun, Args, _Context, _Timeout) when is_function(Fun, 1) ->
    try Fun(Args)
    catch Class:Reason:Stack ->
        {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end;
%% @private 调用处理器：fun/2 形式（接收参数和上下文）
call_handler(Fun, Args, Context, _Timeout) when is_function(Fun, 2) ->
    try Fun(Args, Context)
    catch Class:Reason:Stack ->
        {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end;
%% @private 调用处理器：{Module, Function} 形式
call_handler({M, F}, Args, Context, _Timeout) ->
    try M:F(Args, Context)
    catch Class:Reason:Stack ->
        {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end;
%% @private 调用处理器：{Module, Function, ExtraArgs} 形式
call_handler({M, F, ExtraArgs}, Args, Context, _Timeout) ->
    try erlang:apply(M, F, [Args, Context | ExtraArgs])
    catch Class:Reason:Stack ->
        {error, #{class => Class, reason => Reason, stacktrace => Stack}}
    end.

%% @private 将参数声明转换为 JSON Schema 格式
%% 空参数返回默认的 object schema
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
%% 使用管道模式逐步添加可选字段，避免级联嵌套
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
%% float 映射为 number，其余同名
type_to_schema(string) -> string;
type_to_schema(integer) -> integer;
type_to_schema(float) -> number;
type_to_schema(boolean) -> boolean;
type_to_schema(array) -> array;
type_to_schema(object) -> object;
type_to_schema(Other) -> Other.

%% @private 提取 tool_call 的 ID
extract_id(#{id := Id}) -> Id;
extract_id(#{<<"id">> := Id}) -> Id;
extract_id(_) -> <<"unknown">>.

%% @private 提取 tool_call 的函数名
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
%% 使用 binary 标签避免动态创建 atom，防止 atom 表溢出攻击
parse_args(Args) when is_map(Args) -> Args;
parse_args(Args) when is_binary(Args) ->
    try jsx:decode(Args, [return_maps])
    catch _:_ -> #{<<"raw">> => Args}
    end;
parse_args(_) -> #{}.

