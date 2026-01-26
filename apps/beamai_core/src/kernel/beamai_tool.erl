%%%-------------------------------------------------------------------
%%% @doc 工具定义：处理器、参数、超时、重试、Schema 生成
%%%
%%% 定义 Kernel 可调用的工具单元，支持：
%%% - 多种处理器形式（fun/1, fun/2, {M,F}, {M,F,A}）
%%% - 参数 Schema 声明与 JSON Schema 转换
%%% - 超时和重试策略
%%% - 生成 OpenAI / Anthropic 格式的 tool schema
%%% - tag 字段用于工具分组
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool).

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

%% API - 工具调用协议
-export([parse_tool_call/1, encode_result/1]).

%% API - 从模块加载
-export([from_module/1]).

%% Types
-export_type([tool_spec/0, handler/0, tool_result/0,
              args/0, parameters_schema/0, param_spec/0]).

-type tool_spec() :: #{
    name := binary(),
    handler := handler(),
    description => binary(),
    parameters => parameters_schema(),
    tag => binary() | [binary()],
    timeout => pos_integer(),
    retry => #{max => integer(), delay => integer()},
    filters => [filter_ref()],
    metadata => map()
}.

-type handler() ::
    fun((args()) -> tool_result())
    | fun((args(), beamai_context:t()) -> tool_result())
    | {module(), atom()}
    | {module(), atom(), [term()]}.

-type tool_result() ::
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

%%====================================================================
%% API - 调用
%%====================================================================

%% @doc 调用工具（使用空上下文）
-spec invoke(tool_spec(), args()) -> tool_result().
invoke(ToolSpec, Args) ->
    invoke(ToolSpec, Args, beamai_context:new()).

%% @doc 调用工具（带上下文）
%%
%% 根据工具定义中的 timeout 和 retry 配置执行处理器。
%% 默认超时 30 秒，默认不重试。
-spec invoke(tool_spec(), args(), beamai_context:t()) -> tool_result().
invoke(#{handler := Handler} = ToolSpec, Args, Context) ->
    Timeout = maps:get(timeout, ToolSpec, 30000),
    RetryConf = maps:get(retry, ToolSpec, #{max => 0, delay => 0}),
    invoke_with_retry(Handler, Args, Context, RetryConf, Timeout).

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

%% @doc 将工具执行结果编码为 LLM 可读的二进制
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

%%====================================================================
%% 内部函数 - 调用
%%====================================================================

%% @private 带重试策略的工具调用入口
invoke_with_retry(Handler, Args, Context, #{max := Max, delay := Delay}, Timeout) ->
    invoke_with_retry(Handler, Args, Context, Max, Delay, Timeout).

%% @private 带重试策略的工具调用递归实现
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
    try jsx:decode(Args, [return_maps])
    catch _:_ -> #{<<"raw">> => Args}
    end;
parse_args(_) -> #{}.
