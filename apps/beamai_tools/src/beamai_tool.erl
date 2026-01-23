%%%-------------------------------------------------------------------
%%% @doc 工具定义与执行模块
%%%
%%% 提供工具定义、验证和执行的纯函数。
%%% 合并了原 beamai_tool_builder 的功能，提供统一的工具定义 API。
%%%
%%% 功能：
%%%   - 工具定义（define/3, define/4, define/5）
%%%   - Builder 模式（new/2 → opts/2 → params/2 → handler/2 → build/1）
%%%   - 参数辅助函数（string, int, number, bool, enum, array, object）
%%%   - 规格转换（to_spec/1, to_llm_spec/1, specs_from_list/1）
%%%   - 工具执行（execute/3, execute_all/3）
%%%   - 参数解析（parse_args/1）
%%%   - 执行器工厂（make_executor/1）
%%%
%%% == 使用示例 ==
%%%
%%% === 最简形式 ===
%%% ```erlang
%%% Tool = beamai_tool:define(<<"echo">>, <<"Echo input">>, [
%%%     {<<"text">>, string, <<"Text to echo">>, required}
%%% ], fun(#{<<"text">> := T}) -> {ok, T} end).
%%% ```
%%%
%%% === 带选项 ===
%%% ```erlang
%%% Tool = beamai_tool:define(<<"file_read">>, <<"Read file">>, #{
%%%     category => file,
%%%     permissions => [file_read]
%%% }, [
%%%     {<<"path">>, string, <<"File path">>, required},
%%%     {<<"encoding">>, string, <<"Encoding">>, {default, <<"utf-8">>}}
%%% ], fun handle_read/2).
%%% ```
%%%
%%% === 使用参数辅助函数 ===
%%% ```erlang
%%% Tool = beamai_tool:define(<<"search">>, <<"Search files">>, #{
%%%     category => file
%%% }, [
%%%     beamai_tool:string(<<"pattern">>, <<"Search pattern">>, required),
%%%     beamai_tool:int(<<"limit">>, <<"Max results">>, {default, 100}),
%%%     beamai_tool:bool(<<"recursive">>, <<"Search recursively">>)
%%% ], fun handle_search/2).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool).

%% === 工具定义 API ===
-export([
    define/3,           %% Name, Desc, Handler (无参数)
    define/4,           %% Name, Desc, Params, Handler
    define/5            %% Name, Desc, Opts, Params, Handler
]).

%% === Builder 模式 ===
-export([
    new/2,              %% Name, Desc -> Builder
    opts/2,             %% Builder, Opts -> Builder
    params/2,           %% Builder, Params -> Builder
    handler/2,          %% Builder, Handler -> Builder
    build/1             %% Builder -> tool_def()
]).

%% === 参数辅助函数 ===
-export([
    %% 基础类型
    string/2, string/3,
    int/2, int/3,
    number/2, number/3,
    bool/2, bool/3,
    %% 复合类型
    enum/3, enum/4,
    array/3, array/4,
    object/3, object/4,
    %% 辅助
    params_spec/1,
    no_params/0
]).

%% === 旧版参数函数（向后兼容）===
-export([string_param/1, string_param/2, string_params/1]).
-export([int_param/1, int_param/2, int_params/1]).
-export([array_param/2, object_array_param/2]).
-export([mixed_params/1]).

%% === 规格转换 ===
-export([
    to_spec/1,          %% 转换为 LLM API 规格（带 type => function 包装）
    to_llm_spec/1,      %% 简化格式（name, description, input_schema）
    to_llm_specs/1,     %% 批量转换（简化格式）
    specs_from_list/1   %% 批量转换（旧名，保留兼容）
]).

%% === 通用工具生成 ===
-export([simple_tool/3, simple_tool/4]).
-export([delegation_tool/1]).

%% === 工具执行 ===
-export([execute/3, execute_all/3]).

%% === 参数解析 ===
-export([parse_args/1]).

%% === 执行器工厂 ===
-export([make_executor/1]).

%% 类型定义
-type tool_category() :: file | shell | http | search | todo | human | plan | mcp | custom.
-type tool_permission() :: file_read | file_write | shell_access | network_access | all.
-type param_type() :: string | integer | number | boolean | array | object.

-type tool_def() :: #{
    name := binary(),              %% 工具名称
    description := binary(),       %% 工具描述
    parameters := map(),           %% 参数定义
    handler := function(),         %% 处理函数
    category => tool_category(),   %% 工具分类（可选）
    permissions => [tool_permission()],  %% 所需权限（可选）
    metadata => map()              %% 扩展元数据（可选）
}.
-type tool_spec() :: #{
    type := function,
    function := #{
        name := binary(),          %% 函数名称
        description := binary(),   %% 函数描述
        parameters := map()        %% 参数规格
    }
}.
-type tool_call() :: beamai_message:tool_call().  %% 工具调用
-type tool_result() :: #{tool_call_id := binary(), content := binary()}.  %% 调用结果
-type executor() :: fun((binary(), map(), map()) ->
    {ok, binary()} | {ok, binary(), map()} | {error, term()}).  %% 执行器 (Name, Args, Context)

-export_type([tool_def/0, tool_spec/0, tool_result/0, executor/0]).
-export_type([tool_category/0, tool_permission/0, param_type/0]).

%%====================================================================
%% 工具定义
%%====================================================================

%% @doc 定义工具（使用默认参数）
-spec define(binary(), binary(), function()) -> tool_def().
define(Name, Description, Handler) ->
    define(Name, Description, #{}, [], Handler).

%% @doc 定义工具（带参数规格）
%%
%% 参数格式：
%% - 元组列表：[{Name, Type, Desc}, {Name, Type, Desc, Opts}, ...]
%% - 参数函数返回值列表：[string(...), int(...), ...]
%% - 已构建的 params map
-spec define(binary(), binary(), list() | map(), function()) -> tool_def().
define(Name, Description, Params, Handler) ->
    define(Name, Description, #{}, Params, Handler).

%% @doc 定义工具（带选项和参数规格）
%%
%% Opts 支持：
%% - category: atom() - 工具分类
%% - permissions: [atom()] - 所需权限
%% - metadata: map() - 扩展元数据
-spec define(binary(), binary(), map(), list() | map(), function()) -> tool_def().
define(Name, Description, Opts, Params, Handler) ->
    Base = #{
        name => Name,
        description => Description,
        parameters => normalize_params(Params),
        handler => Handler
    },
    apply_opts(Base, Opts).

%% @private 默认参数规格
default_params() ->
    #{type => object, properties => #{}, required => []}.

%%====================================================================
%% Builder 模式
%%====================================================================

%% @doc 创建工具构建器
-spec new(binary(), binary()) -> map().
new(Name, Description) ->
    #{name => Name, description => Description}.

%% @doc 设置选项
-spec opts(map(), map()) -> map().
opts(Builder, Opts) ->
    apply_opts(Builder, Opts).

%% @doc 设置参数
-spec params(map(), list() | map()) -> map().
params(Builder, Params) ->
    Builder#{parameters => normalize_params(Params)}.

%% @doc 设置处理器
-spec handler(map(), function()) -> map().
handler(Builder, Handler) ->
    Builder#{handler => Handler}.

%% @doc 构建工具定义
-spec build(map()) -> tool_def() | {error, missing_fields}.
build(#{name := _, description := _, parameters := _, handler := _} = Tool) ->
    Tool;
build(Builder) ->
    case maps:is_key(parameters, Builder) andalso maps:is_key(handler, Builder) of
        true -> Builder;
        false -> {error, missing_fields}
    end.

%%====================================================================
%% 基础类型参数
%%====================================================================

%% @doc 字符串参数
-spec string(binary(), binary()) -> map().
string(Name, Description) ->
    #{Name => #{type => string, description => Description}}.

%% @doc 字符串参数（带选项）
-spec string(binary(), binary(), term()) -> map().
string(Name, Description, Opts) ->
    #{Name => build_param(string, Description, Opts)}.

%% @doc 整数参数
-spec int(binary(), binary()) -> map().
int(Name, Description) ->
    #{Name => #{type => integer, description => Description}}.

%% @doc 整数参数（带选项）
-spec int(binary(), binary(), term()) -> map().
int(Name, Description, Opts) ->
    #{Name => build_param(integer, Description, Opts)}.

%% @doc 数值参数
-spec number(binary(), binary()) -> map().
number(Name, Description) ->
    #{Name => #{type => number, description => Description}}.

%% @doc 数值参数（带选项）
-spec number(binary(), binary(), term()) -> map().
number(Name, Description, Opts) ->
    #{Name => build_param(number, Description, Opts)}.

%% @doc 布尔参数
-spec bool(binary(), binary()) -> map().
bool(Name, Description) ->
    #{Name => #{type => boolean, description => Description}}.

%% @doc 布尔参数（带选项）
-spec bool(binary(), binary(), term()) -> map().
bool(Name, Description, Opts) ->
    #{Name => build_param(boolean, Description, Opts)}.

%%====================================================================
%% 复合类型参数
%%====================================================================

%% @doc 枚举参数
-spec enum(binary(), binary(), [binary()]) -> map().
enum(Name, Description, Values) ->
    #{Name => #{type => string, description => Description, enum => Values}}.

%% @doc 枚举参数（带选项）
-spec enum(binary(), binary(), [binary()], term()) -> map().
enum(Name, Description, Values, Opts) ->
    Base = #{type => string, description => Description, enum => Values},
    #{Name => apply_param_opts(Base, Opts)}.

%% @doc 数组参数
-spec array(binary(), binary(), atom() | map()) -> map().
array(Name, Description, ItemType) ->
    #{Name => build_array_param(Description, ItemType)}.

%% @doc 数组参数（带选项）
-spec array(binary(), binary(), atom() | map(), term()) -> map().
array(Name, Description, ItemType, Opts) ->
    Base = build_array_param(Description, ItemType),
    #{Name => apply_param_opts(Base, Opts)}.

%% @doc 对象参数
-spec object(binary(), binary(), list()) -> map().
object(Name, Description, Props) ->
    #{Name => build_object_param(Description, Props)}.

%% @doc 对象参数（带选项）
-spec object(binary(), binary(), list(), term()) -> map().
object(Name, Description, Props, Opts) ->
    Base = build_object_param(Description, Props),
    #{Name => apply_param_opts(Base, Opts)}.

%%====================================================================
%% 参数规格辅助
%%====================================================================

%% @doc 从参数列表构建完整的 parameters map
-spec params_spec(list()) -> map().
params_spec(ParamList) ->
    normalize_params(ParamList).

%% @doc 空参数规格
-spec no_params() -> map().
no_params() ->
    #{type => object, properties => #{}, required => []}.

%%====================================================================
%% 规格转换
%%====================================================================

%% @doc 将工具定义转换为 LLM API 规格（带 type => function 包装）
%%
%% 输出格式适用于 OpenAI 风格的函数调用。
-spec to_spec(tool_def()) -> tool_spec().
to_spec(#{name := Name, description := Desc, parameters := Params}) ->
    build_tool_spec(Name, Desc, Params);
to_spec(#{name := Name, description := Desc}) ->
    build_tool_spec(Name, Desc, default_params());
to_spec(#{name := Name, handler := _}) ->
    build_tool_spec(Name, <<"Tool: ", Name/binary>>, default_params()).

%% @doc 转换为 LLM API 格式（简化格式）
%%
%% 输出格式适用于 Anthropic 等 API。
%% 格式：#{name, description, input_schema}
-spec to_llm_spec(tool_def()) -> map().
to_llm_spec(#{name := Name, description := Desc, parameters := Params}) ->
    #{name => Name, description => Desc, input_schema => Params}.

%% @doc 批量转换为 LLM 规格（简化格式）
-spec to_llm_specs([tool_def()]) -> [map()].
to_llm_specs(Tools) ->
    [to_llm_spec(T) || T <- Tools].

%% @private 构建工具规格
-spec build_tool_spec(binary(), binary(), map()) -> tool_spec().
build_tool_spec(Name, Desc, Params) ->
    #{
        type => function,
        function => #{
            name => Name,
            description => Desc,
            parameters => Params
        }
    }.

%% @doc 批量转换工具定义为规格列表（带 type => function 包装）
-spec specs_from_list([tool_def()]) -> [tool_spec()].
specs_from_list(Tools) ->
    [to_spec(T) || T <- Tools].

%%====================================================================
%% 通用工具生成
%%====================================================================

%% @doc 生成简单工具规格（自动推断必需参数）
%% Properties 中的所有参数默认为必需
-spec simple_tool(binary(), binary(), map()) -> tool_spec().
simple_tool(Name, Description, Properties) ->
    simple_tool(Name, Description, Properties, maps:keys(Properties)).

%% @doc 生成简单工具规格（指定必需参数）
-spec simple_tool(binary(), binary(), map(), [binary()]) -> tool_spec().
simple_tool(Name, Description, Properties, Required) ->
    #{
        type => function,
        function => #{
            name => Name,
            description => Description,
            parameters => #{
                type => object,
                properties => Properties,
                required => Required
            }
        }
    }.

%% @doc 生成委托工具规格
%% 创建一个用于委托任务给子 Agent 的工具
-spec delegation_tool(binary()) -> tool_spec().
delegation_tool(Name) ->
    ToolName = <<"delegate_to_", Name/binary>>,
    simple_tool(
        ToolName,
        <<"Delegate task to ", Name/binary>>,
        #{
            <<"task">> => #{
                type => string,
                description => <<"Task description to delegate">>
            },
            <<"context">> => #{
                type => string,
                description => <<"Additional context for the task">>
            }
        },
        [<<"task">>]
    ).

%% @doc 生成字符串参数属性（使用默认描述）
-spec string_param(binary()) -> map().
string_param(Name) ->
    string_param(Name, <<"Parameter">>).

%% @doc 生成字符串参数属性（带描述）
-spec string_param(binary(), binary()) -> map().
string_param(Name, Description) ->
    #{Name => #{type => string, description => Description}}.

%% @doc 批量生成字符串参数
%% Params: [{Name, Description}, ...]
-spec string_params([{binary(), binary()}]) -> map().
string_params(Params) when is_list(Params) ->
    maps:from_list([{N, #{type => string, description => D}} || {N, D} <- Params]).

%% @doc 生成整数参数属性（使用默认描述）
-spec int_param(binary()) -> map().
int_param(Name) ->
    int_param(Name, <<"Integer parameter">>).

%% @doc 生成整数参数属性（带描述）
-spec int_param(binary(), binary()) -> map().
int_param(Name, Description) ->
    #{Name => #{type => integer, description => Description}}.

%% @doc 批量生成整数参数
-spec int_params([{binary(), binary()}]) -> map().
int_params(Params) when is_list(Params) ->
    maps:from_list([{N, #{type => integer, description => D}} || {N, D} <- Params]).

%% @doc 生成数组参数属性（带元素类型）
%% ItemType: 数组元素的类型（如 string, integer, object 等）
-spec array_param(binary(), binary()) -> map().
array_param(Name, ItemType) ->
    #{Name => #{
        type => array,
        description => <<"Array of ", ItemType/binary, "s">>,
        items => #{type => ItemType}
    }}.

%% @doc 生成对象数组参数
%% ObjectProps: 对象的属性定义
-spec object_array_param(binary(), map()) -> map().
object_array_param(Name, ObjectProps) ->
    #{Name => #{
        type => array,
        description => <<"Array of objects">>,
        items => #{
            type => object,
            properties => ObjectProps
        }
    }}.

%% @doc 混合参数类型（支持多种类型）
-spec mixed_params([{binary(), atom(), binary()}]) -> map().
%% Params: [{Name, Type, Description}, ...]
%% Type: string | integer | number | boolean | array | object
mixed_params(Params) when is_list(Params) ->
    maps:from_list([param(N, T, D) || {N, T, D} <- Params]).

%% @private 生成单个参数
-spec param(binary(), atom(), binary()) -> {binary(), map()}.
param(Name, Type, Description) ->
    TypeMap = case Type of
        string   -> #{type => string, description => Description};
        integer  -> #{type => integer, description => Description};
        number   -> #{type => number, description => Description};
        boolean  -> #{type => boolean, description => Description};
        array    -> #{type => array, description => Description};
        object   -> #{type => object, description => Description}
    end,
    {Name, TypeMap}.

%%====================================================================
%% 工具执行
%%====================================================================

%% @doc 执行单个工具调用
-spec execute(tool_call(), map(), map()) -> tool_result().
execute(ToolCall, Handlers, _Context) ->
    #{id := Id, function := #{name := Name, arguments := ArgsJson}} = ToolCall,
    Args = parse_args(ArgsJson),
    Result = execute_handler(Name, Args, Handlers),
    format_result(Id, Result).

%% @private 执行处理器
execute_handler(Name, Args, Handlers) ->
    case maps:get(Name, Handlers, undefined) of
        undefined -> {error, {unknown_tool, Name}};
        Handler -> safe_execute(Handler, Args)
    end.

%% @private 安全执行（使用公共工具函数）
safe_execute(Handler, Args) ->
    case beamai_utils:safe_execute(fun() -> Handler(Args) end) of
        {ok, Result} -> {ok, beamai_utils:to_binary(Result)};
        Error -> Error
    end.

%% @doc 批量执行工具调用
-spec execute_all([tool_call()], map(), map()) -> [tool_result()].
execute_all(ToolCalls, Handlers, Context) ->
    [execute(TC, Handlers, Context) || TC <- ToolCalls].

%%====================================================================
%% 参数解析
%%====================================================================

%% @doc 解析工具调用参数
-spec parse_args(binary() | map()) -> map().
parse_args(Args) ->
    beamai_utils:parse_json(Args).

%%====================================================================
%% 执行器工厂
%%====================================================================

%% @doc 从工具列表创建执行器函数
-spec make_executor([tool_def()]) -> executor().
make_executor(Tools) ->
    Handlers = build_handlers(Tools),
    fun(Name, Args, _Context) ->
        execute_handler(Name, Args, Handlers)
    end.

%% @private 构建处理器映射
build_handlers(Tools) ->
    lists:foldl(
        fun(#{name := Name, handler := Handler}, Acc) ->
            Acc#{Name => Handler}
        end,
        #{},
        Tools
    ).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 格式化执行结果
format_result(Id, {ok, Content}) ->
    #{tool_call_id => Id, content => Content};
format_result(Id, {error, Reason}) ->
    #{tool_call_id => Id, content => beamai_utils:format_error(Reason)}.

%% @private 应用选项到工具定义
apply_opts(Tool, Opts) ->
    maps:fold(fun
        (category, V, Acc) -> Acc#{category => V};
        (permissions, V, Acc) -> Acc#{permissions => V};
        (metadata, V, Acc) -> Acc#{metadata => V};
        (_, _, Acc) -> Acc
    end, Tool, Opts).

%% @private 标准化参数定义
normalize_params(Params) when is_map(Params) ->
    case maps:is_key(type, Params) of
        true -> Params;
        false -> #{type => object, properties => Params, required => []}
    end;
normalize_params(ParamList) when is_list(ParamList) ->
    {Properties, Required} = collect_params(ParamList, #{}, []),
    #{type => object, properties => Properties, required => Required}.

%% @private 收集参数定义
collect_params([], Props, Req) ->
    {Props, lists:reverse(Req)};
collect_params([Param | Rest], Props, Req) ->
    {Name, Schema, IsRequired} = parse_param(Param),
    NewProps = Props#{Name => Schema},
    NewReq = case IsRequired of
        true -> [Name | Req];
        false -> Req
    end,
    collect_params(Rest, NewProps, NewReq).

%% @private 解析单个参数定义
parse_param({Name, Type, Desc}) ->
    {Name, #{type => Type, description => Desc}, false};
parse_param({Name, Type, Desc, Opts}) ->
    Schema = build_param(Type, Desc, Opts),
    IsRequired = is_required(Opts),
    {Name, Schema, IsRequired};
parse_param(ParamMap) when is_map(ParamMap) ->
    [{Name, Schema}] = maps:to_list(ParamMap),
    IsRequired = maps:get('$required', Schema, false),
    CleanSchema = maps:remove('$required', Schema),
    {Name, CleanSchema, IsRequired}.

%% @private 构建参数 schema
build_param(Type, Description, Opts) ->
    Base = #{type => normalize_type(Type), description => Description},
    apply_param_opts(Base, Opts).

%% @private 标准化类型名
normalize_type(str) -> string;
normalize_type(int) -> integer;
normalize_type(num) -> number;
normalize_type(boolean) -> boolean;
normalize_type(Type) -> Type.

%% @private 应用参数选项
apply_param_opts(Schema, required) ->
    Schema#{'$required' => true};
apply_param_opts(Schema, {default, Value}) ->
    Schema#{default => Value};
apply_param_opts(Schema, {enum, Values}) ->
    Schema#{enum => Values};
apply_param_opts(Schema, {values, Values}) ->
    Schema#{enum => Values};
apply_param_opts(Schema, Opts) when is_list(Opts) ->
    lists:foldl(fun(Opt, Acc) -> apply_param_opts(Acc, Opt) end, Schema, Opts);
apply_param_opts(Schema, _) ->
    Schema.

%% @private 检查是否必需
is_required(required) -> true;
is_required(Opts) when is_list(Opts) -> lists:member(required, Opts);
is_required(_) -> false.

%% @private 构建数组参数 schema
build_array_param(Description, ItemType) when is_atom(ItemType) ->
    #{type => array, description => Description, items => #{type => ItemType}};
build_array_param(Description, ItemSchema) when is_map(ItemSchema) ->
    InnerSchema = case maps:size(ItemSchema) of
        1 ->
            [{_, S}] = maps:to_list(ItemSchema),
            maps:remove('$required', S);
        _ ->
            ItemSchema
    end,
    #{type => array, description => Description, items => InnerSchema}.

%% @private 构建对象参数 schema
build_object_param(Description, Props) when is_list(Props) ->
    {Properties, Required} = collect_params(Props, #{}, []),
    #{
        type => object,
        description => Description,
        properties => Properties,
        required => Required
    }.
