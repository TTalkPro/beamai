%%%-------------------------------------------------------------------
%%% @doc 工具定义与执行模块
%%%
%%% 提供工具的定义、验证和执行功能：
%%%   - 工具定义（define/3, define/4, define/5）
%%%   - Builder 模式（new/2 -> opts/2 -> params/2 -> handler/2 -> build/1）
%%%   - 参数辅助函数（string, int, number, bool, enum, array, object）
%%%   - 规格转换（to_spec/1, to_llm_spec/1, specs_from_list/1）
%%%   - 工具执行（execute/3, execute_all/3）
%%%   - 参数解析（parse_args/1）
%%%   - 执行器工厂（make_executor/1）
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool).

%% === 工具定义 API ===
-export([
    define/3,
    define/4,
    define/5
]).

%% === Builder 模式 API ===
-export([
    new/2,
    opts/2,
    params/2,
    handler/2,
    build/1
]).

%% === 参数辅助函数 ===
-export([
    string/2, string/3,
    int/2, int/3,
    number/2, number/3,
    bool/2, bool/3,
    enum/3, enum/4,
    array/3, array/4,
    object/3, object/4,
    params_spec/1,
    no_params/0
]).

%% === 遗留参数函数（向后兼容） ===
-export([string_param/1, string_param/2, string_params/1]).
-export([int_param/1, int_param/2, int_params/1]).
-export([array_param/2, object_array_param/2]).
-export([mixed_params/1]).

%% === 规格转换函数 ===
-export([
    to_spec/1,
    to_llm_spec/1,
    to_llm_specs/1,
    specs_from_list/1
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

%% @type tool_category() :: file | shell | http | search | todo | human | plan | mcp | custom.
%% 工具分类类型，用于标识工具的功能类别
-type tool_category() :: file | shell | http | search | todo | human | plan | mcp | custom.

%% @type tool_permission() :: file_read | file_write | shell_access | network_access | all.
%% 工具权限类型，用于控制工具的访问权限
-type tool_permission() :: file_read | file_write | shell_access | network_access | all.

%% @type param_type() :: string | integer | number | boolean | array | object.
%% 参数类型定义，支持基础类型和复合类型
-type param_type() :: string | integer | number | boolean | array | object.

%% @type tool_def() :: map().
%% 工具定义映射，包含名称、描述、参数规格、处理器函数等字段
-type tool_def() :: #{
    name := binary(),
    description := binary(),
    parameters := map(),
    handler := function(),
    category => tool_category(),
    permissions => [tool_permission()],
    metadata => map()
}.

%% @type tool_spec() :: map().
%% 工具规格映射，符合 LLM function calling 标准格式
-type tool_spec() :: #{
    type := function,
    function := #{
        name := binary(),
        description := binary(),
        parameters := map()
    }
}.

%% @type tool_call() :: map().
%% 工具调用映射，包含调用 ID、函数名和参数
-type tool_call() :: map().

%% @type tool_result() :: map().
%% 工具执行结果映射，包含调用 ID 和返回内容
-type tool_result() :: #{tool_call_id := binary(), content := binary()}.

%% @type executor() :: function().
%% 执行器函数类型，接收工具名、参数和上下文，返回执行结果
-type executor() :: fun((binary(), map(), map()) ->
    {ok, binary()} | {ok, binary(), map()} | {error, term()}).

-export_type([tool_def/0, tool_spec/0, tool_result/0, executor/0]).
-export_type([tool_category/0, tool_permission/0, param_type/0]).

%%====================================================================
%% 工具定义
%%====================================================================

%% @doc 定义工具（仅处理器，无参数）。
%%
%% 使用名称、描述和处理器函数定义一个工具，参数规格为空。
%%
%% @param Name        工具名称（二进制字符串）
%% @param Description 工具描述（二进制字符串）
%% @param Handler     工具处理器函数
%% @returns 工具定义映射
-spec define(binary(), binary(), function()) -> tool_def().
define(Name, Description, Handler) ->
    define(Name, Description, #{}, [], Handler).

%% @doc 定义工具（带参数）。
%%
%% 使用名称、描述、参数规格和处理器函数定义一个工具。
%%
%% @param Name        工具名称（二进制字符串）
%% @param Description 工具描述（二进制字符串）
%% @param Params      参数定义（列表或映射格式）
%% @param Handler     工具处理器函数
%% @returns 工具定义映射
-spec define(binary(), binary(), list() | map(), function()) -> tool_def().
define(Name, Description, Params, Handler) ->
    define(Name, Description, #{}, Params, Handler).

%% @doc 定义工具（带选项和参数）。
%%
%% 使用名称、描述、附加选项、参数规格和处理器函数定义一个完整的工具。
%% 选项支持 category（分类）、permissions（权限）和 metadata（元数据）。
%%
%% @param Name        工具名称（二进制字符串）
%% @param Description 工具描述（二进制字符串）
%% @param Opts        附加选项映射
%% @param Params      参数定义（列表或映射格式）
%% @param Handler     工具处理器函数
%% @returns 工具定义映射
-spec define(binary(), binary(), map(), list() | map(), function()) -> tool_def().
define(Name, Description, Opts, Params, Handler) ->
    Base = #{
        name => Name,
        description => Description,
        parameters => normalize_params(Params),
        handler => Handler
    },
    apply_opts(Base, Opts).

%% @private
%% 返回默认的空参数规格（object 类型，无属性，无必填项）
default_params() ->
    #{type => object, properties => #{}, required => []}.

%%====================================================================
%% Builder 模式
%%====================================================================

%% @doc 创建新的工具构建器。
%%
%% Builder 模式的第一步，初始化包含名称和描述的构建器映射。
%% 后续可链式调用 opts/2, params/2, handler/2, build/1 完成工具构建。
%%
%% @param Name        工具名称（二进制字符串）
%% @param Description 工具描述（二进制字符串）
%% @returns 初始化的构建器映射
-spec new(binary(), binary()) -> map().
new(Name, Description) ->
    #{name => Name, description => Description}.

%% @doc 为构建器添加选项。
%%
%% 将附加选项（如分类、权限、元数据）应用到构建器中。
%%
%% @param Builder 构建器映射
%% @param Opts    选项映射
%% @returns 添加了选项的构建器映射
-spec opts(map(), map()) -> map().
opts(Builder, Opts) ->
    apply_opts(Builder, Opts).

%% @doc 为构建器添加参数规格。
%%
%% 将参数定义规范化后添加到构建器中。
%%
%% @param Builder 构建器映射
%% @param Params  参数定义（列表或映射格式）
%% @returns 添加了参数规格的构建器映射
-spec params(map(), list() | map()) -> map().
params(Builder, Params) ->
    Builder#{parameters => normalize_params(Params)}.

%% @doc 为构建器添加处理器函数。
%%
%% 设置工具的执行处理器函数。
%%
%% @param Builder 构建器映射
%% @param Handler 处理器函数
%% @returns 添加了处理器的构建器映射
-spec handler(map(), function()) -> map().
handler(Builder, Handler) ->
    Builder#{handler => Handler}.

%% @doc 完成工具构建。
%%
%% 验证构建器是否包含所有必要字段（name, description, parameters, handler），
%% 如果缺少必要字段则返回错误。
%%
%% @param Builder 构建器映射
%% @returns 完成的工具定义映射，或 {error, missing_fields} 错误
-spec build(map()) -> tool_def() | {error, missing_fields}.
build(#{name := _, description := _, parameters := _, handler := _} = Tool) ->
    Tool;
build(Builder) ->
    case maps:is_key(parameters, Builder) andalso maps:is_key(handler, Builder) of
        true -> Builder;
        false -> {error, missing_fields}
    end.

%%====================================================================
%% 基础类型参数辅助函数
%%====================================================================

%% @doc 创建字符串类型参数定义。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @returns 包含参数名和类型信息的映射
-spec string(binary(), binary()) -> map().
string(Name, Description) ->
    #{Name => #{type => string, description => Description}}.

%% @doc 创建字符串类型参数定义（带选项）。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @param Opts        附加选项（如 required, {default, Value}, {enum, Values}）
%% @returns 包含参数名和类型信息的映射
-spec string(binary(), binary(), term()) -> map().
string(Name, Description, Opts) ->
    #{Name => build_param(string, Description, Opts)}.

%% @doc 创建整数类型参数定义。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @returns 包含参数名和类型信息的映射
-spec int(binary(), binary()) -> map().
int(Name, Description) ->
    #{Name => #{type => integer, description => Description}}.

%% @doc 创建整数类型参数定义（带选项）。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @param Opts        附加选项
%% @returns 包含参数名和类型信息的映射
-spec int(binary(), binary(), term()) -> map().
int(Name, Description, Opts) ->
    #{Name => build_param(integer, Description, Opts)}.

%% @doc 创建数值类型参数定义。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @returns 包含参数名和类型信息的映射
-spec number(binary(), binary()) -> map().
number(Name, Description) ->
    #{Name => #{type => number, description => Description}}.

%% @doc 创建数值类型参数定义（带选项）。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @param Opts        附加选项
%% @returns 包含参数名和类型信息的映射
-spec number(binary(), binary(), term()) -> map().
number(Name, Description, Opts) ->
    #{Name => build_param(number, Description, Opts)}.

%% @doc 创建布尔类型参数定义。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @returns 包含参数名和类型信息的映射
-spec bool(binary(), binary()) -> map().
bool(Name, Description) ->
    #{Name => #{type => boolean, description => Description}}.

%% @doc 创建布尔类型参数定义（带选项）。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @param Opts        附加选项
%% @returns 包含参数名和类型信息的映射
-spec bool(binary(), binary(), term()) -> map().
bool(Name, Description, Opts) ->
    #{Name => build_param(boolean, Description, Opts)}.

%%====================================================================
%% 复合类型参数辅助函数
%%====================================================================

%% @doc 创建枚举类型参数定义。
%%
%% 枚举类型基于字符串类型，限定可选值范围。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @param Values      可选值列表
%% @returns 包含参数名和类型信息的映射
-spec enum(binary(), binary(), [binary()]) -> map().
enum(Name, Description, Values) ->
    #{Name => #{type => string, description => Description, enum => Values}}.

%% @doc 创建枚举类型参数定义（带选项）。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @param Values      可选值列表
%% @param Opts        附加选项
%% @returns 包含参数名和类型信息的映射
-spec enum(binary(), binary(), [binary()], term()) -> map().
enum(Name, Description, Values, Opts) ->
    Base = #{type => string, description => Description, enum => Values},
    #{Name => apply_param_opts(Base, Opts)}.

%% @doc 创建数组类型参数定义。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @param ItemType    数组元素类型（原子或映射）
%% @returns 包含参数名和类型信息的映射
-spec array(binary(), binary(), atom() | map()) -> map().
array(Name, Description, ItemType) ->
    #{Name => build_array_param(Description, ItemType)}.

%% @doc 创建数组类型参数定义（带选项）。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @param ItemType    数组元素类型（原子或映射）
%% @param Opts        附加选项
%% @returns 包含参数名和类型信息的映射
-spec array(binary(), binary(), atom() | map(), term()) -> map().
array(Name, Description, ItemType, Opts) ->
    Base = build_array_param(Description, ItemType),
    #{Name => apply_param_opts(Base, Opts)}.

%% @doc 创建对象类型参数定义。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @param Props       对象属性定义列表
%% @returns 包含参数名和类型信息的映射
-spec object(binary(), binary(), list()) -> map().
object(Name, Description, Props) ->
    #{Name => build_object_param(Description, Props)}.

%% @doc 创建对象类型参数定义（带选项）。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @param Props       对象属性定义列表
%% @param Opts        附加选项
%% @returns 包含参数名和类型信息的映射
-spec object(binary(), binary(), list(), term()) -> map().
object(Name, Description, Props, Opts) ->
    Base = build_object_param(Description, Props),
    #{Name => apply_param_opts(Base, Opts)}.

%%====================================================================
%% 参数规格辅助函数
%%====================================================================

%% @doc 将参数列表转换为标准参数规格映射。
%%
%% 接收参数定义列表，返回规范化的参数规格映射。
%%
%% @param ParamList 参数定义列表
%% @returns 规范化的参数规格映射（type=object 格式）
-spec params_spec(list()) -> map().
params_spec(ParamList) ->
    normalize_params(ParamList).

%% @doc 返回空参数规格（无参数工具使用）。
%%
%% @returns 空的参数规格映射
-spec no_params() -> map().
no_params() ->
    #{type => object, properties => #{}, required => []}.

%%====================================================================
%% 规格转换
%%====================================================================

%% @doc 将工具定义转换为标准工具规格。
%%
%% 将内部工具定义映射转换为符合 LLM function calling 标准的规格格式。
%% 支持三种情况：完整定义、仅名称+描述、仅名称+处理器。
%%
%% @param ToolDef 工具定义映射
%% @returns 标准工具规格映射（type=function 格式）
-spec to_spec(tool_def()) -> tool_spec().
to_spec(#{name := Name, description := Desc, parameters := Params}) ->
    build_tool_spec(Name, Desc, Params);
to_spec(#{name := Name, description := Desc}) ->
    build_tool_spec(Name, Desc, default_params());
to_spec(#{name := Name, handler := _}) ->
    build_tool_spec(Name, <<"Tool: ", Name/binary>>, default_params()).

%% @doc 将工具定义转换为 LLM 专用规格。
%%
%% 生成适用于 LLM 调用的简化规格格式，包含 name、description 和 input_schema。
%%
%% @param ToolDef 工具定义映射
%% @returns LLM 规格映射
-spec to_llm_spec(tool_def()) -> map().
to_llm_spec(#{name := Name, description := Desc, parameters := Params}) ->
    #{name => Name, description => Desc, input_schema => Params}.

%% @doc 批量将工具定义列表转换为 LLM 专用规格列表。
%%
%% @param Tools 工具定义列表
%% @returns LLM 规格映射列表
-spec to_llm_specs([tool_def()]) -> [map()].
to_llm_specs(Tools) ->
    [to_llm_spec(T) || T <- Tools].

%% @private
%% 构建标准工具规格映射（type=function 格式）
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

%% @doc 批量将工具定义列表转换为标准工具规格列表。
%%
%% @param Tools 工具定义列表
%% @returns 标准工具规格列表
-spec specs_from_list([tool_def()]) -> [tool_spec()].
specs_from_list(Tools) ->
    [to_spec(T) || T <- Tools].

%%====================================================================
%% 通用工具生成
%%====================================================================

%% @doc 生成简单工具规格（所有属性均为必填）。
%%
%% 快速生成工具规格，将所有属性键自动设为必填参数。
%%
%% @param Name        工具名称（二进制字符串）
%% @param Description 工具描述（二进制字符串）
%% @param Properties  属性定义映射
%% @returns 工具规格映射
-spec simple_tool(binary(), binary(), map()) -> tool_spec().
simple_tool(Name, Description, Properties) ->
    simple_tool(Name, Description, Properties, maps:keys(Properties)).

%% @doc 生成简单工具规格（指定必填属性）。
%%
%% 快速生成工具规格，手动指定必填参数列表。
%%
%% @param Name        工具名称（二进制字符串）
%% @param Description 工具描述（二进制字符串）
%% @param Properties  属性定义映射
%% @param Required    必填参数名列表
%% @returns 工具规格映射
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

%% @doc 生成委托工具规格。
%%
%% 创建一个用于将任务委托给其他代理的工具规格。
%% 工具名称格式为 "delegate_to_<Name>"，包含 task 和 context 两个参数。
%%
%% @param Name 目标代理名称（二进制字符串）
%% @returns 委托工具规格映射
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

%% @doc 创建字符串参数映射（遗留函数，使用默认描述）。
%%
%% @param Name 参数名称（二进制字符串）
%% @returns 参数映射
-spec string_param(binary()) -> map().
string_param(Name) ->
    string_param(Name, <<"Parameter">>).

%% @doc 创建字符串参数映射（遗留函数，指定描述）。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @returns 参数映射
-spec string_param(binary(), binary()) -> map().
string_param(Name, Description) ->
    #{Name => #{type => string, description => Description}}.

%% @doc 批量创建字符串参数映射（遗留函数）。
%%
%% @param Params 参数名称和描述的元组列表 [{Name, Description}, ...]
%% @returns 合并后的参数映射
-spec string_params([{binary(), binary()}]) -> map().
string_params(Params) when is_list(Params) ->
    maps:from_list([{N, #{type => string, description => D}} || {N, D} <- Params]).

%% @doc 创建整数参数映射（遗留函数，使用默认描述）。
%%
%% @param Name 参数名称（二进制字符串）
%% @returns 参数映射
-spec int_param(binary()) -> map().
int_param(Name) ->
    int_param(Name, <<"Integer parameter">>).

%% @doc 创建整数参数映射（遗留函数，指定描述）。
%%
%% @param Name        参数名称（二进制字符串）
%% @param Description 参数描述（二进制字符串）
%% @returns 参数映射
-spec int_param(binary(), binary()) -> map().
int_param(Name, Description) ->
    #{Name => #{type => integer, description => Description}}.

%% @doc 批量创建整数参数映射（遗留函数）。
%%
%% @param Params 参数名称和描述的元组列表
%% @returns 合并后的参数映射
-spec int_params([{binary(), binary()}]) -> map().
int_params(Params) when is_list(Params) ->
    maps:from_list([{N, #{type => integer, description => D}} || {N, D} <- Params]).

%% @doc 创建数组参数映射（遗留函数）。
%%
%% @param Name     参数名称（二进制字符串）
%% @param ItemType 数组元素类型（二进制字符串）
%% @returns 数组参数映射
-spec array_param(binary(), binary()) -> map().
array_param(Name, ItemType) ->
    #{Name => #{
        type => array,
        description => <<"Array of ", ItemType/binary, "s">>,
        items => #{type => ItemType}
    }}.

%% @doc 创建对象数组参数映射（遗留函数）。
%%
%% @param Name        参数名称（二进制字符串）
%% @param ObjectProps 对象属性定义映射
%% @returns 对象数组参数映射
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

%% @doc 批量创建混合类型参数映射（遗留函数）。
%%
%% @param Params 参数元组列表 [{Name, Type, Description}, ...]
%% @returns 合并后的混合类型参数映射
-spec mixed_params([{binary(), atom(), binary()}]) -> map().
mixed_params(Params) when is_list(Params) ->
    maps:from_list([param(N, T, D) || {N, T, D} <- Params]).

%% @private
%% 将单个参数元组转换为 {Name, TypeMap} 键值对
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

%% @doc 执行单个工具调用。
%%
%% 解析工具调用中的函数名和参数 JSON，在处理器映射中查找对应的处理器并执行。
%%
%% @param ToolCall  工具调用映射（包含 id, function.name, function.arguments）
%% @param Handlers  处理器映射 #{Name => HandlerFun}
%% @param _Context  执行上下文（当前未使用）
%% @returns 工具执行结果映射 #{tool_call_id, content}
-spec execute(tool_call(), map(), map()) -> tool_result().
execute(ToolCall, Handlers, _Context) ->
    #{id := Id, function := #{name := Name, arguments := ArgsJson}} = ToolCall,
    Args = parse_args(ArgsJson),
    Result = execute_handler(Name, Args, Handlers),
    format_result(Id, Result).

%% @private
%% 从处理器映射中查找并执行指定名称的工具处理器
execute_handler(Name, Args, Handlers) ->
    case maps:get(Name, Handlers, undefined) of
        undefined -> {error, {unknown_tool, Name}};
        Handler -> safe_execute(Handler, Args)
    end.

%% @private
%% 安全执行处理器函数，捕获异常并转换结果为二进制字符串
safe_execute(Handler, Args) ->
    case beamai_utils:safe_execute(fun() -> Handler(Args) end) of
        {ok, Result} -> {ok, beamai_utils:to_binary(Result)};
        Error -> Error
    end.

%% @doc 批量执行多个工具调用。
%%
%% 依次执行所有工具调用并收集结果。
%%
%% @param ToolCalls 工具调用列表
%% @param Handlers  处理器映射
%% @param Context   执行上下文
%% @returns 工具执行结果列表
-spec execute_all([tool_call()], map(), map()) -> [tool_result()].
execute_all(ToolCalls, Handlers, Context) ->
    [execute(TC, Handlers, Context) || TC <- ToolCalls].

%%====================================================================
%% 参数解析
%%====================================================================

%% @doc 解析工具调用参数。
%%
%% 将 JSON 二进制字符串或已解析的映射转换为标准参数映射。
%%
%% @param Args JSON 二进制字符串或已解析的映射
%% @returns 解析后的参数映射
-spec parse_args(binary() | map()) -> map().
parse_args(Args) ->
    beamai_utils:parse_json(Args).

%%====================================================================
%% 执行器工厂
%%====================================================================

%% @doc 从工具定义列表创建执行器函数。
%%
%% 将工具定义列表中的处理器提取并构建为统一的执行器函数，
%% 该执行器接收工具名、参数和上下文，自动路由到对应的处理器。
%%
%% @param Tools 工具定义列表
%% @returns 执行器函数 fun(Name, Args, Context) -> Result
-spec make_executor([tool_def()]) -> executor().
make_executor(Tools) ->
    Handlers = build_handlers(Tools),
    fun(Name, Args, _Context) ->
        execute_handler(Name, Args, Handlers)
    end.

%% @private
%% 从工具定义列表中提取所有处理器，构建名称到处理器的映射
build_handlers(Tools) ->
    lists:foldl(
        fun(#{name := Name, handler := Handler}, Acc) ->
            Acc#{Name => Handler}
        end,
        #{},
        Tools
    ).

%%====================================================================
%% 内部辅助函数
%%====================================================================

%% @private
%% 格式化工具执行结果为标准结果映射
%% 成功时直接使用返回内容，失败时格式化错误信息
format_result(Id, {ok, Content}) ->
    #{tool_call_id => Id, content => Content};
format_result(Id, {error, Reason}) ->
    #{tool_call_id => Id, content => beamai_utils:format_error(Reason)}.

%% @private
%% 将选项映射应用到工具定义中
%% 仅处理 category、permissions、metadata 三种已知选项，忽略其他
apply_opts(Tool, Opts) ->
    maps:fold(fun
        (category, V, Acc) -> Acc#{category => V};
        (permissions, V, Acc) -> Acc#{permissions => V};
        (metadata, V, Acc) -> Acc#{metadata => V};
        (_, _, Acc) -> Acc
    end, Tool, Opts).

%% @private
%% 规范化参数定义为标准格式（type=object 的映射）
%% 如果输入已经是带 type 键的映射则直接返回
%% 如果输入是无 type 键的映射则包装为 object 类型
%% 如果输入是列表则收集并转换为 object 类型
normalize_params(Params) when is_map(Params) ->
    case maps:is_key(type, Params) of
        true -> Params;
        false -> #{type => object, properties => Params, required => []}
    end;
normalize_params(ParamList) when is_list(ParamList) ->
    {Properties, Required} = collect_params(ParamList, #{}, []),
    #{type => object, properties => Properties, required => Required}.

%% @private
%% 递归收集参数列表中的属性和必填字段
%% 返回 {属性映射, 必填字段列表}
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

%% @private
%% 解析单个参数定义为 {名称, 模式, 是否必填} 三元组
%% 支持三种输入格式：
%%   - {Name, Type, Desc} 元组（默认非必填）
%%   - {Name, Type, Desc, Opts} 元组（可通过 Opts 指定必填）
%%   - 映射格式（通过 '$required' 键指定必填）
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

%% @private
%% 构建参数模式映射：规范化类型 + 描述 + 应用选项
build_param(Type, Description, Opts) ->
    Base = #{type => normalize_type(Type), description => Description},
    apply_param_opts(Base, Opts).

%% @private
%% 规范化类型名称的简写形式为标准形式
%% str -> string, int -> integer, num -> number
normalize_type(str) -> string;
normalize_type(int) -> integer;
normalize_type(num) -> number;
normalize_type(boolean) -> boolean;
normalize_type(Type) -> Type.

%% @private
%% 将参数选项应用到参数模式映射中
%% 支持的选项：required（必填标记）、{default, Value}（默认值）、
%% {enum, Values}（枚举值）、{values, Values}（同 enum）、列表（多个选项）
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

%% @private
%% 判断参数选项中是否包含 required 标记
is_required(required) -> true;
is_required(Opts) when is_list(Opts) -> lists:member(required, Opts);
is_required(_) -> false.

%% @private
%% 构建数组类型参数模式
%% 如果 ItemType 是原子则作为元素类型
%% 如果 ItemType 是映射则作为元素模式（处理单键映射时移除 '$required' 标记）
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

%% @private
%% 构建对象类型参数模式
%% 从属性列表中收集属性定义和必填字段列表
build_object_param(Description, Props) when is_list(Props) ->
    {Properties, Required} = collect_params(Props, #{}, []),
    #{
        type => object,
        description => Description,
        properties => Properties,
        required => Required
    }.
