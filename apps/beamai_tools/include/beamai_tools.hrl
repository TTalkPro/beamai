%%%-------------------------------------------------------------------
%%% @doc beamai_tools 公共头文件
%%%
%%% 定义工具相关的类型和宏。
%%%
%%% == 设计说明 ==
%%%
%%% 使用 map 而非 record 定义工具，原因：
%%% - 与 MCP 等外部协议格式一致
%%% - 扩展性好，不同 Provider 可添加自定义字段
%%% - 序列化友好，无需转换
%%% - 热升级无兼容性问题
%%%
%%% == 类型定义 ==
%%%
%%% 本文件中的类型定义与 beamai_tool 模块导出的类型兼容。
%%% 推荐直接使用 beamai_tool 模块的导出类型：
%%% - beamai_tool:tool_def()
%%% - beamai_tool:tool_category()
%%% - beamai_tool:tool_permission()
%%% - beamai_tool:param_type()
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(AGENT_TOOLS_HRL).
-define(AGENT_TOOLS_HRL, true).

%%====================================================================
%% 类型定义
%%====================================================================

%% 工具分类
-type tool_category() :: file | shell | http | search | todo | human | plan | mcp | custom.

%% 工具权限
-type tool_permission() :: file_read | file_write | shell_access | network_access | all.

%% 工具提供者模块
-type tool_provider() :: module().

%% 参数类型
-type param_type() :: string | integer | number | boolean | array | object.

%% 工具定义（map 格式）
%%
%% 必需字段：
%% - name: 工具名称
%% - description: 工具描述
%% - parameters: 参数规格 (JSON Schema 格式)
%% - handler: 处理器函数 fun((Args :: map()) -> Result) 或
%%            fun((Args :: map(), Context :: map()) -> Result)
%%
%% 可选字段：
%% - category: 工具分类
%% - permissions: 所需权限列表
%% - metadata: 扩展元数据
-type tool_def() :: #{
    name := binary(),
    description := binary(),
    parameters := map(),
    handler := function(),
    category => tool_category(),
    permissions => [tool_permission()],
    metadata => map()
}.

%% 工具执行结果
-type tool_result() :: #{
    success := boolean(),
    result => term(),
    error => term(),
    context_updates => map()
}.

%%====================================================================
%% 宏定义 - 参数构建
%%====================================================================

%% 创建字符串参数
-define(STRING_PARAM(Desc), #{type => string, description => Desc}).

%% 创建整数参数
-define(INT_PARAM(Desc), #{type => integer, description => Desc}).

%% 创建数字参数
-define(NUMBER_PARAM(Desc), #{type => number, description => Desc}).

%% 创建布尔参数
-define(BOOL_PARAM(Desc), #{type => boolean, description => Desc}).

%% 创建数组参数
-define(ARRAY_PARAM(ItemType, Desc), #{type => array, items => ItemType, description => Desc}).

%% 创建枚举参数
-define(ENUM_PARAM(Values, Desc), #{type => string, enum => Values, description => Desc}).

%% 创建对象参数
-define(OBJECT_PARAM(Props, Desc), #{type => object, properties => Props, description => Desc}).

%%====================================================================
%% 宏定义 - 工具构建
%%====================================================================

%% 构建完整的 parameters 规格（JSON Schema 格式）
-define(PARAMS(Props, Required), #{
    type => object,
    properties => Props,
    required => Required
}).

%% 构建 parameters 规格（所有参数都必需）
-define(PARAMS(Props), ?PARAMS(Props, maps:keys(Props))).

%% 无参数工具
-define(NO_PARAMS, #{type => object, properties => #{}, required => []}).

%% 工具定义快捷宏（基础版）
-define(TOOL(Name, Desc, Params, Handler), #{
    name => Name,
    description => Desc,
    parameters => Params,
    handler => Handler
}).

%% 工具定义快捷宏（带分类）
-define(TOOL(Name, Desc, Category, Params, Handler), #{
    name => Name,
    description => Desc,
    category => Category,
    parameters => Params,
    handler => Handler
}).

%% 工具定义快捷宏（完整版）
-define(TOOL_FULL(Name, Desc, Category, Perms, Params, Handler, Meta), #{
    name => Name,
    description => Desc,
    category => Category,
    permissions => Perms,
    parameters => Params,
    handler => Handler,
    metadata => Meta
}).

-endif. %% AGENT_TOOLS_HRL
