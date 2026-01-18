%%%-------------------------------------------------------------------
%%% @doc 工具定义辅助模块
%%%
%%% 提供工具定义的公共函数：
%%% - 参数定义辅助函数：简化工具参数规格创建
%%% - 工具规格构建器：统一工具定义格式
%%%
%%% 设计原则：
%%% - 减少样板代码
%%% - 统一工具定义风格
%%% - 类型安全
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_tool_defs).

%%====================================================================
%% 导出 API
%%====================================================================

%% 参数定义辅助
-export([
    string_param/1,
    int_param/1,
    bool_param/1,
    array_param/2,
    enum_param/2
]).

%% 工具构建
-export([
    make_tool/4,
    make_tool/5
]).

%%====================================================================
%% 参数定义辅助函数
%%====================================================================

%% @doc 创建字符串类型参数定义
-spec string_param(binary()) -> map().
string_param(Description) ->
    #{type => string, description => Description}.

%% @doc 创建整数类型参数定义
-spec int_param(binary()) -> map().
int_param(Description) ->
    #{type => integer, description => Description}.

%% @doc 创建布尔类型参数定义
-spec bool_param(binary()) -> map().
bool_param(Description) ->
    #{type => boolean, description => Description}.

%% @doc 创建数组类型参数定义
-spec array_param(map(), binary()) -> map().
array_param(ItemType, Description) ->
    #{type => array, items => ItemType, description => Description}.

%% @doc 创建枚举类型参数定义
-spec enum_param([binary()], binary()) -> map().
enum_param(Values, Description) ->
    #{type => string, enum => Values, description => Description}.

%%====================================================================
%% 工具构建函数
%%====================================================================

%% @doc 创建工具定义（所有参数必需）
%%
%% @param Name 工具名称
%% @param Description 工具描述
%% @param Properties 参数属性定义
%% @param Handler 处理器函数
-spec make_tool(binary(), binary(), map(), function()) -> map().
make_tool(Name, Description, Properties, Handler) ->
    make_tool(Name, Description, Properties, maps:keys(Properties), Handler).

%% @doc 创建工具定义（指定必需参数）
%%
%% @param Name 工具名称
%% @param Description 工具描述
%% @param Properties 参数属性定义
%% @param Required 必需参数列表
%% @param Handler 处理器函数
-spec make_tool(binary(), binary(), map(), [binary()], function()) -> map().
make_tool(Name, Description, Properties, Required, Handler) ->
    #{
        name => Name,
        description => Description,
        parameters => #{
            type => object,
            properties => Properties,
            required => Required
        },
        handler => Handler
    }.
