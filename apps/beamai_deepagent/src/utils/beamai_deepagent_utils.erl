%%%-------------------------------------------------------------------
%%% @doc Graph Deep Agent 公共工具模块
%%%
%%% 提供各模块共享的工具函数，遵循单一职责原则：
%%% - 状态操作辅助函数：简化 graph_state 访问
%%% - 轨迹记录辅助函数：统一轨迹管理
%%% - 消息处理辅助函数：消息列表操作
%%% - 数据转换辅助函数：类型转换和序列化
%%% - 工具定义辅助函数：简化工具规格创建
%%%
%%% 设计原则：
%%% - 纯函数：所有函数无副作用
%%% - 高复用：被多个模块通过 -import 引用
%%% - 单一职责：每个函数只做一件事
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_utils).

%%====================================================================
%% 导出 API
%%====================================================================

%% 状态操作
-export([
    state_get/2,
    state_get/3,
    state_set/3
]).

%% 轨迹操作
-export([
    add_trace/3
]).

%% 消息操作
-export([
    prepend_system_prompt/2,
    extract_last_assistant_response/1
]).

%% 数据转换
-export([
    parse_json/1
]).

%% 工具定义辅助
-export([
    make_string_param/1,
    make_integer_param/1,
    make_boolean_param/1,
    make_array_param/2
]).

%% 默认值
-export([
    default_system_prompt/0
]).

%%====================================================================
%% 状态操作辅助函数
%%====================================================================

%% @doc 从状态获取值（无默认值版本）
%%
%% 封装 graph_state:get/2，供其他模块通过 -import 使用。
%% 如果键不存在，返回 undefined。
-spec state_get(graph_state:state(), atom()) -> term().
state_get(State, Key) ->
    graph_state:get(State, Key).

%% @doc 从状态获取值（带默认值）
%%
%% 封装 graph_state:get/3，键不存在时返回指定默认值。
-spec state_get(graph_state:state(), atom(), term()) -> term().
state_get(State, Key, Default) ->
    graph_state:get(State, Key, Default).

%% @doc 设置状态值
%%
%% 封装 graph_state:set/3，返回更新后的状态。
-spec state_set(graph_state:state(), atom(), term()) -> graph_state:state().
state_set(State, Key, Value) ->
    graph_state:set(State, Key, Value).

%%====================================================================
%% 轨迹操作辅助函数
%%====================================================================

%% @doc 添加轨迹记录
%%
%% 向状态中的轨迹添加新记录。
%% 如果轨迹不存在则创建新轨迹。
-spec add_trace(graph_state:state(), atom(), term()) -> graph_state:state().
add_trace(State, Type, Data) ->
    Trace = state_get(State, trace, beamai_deepagent_trace:new()),
    NewTrace = beamai_deepagent_trace:add(Trace, Type, Data),
    state_set(State, trace, NewTrace).

%%====================================================================
%% 消息操作辅助函数
%%====================================================================

%% @doc 在消息前添加系统提示
%%
%% 创建包含系统提示的完整消息列表，用于 LLM 调用。
%% 委托给 beamai_message:prepend_system/2
-spec prepend_system_prompt(binary(), [map()]) -> [map()].
prepend_system_prompt(SystemPrompt, Messages) ->
    beamai_message:prepend_system(SystemPrompt, Messages).

%% @doc 从消息列表提取最后的助手响应
%%
%% 使用 beamai_message:extract_last_content/1 获取最后的助手消息内容。
%% 如果没有找到，返回默认提示信息。
-spec extract_last_assistant_response([map()]) -> binary().
extract_last_assistant_response(Messages) ->
    case beamai_message:extract_last_content(Messages) of
        <<>> -> <<"No response generated">>;
        Content -> Content
    end.

%%====================================================================
%% 数据转换辅助函数
%%====================================================================

%% @doc 解析 JSON 字符串为 map
%%
%% 安全解析，解析失败时返回空 map 而非抛出异常。
%% 如果输入已经是 map，直接返回。
%% 委托给 beamai_utils:parse_json/1
-spec parse_json(binary() | map()) -> map().
parse_json(Data) ->
    beamai_utils:parse_json(Data).

%%====================================================================
%% 工具定义辅助函数
%%====================================================================

%% @doc 创建字符串类型参数定义
%%
%% 用于工具参数定义中的字符串字段
-spec make_string_param(binary()) -> map().
make_string_param(Description) ->
    #{type => string, description => Description}.

%% @doc 创建整数类型参数定义
%%
%% 用于工具参数定义中的整数字段
-spec make_integer_param(binary()) -> map().
make_integer_param(Description) ->
    #{type => integer, description => Description}.

%% @doc 创建布尔类型参数定义
%%
%% 用于工具参数定义中的布尔字段
-spec make_boolean_param(binary()) -> map().
make_boolean_param(Description) ->
    #{type => boolean, description => Description}.

%% @doc 创建数组类型参数定义
%%
%% 用于工具参数定义中的数组字段。
%% ItemType 指定数组元素的类型 schema。
-spec make_array_param(map(), binary()) -> map().
make_array_param(ItemType, Description) ->
    #{type => array, items => ItemType, description => Description}.

%%====================================================================
%% 默认值
%%====================================================================

%% @doc 默认系统提示词
%%
%% 定义 Deep Agent 的默认行为和能力说明。
%% 包括计划、执行、反思和并行处理四大能力。
-spec default_system_prompt() -> binary().
default_system_prompt() ->
    <<"You are a helpful AI assistant with advanced planning and execution capabilities.

Your abilities:
1. **Planning**: Create structured plans for complex tasks using create_plan tool
2. **Execution**: Execute tools and subtasks to accomplish goals
3. **Reflection**: Analyze progress and adjust strategy using reflect tool
4. **Parallel Processing**: Spawn subtasks for parallel execution

Guidelines:
- For complex tasks, always create a plan first
- Break down large tasks into smaller, manageable steps
- Use reflection when encountering obstacles
- Leverage parallel subtasks for independent work
- Provide clear, concise responses

Always think step by step and use the available tools effectively.">>.
