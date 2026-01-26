%%%-------------------------------------------------------------------
%%% @doc 工具系统公共 API
%%%
%%% 提供对工具系统的统一访问接口：
%%% - 工具模块加载（load/2, load_all/2, available/0）
%%% - 中间件集成（with_middleware/2, presets/1）
%%% - 工具定义 DSL 快捷方式（define_tool/3, define_tool/4, define_tool/5）
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% K0 = beamai:kernel(),
%%% %% 加载工具模块
%%% K1 = beamai_tools:load(K0, beamai_tool_file),
%%% %% 添加中间件
%%% K2 = beamai_tools:with_middleware(K1, beamai_middleware_presets:default()),
%%% %% 添加 LLM 配置并使用
%%% K3 = beamai:add_llm(K2, LlmConfig),
%%% {ok, Response, _Ctx} = beamai:chat_with_tools(K3, Messages).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tools).

%% 工具加载相关 API
-export([load/2, load_all/2, available/0]).

%% 中间件集成相关 API
-export([with_middleware/2, presets/1]).

%% 工具定义 DSL 快捷方式
-export([define_tool/3, define_tool/4, define_tool/5]).

%%====================================================================
%% 工具加载
%%====================================================================

%% @doc 加载工具模块到 kernel。
%%
%% 指定的模块必须实现 beamai_tool_behaviour 的回调函数
%% （tool_info/0 和 tools/0）。
%%
%% 同时会加载工具模块通过 filters/0 回调定义的所有过滤器。
%%
%% @param Kernel  当前的 Kernel 实例
%% @param Module  实现了 beamai_tool_behaviour 的工具模块名
%% @returns 注册了工具后的更新 Kernel 实例
-spec load(beamai_kernel:kernel(), module()) -> beamai_kernel:kernel().
load(Kernel, Module) ->
    K1 = beamai_kernel:add_tool_module(Kernel, Module),
    %% 如果工具模块定义了 filters/0 回调，则加载对应的过滤器
    case erlang:function_exported(Module, filters, 0) of
        true ->
            Filters = Module:filters(),
            lists:foldl(fun(F, K) -> beamai_kernel:add_filter(K, F) end, K1, Filters);
        false ->
            K1
    end.

%% @doc 批量加载多个工具模块到 kernel。
%%
%% 按顺序依次加载所有指定的工具模块，每个模块的过滤器也会被加载。
%%
%% @param Kernel   当前的 Kernel 实例
%% @param Modules  工具模块名列表
%% @returns 加载所有工具后的更新 Kernel 实例
-spec load_all(beamai_kernel:kernel(), [module()]) -> beamai_kernel:kernel().
load_all(Kernel, Modules) ->
    lists:foldl(fun(M, K) -> load(K, M) end, Kernel, Modules).

%% @doc 列出所有内置工具模块。
%%
%% 返回系统内置可用的工具模块列表，包括文件操作、Shell 命令、
%% TODO 列表和人机交互等工具。
%%
%% @returns 内置工具模块名列表
-spec available() -> [module()].
available() ->
    [
        beamai_tool_file,
        beamai_tool_shell,
        beamai_tool_todo,
        beamai_tool_human
    ].

%%====================================================================
%% 中间件集成
%%====================================================================

%% @doc 初始化中间件链并添加 filters 到 kernel。
%%
%% 将中间件规格列表通过中间件运行器转换为 kernel 过滤器，
%% 然后逐一添加到 kernel 中。
%%
%% @param Kernel          当前的 Kernel 实例
%% @param MiddlewareSpecs 中间件规格列表，格式为 [{Module, Opts}, ...]
%% @returns 添加了中间件过滤器后的更新 Kernel 实例
-spec with_middleware(beamai_kernel:kernel(), [term()]) -> beamai_kernel:kernel().
with_middleware(Kernel, MiddlewareSpecs) ->
    Chain = beamai_middleware_runner:init(MiddlewareSpecs),
    Filters = beamai_middleware_runner:to_filters(Chain),
    lists:foldl(fun(F, K) -> beamai_kernel:add_filter(K, F) end, Kernel, Filters).

%% @doc 获取预设中间件配置。
%%
%% 根据预设名称返回对应的中间件规格列表。
%% 支持的预设名称：default（默认）、minimal（最小化）、
%% production（生产环境）、development（开发环境）、human_in_loop（人机协作）。
%% 如果传入未知的预设名称，则返回默认配置。
%%
%% @param PresetName 预设名称原子值
%% @returns 对应预设的中间件规格列表
-spec presets(atom()) -> [term()].
presets(default) -> beamai_middleware_presets:default();
presets(minimal) -> beamai_middleware_presets:minimal();
presets(production) -> beamai_middleware_presets:production();
presets(development) -> beamai_middleware_presets:development();
presets(human_in_loop) -> beamai_middleware_presets:human_in_loop();
presets(_) -> beamai_middleware_presets:default().

%%====================================================================
%% 工具定义 DSL
%%====================================================================

%% @doc 工具定义 DSL 快捷方式（仅处理器，无参数定义）。
%%
%% 定义一个只有名称、描述和处理器函数的工具，不包含参数规格。
%%
%% @param Name        工具名称（二进制字符串）
%% @param Description 工具描述（二进制字符串）
%% @param Handler     工具处理器函数
%% @returns 工具定义映射
-spec define_tool(binary(), binary(), function()) -> beamai_tool:tool_spec().
define_tool(Name, Description, Handler) ->
    beamai_tool:new(Name, Handler, #{description => Description}).

%% @doc 工具定义 DSL 快捷方式（带参数定义）。
%%
%% 定义一个包含参数规格的工具。
%%
%% @param Name        工具名称（二进制字符串）
%% @param Description 工具描述（二进制字符串）
%% @param Params      参数定义（列表或映射格式）
%% @param Handler     工具处理器函数
%% @returns 工具定义映射
-spec define_tool(binary(), binary(), list() | map(), function()) -> beamai_tool:tool_spec().
define_tool(Name, Description, Params, Handler) ->
    beamai_tool:new(Name, Handler, #{description => Description, parameters => Params}).

%% @doc 工具定义 DSL 快捷方式（带选项和参数定义）。
%%
%% 定义一个包含附加选项（如分类、权限、元数据）和参数规格的工具。
%%
%% @param Name        工具名称（二进制字符串）
%% @param Description 工具描述（二进制字符串）
%% @param Opts        附加选项映射（category, permissions, metadata 等）
%% @param Params      参数定义（列表或映射格式）
%% @param Handler     工具处理器函数
%% @returns 工具定义映射
-spec define_tool(binary(), binary(), map(), list() | map(), function()) -> beamai_tool:tool_spec().
define_tool(Name, Description, Opts, Params, Handler) ->
    beamai_tool:new(Name, Handler, maps:merge(Opts, #{description => Description, parameters => Params})).
