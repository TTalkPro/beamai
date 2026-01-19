%%%-------------------------------------------------------------------
%%% @doc LLM 客户端适配器
%%%
%%% 封装 LLM 客户端调用，支持运行时注入自定义实现。
%%% 默认使用 llm_client (beamai_llm)，但可以通过配置替换。
%%%
%%% 这使得 beamai_tools 不需要硬依赖 beamai_llm。
%%%
%%% == 配置方式 ==
%%%
%%% 1. Application 环境变量（全局配置）：
%%% ```
%%% application:set_env(beamai_tools, llm_module, my_llm_client).
%%% ```
%%%
%%% 2. 调用时传入（局部配置）：
%%% ```
%%% beamai_llm_adapter:chat(Config, Request, #{llm_module => my_llm_client}).
%%% ```
%%%
%%% == 自定义实现 ==
%%%
%%% 自定义模块需实现 beamai_llm_behaviour：
%%% ```
%%% -module(my_llm_client).
%%% -behaviour(beamai_llm_behaviour).
%%% -export([chat/2, is_valid_config/1]).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_adapter).

%% API
-export([chat/2, chat/3]).
-export([is_valid_config/1, is_valid_config/2]).
-export([get_module/0, get_module/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 发送聊天请求到 LLM
%%
%% 使用默认或配置的 LLM 模块发送请求。
%%
%% @param Config LLM 配置
%% @param Request 请求参数
%% @returns {ok, Response} | {error, Reason}
-spec chat(Config :: map(), Request :: map()) ->
    {ok, Response :: map()} | {error, term()}.
chat(Config, Request) ->
    chat(Config, Request, #{}).

%% @doc 发送聊天请求到 LLM（带选项）
%%
%% @param Config LLM 配置
%% @param Request 请求参数，格式为 #{messages => [...], tools => [...], ...}
%% @param Opts 选项，可包含 llm_module 指定自定义模块
%% @returns {ok, Response} | {error, Reason}
-spec chat(Config :: map(), Request :: map(), Opts :: map()) ->
    {ok, Response :: map()} | {error, term()}.
chat(Config, Request, Opts) ->
    Module = get_module(Opts),
    %% 提取 messages 和其他选项
    %% llm_client:chat/3 期望 (Config, Messages, Opts)，其中 Messages 是列表
    Messages = maps:get(messages, Request, []),
    RequestOpts = maps:without([messages], Request),
    try
        Module:chat(Config, Messages, RequestOpts)
    catch
        error:undef ->
            {error, {llm_module_not_found, Module}};
        Class:Reason:Stacktrace ->
            logger:error("LLM adapter call failed: ~p:~p~n~p",
                        [Class, Reason, Stacktrace]),
            {error, {llm_call_failed, Reason}}
    end.

%% @doc 验证 LLM 配置是否有效
%%
%% @param Config 要验证的配置
%% @returns true | false
-spec is_valid_config(Config :: map()) -> boolean().
is_valid_config(Config) ->
    is_valid_config(Config, #{}).

%% @doc 验证 LLM 配置是否有效（带选项）
%%
%% @param Config 要验证的配置
%% @param Opts 选项
%% @returns true | false
-spec is_valid_config(Config :: map(), Opts :: map()) -> boolean().
is_valid_config(Config, Opts) ->
    Module = get_module(Opts),
    try
        Module:is_valid_config(Config)
    catch
        error:undef ->
            %% 模块不存在或未实现该函数，返回 false
            false;
        _:_ ->
            false
    end.

%% @doc 获取当前配置的 LLM 模块
%%
%% @returns 模块名
-spec get_module() -> module().
get_module() ->
    get_module(#{}).

%% @doc 获取 LLM 模块（支持选项覆盖）
%%
%% 优先级：
%% 1. Opts 中的 llm_module
%% 2. Application 环境变量 beamai_tools.llm_module
%% 3. 默认值 llm_client
%%
%% @param Opts 选项
%% @returns 模块名
-spec get_module(Opts :: map()) -> module().
get_module(Opts) ->
    case maps:get(llm_module, Opts, undefined) of
        undefined ->
            application:get_env(beamai_tools, llm_module, llm_client);
        Module ->
            Module
    end.
