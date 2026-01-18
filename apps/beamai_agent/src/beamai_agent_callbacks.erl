%%%-------------------------------------------------------------------
%%% @doc 回调管理模块
%%%
%%% 负责 Agent 回调系统的管理：
%%% - 回调处理器初始化
%%% - 回调更新
%%% - 回调调用
%%% - 回调记录与 Map 转换
%%%
%%% 支持 19 种生命周期回调，覆盖 LLM、Tool、Agent、Chain、Retriever 等。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_callbacks).

-include("beamai_agent.hrl").

%% API 导出
-export([
    init/1,
    update/2,
    invoke/3,
    to_map/1,
    build_metadata/1,
    generate_run_id/0
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 初始化回调处理器
%%
%% 从配置选项初始化回调，兼容旧的 on_complete/on_error 选项。
-spec init(map()) -> #callbacks{}.
init(Opts) ->
    CallbackOpts = maps:get(callbacks, Opts, #{}),
    %% 兼容旧的 on_complete/on_error 选项
    OnComplete = maps:get(on_complete, Opts, undefined),
    OnError = maps:get(on_error, Opts, undefined),

    #callbacks{
        on_llm_start = maps:get(on_llm_start, CallbackOpts, undefined),
        on_llm_end = maps:get(on_llm_end, CallbackOpts, undefined),
        on_llm_error = maps:get(on_llm_error, CallbackOpts, undefined),
        on_llm_new_token = maps:get(on_llm_new_token, CallbackOpts, undefined),
        on_tool_start = maps:get(on_tool_start, CallbackOpts, undefined),
        on_tool_end = maps:get(on_tool_end, CallbackOpts, undefined),
        on_tool_error = maps:get(on_tool_error, CallbackOpts, undefined),
        on_agent_action = maps:get(on_agent_action, CallbackOpts, undefined),
        on_agent_finish = maps:get(on_agent_finish, CallbackOpts,
                                   maps:get(on_agent_finish, CallbackOpts, OnComplete)),
        on_chain_start = maps:get(on_chain_start, CallbackOpts, undefined),
        on_chain_end = maps:get(on_chain_end, CallbackOpts, undefined),
        on_chain_error = maps:get(on_chain_error, CallbackOpts,
                                  maps:get(on_chain_error, CallbackOpts, OnError)),
        on_retriever_start = maps:get(on_retriever_start, CallbackOpts, undefined),
        on_retriever_end = maps:get(on_retriever_end, CallbackOpts, undefined),
        on_retriever_error = maps:get(on_retriever_error, CallbackOpts, undefined),
        on_text = maps:get(on_text, CallbackOpts, undefined),
        on_retry = maps:get(on_retry, CallbackOpts, undefined),
        on_custom_event = maps:get(on_custom_event, CallbackOpts, undefined)
    }.

%% @doc 更新回调处理器
%%
%% 使用新的选项更新回调，未指定的保持原值。
-spec update(#callbacks{}, map()) -> #callbacks{}.
update(Callbacks, Opts) ->
    #callbacks{
        on_llm_start = maps:get(on_llm_start, Opts, Callbacks#callbacks.on_llm_start),
        on_llm_end = maps:get(on_llm_end, Opts, Callbacks#callbacks.on_llm_end),
        on_llm_error = maps:get(on_llm_error, Opts, Callbacks#callbacks.on_llm_error),
        on_llm_new_token = maps:get(on_llm_new_token, Opts, Callbacks#callbacks.on_llm_new_token),
        on_tool_start = maps:get(on_tool_start, Opts, Callbacks#callbacks.on_tool_start),
        on_tool_end = maps:get(on_tool_end, Opts, Callbacks#callbacks.on_tool_end),
        on_tool_error = maps:get(on_tool_error, Opts, Callbacks#callbacks.on_tool_error),
        on_agent_action = maps:get(on_agent_action, Opts, Callbacks#callbacks.on_agent_action),
        on_agent_finish = maps:get(on_agent_finish, Opts, Callbacks#callbacks.on_agent_finish),
        on_chain_start = maps:get(on_chain_start, Opts, Callbacks#callbacks.on_chain_start),
        on_chain_end = maps:get(on_chain_end, Opts, Callbacks#callbacks.on_chain_end),
        on_chain_error = maps:get(on_chain_error, Opts, Callbacks#callbacks.on_chain_error),
        on_retriever_start = maps:get(on_retriever_start, Opts, Callbacks#callbacks.on_retriever_start),
        on_retriever_end = maps:get(on_retriever_end, Opts, Callbacks#callbacks.on_retriever_end),
        on_retriever_error = maps:get(on_retriever_error, Opts, Callbacks#callbacks.on_retriever_error),
        on_text = maps:get(on_text, Opts, Callbacks#callbacks.on_text),
        on_retry = maps:get(on_retry, Opts, Callbacks#callbacks.on_retry),
        on_custom_event = maps:get(on_custom_event, Opts, Callbacks#callbacks.on_custom_event)
    }.

%% @doc 调用回调函数
%%
%% 从回调记录中获取处理器并安全调用。
%% 回调失败只记录警告，不影响主流程。
-spec invoke(atom(), list(), #callbacks{}) -> ok.
invoke(CallbackName, Args, Callbacks) ->
    Handler = get_handler(CallbackName, Callbacks),
    call_handler(Handler, Args).

%% @doc 将回调记录转换为 map
%%
%% 用于传递给图状态。
-spec to_map(#callbacks{}) -> map().
to_map(#callbacks{} = C) ->
    #{
        on_llm_start => C#callbacks.on_llm_start,
        on_llm_end => C#callbacks.on_llm_end,
        on_llm_error => C#callbacks.on_llm_error,
        on_llm_new_token => C#callbacks.on_llm_new_token,
        on_tool_start => C#callbacks.on_tool_start,
        on_tool_end => C#callbacks.on_tool_end,
        on_tool_error => C#callbacks.on_tool_error,
        on_agent_action => C#callbacks.on_agent_action,
        on_agent_finish => C#callbacks.on_agent_finish,
        on_chain_start => C#callbacks.on_chain_start,
        on_chain_end => C#callbacks.on_chain_end,
        on_chain_error => C#callbacks.on_chain_error,
        on_retriever_start => C#callbacks.on_retriever_start,
        on_retriever_end => C#callbacks.on_retriever_end,
        on_retriever_error => C#callbacks.on_retriever_error,
        on_text => C#callbacks.on_text,
        on_retry => C#callbacks.on_retry,
        on_custom_event => C#callbacks.on_custom_event
    }.

%% @doc 构建回调元数据
%%
%% 从状态记录构建元数据 map。
-spec build_metadata(#state{}) -> map().
build_metadata(#state{id = Id, name = Name, run_id = RunId}) ->
    #{
        agent_id => Id,
        agent_name => Name,
        run_id => RunId,
        timestamp => erlang:system_time(millisecond)
    }.

%% @doc 生成运行 ID
%%
%% 生成 UUID 格式的唯一运行标识。
-spec generate_run_id() -> binary().
generate_run_id() ->
    <<A:32, B:16, C:16, D:16, E:48>> = crypto:strong_rand_bytes(16),
    iolist_to_binary(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~4.16.0b-~12.16.0b",
                                   [A, B, C, D, E])).

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 获取回调处理器
-spec get_handler(atom(), #callbacks{}) -> function() | undefined.
get_handler(CallbackName, Callbacks) ->
    CallbackMap = to_map(Callbacks),
    maps:get(CallbackName, CallbackMap, undefined).

%% @private 调用回调处理器
-spec call_handler(function() | undefined, list()) -> ok.
call_handler(undefined, _Args) ->
    ok;
call_handler(Handler, Args) when is_function(Handler) ->
    try
        erlang:apply(Handler, Args)
    catch
        Class:Reason:Stack ->
            logger:warning("回调执行失败: ~p:~p~n堆栈: ~p", [Class, Reason, Stack])
    end,
    ok.
