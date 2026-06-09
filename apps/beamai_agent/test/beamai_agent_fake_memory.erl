%%%-------------------------------------------------------------------
%%% @doc 测试用自定义记忆 Provider（演示 beamai_memory_provider 协议可 DIY）
%%%
%%% 用一个命名 ETS 表存历史，并统计 prepare/append 调用次数，证明 agent 确实
%%% 通过协议把记忆委托给了本模块。Ref = ETS 表名（atom）。
%%%
%%% 协议（agent 自管编排）：history(载入跨轮)/append(持久化)/prepare(发送前变换)/clear。
%%% 这里 prepare 恒等（不裁剪），只统计被调次数。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_agent_fake_memory).

-behaviour(beamai_memory_provider).

-export([new/1, count/2]).
-export([history/2, append/3, prepare/3, clear/2]).

%% @doc 构造 provider 句柄（顺带建表）
new(Tab) ->
    catch ets:new(Tab, [named_table, public, set]),
    {?MODULE, Tab}.

%% @doc 读取某类调用计数
count(Tab, Key) ->
    case ets:lookup(Tab, {count, Key}) of
        [{_, N}] -> N;
        [] -> 0
    end.

%%====================================================================
%% beamai_memory_provider 回调
%%====================================================================

history(Tab, ConvId) ->
    bump(Tab, history),
    hist(Tab, ConvId).

append(Tab, ConvId, Msgs) ->
    bump(Tab, append),
    ets:insert(Tab, {{hist, ConvId}, hist(Tab, ConvId) ++ Msgs}),
    ok.

prepare(Tab, _ConvId, Messages) ->
    bump(Tab, prepare),
    Messages.

clear(Tab, ConvId) ->
    ets:insert(Tab, {{hist, ConvId}, []}),
    ok.

%%====================================================================
%% 内部
%%====================================================================

hist(Tab, ConvId) ->
    case ets:lookup(Tab, {hist, ConvId}) of
        [{_, H}] -> H;
        [] -> []
    end.

bump(Tab, Key) ->
    ets:insert(Tab, {{count, Key}, count(Tab, Key) + 1}).
