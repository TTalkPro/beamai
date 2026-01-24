%%%-------------------------------------------------------------------
%%% @doc 通用数据变换 Step
%%%
%%% 收集所有 required_inputs 后，应用 transform 函数，
%%% 将结果存入 state 并发出输出事件。
%%%
%%% == 配置 ==
%%%   transform — fun(Inputs :: map()) -> Result :: term()
%%%   output_event — 输出事件名（默认 transformed）
%%%   required_inputs — 必须收集的输入列表
%%%
%%% == 示例 ==
%%% ```
%%% Config = #{
%%%     required_inputs => [a, b],
%%%     transform => fun(#{a := A, b := B}) -> #{sum => A + B} end,
%%%     output_event => result_ready
%%% }
%%% '''
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_process_step_transform).

-behaviour(beamai_step_behaviour).

-export([init/1, can_activate/2, on_activate/3]).

init(Config) ->
    TransformFn = maps:get(transform, Config, fun(X) -> X end),
    OutputEvent = maps:get(output_event, Config, transformed),
    {ok, #{
        transform => TransformFn,
        output_event => OutputEvent,
        result => undefined
    }}.

can_activate(_Inputs, _State) ->
    true.

on_activate(Inputs, #{transform := Fn, output_event := OutputEvent} = State, _Context) ->
    Result = Fn(Inputs),
    Event = beamai_process_event:new(OutputEvent, Result),
    {ok, #{events => [Event], state => State#{result => Result}}}.
