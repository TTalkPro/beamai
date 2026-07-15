%%%-------------------------------------------------------------------
%%% @doc Mock ToolCallingManager 实现（供测试注入）
%%%
%%% 不看 ToolCalls，直接返回固定结果。验证 seam 可替换。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mock_tcm_impl).
-behaviour(beamai_tool_calling_manager).

-export([execute_tool_calls/4]).

-spec execute_tool_calls(term(), beamai_kernel:kernel(), [map()],
                    beamai_tool_calling_manager:execute_opts()) ->
    beamai_tool_calling_manager:execute_result().
execute_tool_calls(_Ref, _Kernel, _ToolCalls, _Opts) ->
    FixedMsg = #{role => tool, tool_call_id => <<"fixed">>, content => <<"mocked">>},
    FixedRecord = #{name => <<"mock">>, result => <<"mocked">>, tool_call_id => <<"fixed">>},
    #{messages => [FixedMsg], records => [FixedRecord], context => beamai_context:new()}.
