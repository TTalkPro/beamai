%%%-------------------------------------------------------------------
%%% @doc Tool 安全声明与结果策略测试
%%%
%%% 覆盖 opt-in 参数校验、权限/结果策略元数据校验、结果 redaction 与
%%% 编码后大小上限。未声明安全字段的工具保持旧行为。
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_safety_tests).

-include_lib("eunit/include/eunit.hrl").

strict_args_test_() ->
    [
     {"strict_args=true 时合法参数执行 handler",
      fun() ->
          Tool = strict_tool(fun(_Args) -> {ok, handled} end),
          ?assertEqual({ok, handled},
                       beamai_tool:invoke(Tool, #{<<"query">> => <<"erlang">>}))
      end},

     {"strict_args=true 时非法参数不执行 handler",
      fun() ->
          Tool = strict_tool(fun(_Args) -> {ok, must_not_run} end),
          {error, Reason} = beamai_tool:invoke(Tool, #{}),
          ?assertEqual(semantic, beamai_tool_error:classify(Reason)),
          ?assertMatch(#{reason := invalid_tool_args, errors := [_ | _]},
                       Reason)
      end},

     {"strict_args=true 且参数不是 object 时拒绝执行",
      fun() ->
          Tool = strict_tool(fun(_Args) -> {ok, must_not_run} end),
          {error, Reason} = beamai_tool:invoke(Tool, bad),
          ?assertMatch(#{reason := invalid_tool_args,
                         errors := [args_not_object]}, Reason)
      end},

     {"未声明 strict_args 的工具保持旧行为",
      fun() ->
          Tool = beamai_tool:new(<<"legacy">>, fun(_Args) -> {ok, ok} end),
          ?assertEqual(ok, beamai_tool:validate(Tool)),
          ?assertEqual({ok, ok}, beamai_tool:invoke(Tool, #{}))
      end}
    ].

declaration_validation_test_() ->
    [
     {"安全声明形状合法",
      fun() ->
          Tool = safe_tool(),
          ?assertEqual(ok, beamai_tool:validate(Tool))
      end},

     {"strict_args / max_result_bytes 非法形状报错",
      fun() ->
          Tool = (safe_tool())#{strict_args => maybe_yes,
                                max_result_bytes => 0},
          {error, Errors} = beamai_tool:validate(Tool),
          ?assert(lists:member({invalid_strict_args, maybe_yes}, Errors)),
          ?assert(lists:member({invalid_max_result_bytes, 0}, Errors))
      end},

     {"permissions 只接受约定键且值必须是布尔",
      fun() ->
          Tool = (safe_tool())#{permissions => #{network => yes}},
          ?assertMatch({error, [{invalid_permissions, _}]},
                       beamai_tool:validate(Tool))
      end},

     {"result_redaction 的正则必须可编译",
      fun() ->
          Tool = (safe_tool())#{result_redaction => [{<<"ok">>, <<"x">>}]},
          ?assertEqual(ok, beamai_tool:validate(Tool)),
          Bad = (safe_tool())#{result_redaction => [{<<"(">>, <<"x">>}]},
          ?assertMatch({error, [{invalid_result_redaction, _}]},
                       beamai_tool:validate(Bad))
      end}
    ].

result_policy_test_() ->
    [
     {"executor 对结果应用 redaction 并返回文本",
      fun() ->
          Tool = (safe_tool())#{
              handler => fun(_A) -> {ok, <<"token sk-secret done">>} end
          },
          CC = beamai_chat_client:add_tool(beamai_chat_client:new(), Tool),
          ?assertEqual({ok, <<"token [REDACTED] done">>, #{}},
                       beamai_tool_executor:invoke(
                         CC, <<"search_files">>, #{<<"query">> => <<"x">>},
                                                   beamai_context:new()))
      end},

     {"executor 按 redaction 后的最终文本执行大小上限",
      fun() ->
          Tool = (safe_tool())#{
              max_result_bytes => 8,
              handler => fun(_A) -> {ok, <<"token sk-secret done">>} end
          },
          CC = beamai_chat_client:add_tool(beamai_chat_client:new(), Tool),
          {error, Reason} =
              beamai_tool_executor:invoke(
                CC, <<"search_files">>, #{<<"query">> => <<"x">>},
                beamai_context:new()),
          ?assertEqual(semantic, beamai_tool_error:classify(Reason)),
          ?assertMatch(#{reason := tool_result_too_large,
                         max_bytes := 8}, Reason)
      end},

     {"未配置策略的工具返回原始 term",
      fun() ->
          Tool = beamai_tool:new(<<"raw">>, fun(_A) -> {ok, {tuple, 1}} end),
          CC = beamai_chat_client:add_tool(beamai_chat_client:new(), Tool),
          ?assertEqual({ok, {tuple, 1}, #{}},
                       beamai_tool_executor:invoke(CC, <<"raw">>, #{},
                                                   beamai_context:new()))
      end}
    ].

redact_result_test() ->
    Rules = [{<<"sk-[A-Za-z0-9]+">>, <<"[REDACTED]">>},
             {<<"password=.+">>, <<"password=***">>}],
    {ok, Text} = beamai_tool:redact_result(
                   <<"sk-abc password=hello">>, Rules),
    ?assertEqual(<<"[REDACTED] password=***">>, Text).

strict_tool(Handler) ->
    (safe_tool())#{handler => Handler}.

safe_tool() ->
    #{
        name => <<"search_files">>,
        handler => fun(_Args) -> {ok, ok} end,
        parameters => #{
            query => #{type => string, required => true}
        },
        strict_args => true,
        permissions => #{filesystem => true, network => false},
        result_redaction => [{<<"sk-[A-Za-z0-9]+">>, <<"[REDACTED]">>}]
    }.
