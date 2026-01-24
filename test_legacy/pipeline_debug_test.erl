%% pipeline_debug_test.erl
%% Pipeline 调试测试

-module(pipeline_debug_test).
-export([run/0]).

run() ->
    io:format("=== Pipeline Debug Test ===~n~n"),

    %% 1. 创建 LLM 配置
    ApiKey = os:getenv("ZHIPU_API_KEY"),
    case ApiKey of
        false ->
            io:format("ERROR: ZHIPU_API_KEY not set~n"),
            {error, no_api_key};
        _ ->
            run_test(list_to_binary(ApiKey))
    end.

run_test(ApiKey) ->
    LLM = llm_client:create(anthropic, #{
        model => <<"glm-4.7">>,
        api_key => ApiKey,
        base_url => <<"https://open.bigmodel.cn/api/anthropic">>
    }),

    io:format("1. LLM Config created~n"),

    %% 2. 先测试单独的 LLM 调用
    io:format("~n2. Testing direct LLM call...~n"),
    Messages = [#{role => user, content => <<"Hello, respond with just 'OK'">>}],
    case llm_client:chat(LLM, Messages, #{}) of
        {ok, Resp} ->
            io:format("   LLM Response: ~p~n", [Resp]);
        {error, Err} ->
            io:format("   LLM Error: ~p~n", [Err])
    end,

    %% 3. 测试带工具的 LLM 调用
    io:format("~n3. Testing LLM call with tools...~n"),
    Tools = [#{
        name => <<"test_tool">>,
        description => <<"A test tool. Call this with task parameter.">>,
        input_schema => #{
            type => object,
            properties => #{
                <<"task">> => #{type => string, description => <<"Task to do">>}
            },
            required => [<<"task">>]
        }
    }],

    ToolMessages = [#{role => user, content => <<"Use the test_tool to say hello">>}],
    case llm_client:chat(LLM, ToolMessages, #{tools => Tools}) of
        {ok, Resp2} ->
            io:format("   Tool LLM Response: ~p~n~n", [Resp2]),
            io:format("   - content: ~p~n", [maps:get(content, Resp2, null)]),
            io:format("   - tool_calls: ~p~n", [maps:get(tool_calls, Resp2, [])]),
            io:format("   - finish_reason: ~p~n", [maps:get(finish_reason, Resp2, undefined)]);
        {error, Err2} ->
            io:format("   Tool LLM Error: ~p~n", [Err2])
    end,

    %% 4. 测试 Pipeline 创建
    io:format("~n4. Creating Pipeline...~n"),
    {ok, Team} = beamai_agent:start_pipeline(<<"debug_team">>, #{
        agents => [
            #{name => <<"worker1">>, system_prompt => <<"You are worker 1.">>}
        ],
        llm => LLM
    }),
    io:format("   Pipeline created: ~p~n", [Team]),

    %% 5. 测试 Pipeline 运行（带回调）
    io:format("~n5. Running Pipeline with callbacks...~n"),
    beamai_agent:set_callbacks(Team, #{
        on_llm_start => fun(Msgs, _) ->
            io:format("   [on_llm_start] Messages count: ~p~n", [length(Msgs)])
        end,
        on_llm_end => fun(Resp, _) ->
            io:format("   [on_llm_end] Response: ~p~n", [Resp])
        end,
        on_llm_error => fun(Err, _) ->
            io:format("   [on_llm_error] Error: ~p~n", [Err])
        end,
        on_tool_start => fun(Name, Args, _) ->
            io:format("   [on_tool_start] Tool: ~s, Args: ~p~n", [Name, Args])
        end,
        on_tool_end => fun(Name, Result, _) ->
            io:format("   [on_tool_end] Tool: ~s, Result: ~p~n", [Name, Result])
        end
    }),

    Result = beamai_agent:run(Team, <<"Just say hello">>),
    io:format("~n6. Pipeline Result: ~p~n", [Result]),

    %% 清理
    beamai_agent:stop(Team),

    ok.
