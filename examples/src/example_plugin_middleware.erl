%%%-------------------------------------------------------------------
%%% @doc Plugin + Middleware Example
%%%
%%% Demonstrates using the beamai_plugin system with middleware.
%%%
%%% Usage:
%%% ```
%%% export ZHIPU_API_KEY=your-api-key
%%% rebar3 shell
%%% example_plugin_middleware:run().
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_plugin_middleware).

-export([run/0, run_simple/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc Full example: Plugin + Middleware + LLM
-spec run() -> ok.
run() ->
    io:format("=== BeamAI Plugin + Middleware Example ===~n~n"),

    %% 1. Create kernel
    K0 = beamai:kernel(),

    %% 2. Load file plugin
    K1 = beamai_plugins:load(K0, beamai_plugin_file),
    io:format("Loaded file plugin: ~p functions~n",
              [length(beamai_kernel:list_functions(K1))]),

    %% 3. Add middleware (call limits + model retry)
    K2 = beamai_plugins:with_middleware(K1, beamai_middleware_presets:default()),
    #{filters := Filters} = K2,
    io:format("Added middleware: ~p filters~n", [length(Filters)]),

    %% 4. Add LLM service
    LLMConfig = example_llm_config:anthropic(),
    K3 = beamai:add_llm(K2, LLMConfig),

    %% 5. Show registered tools
    Tools = beamai:tools(K3, anthropic),
    io:format("Registered tools: ~p~n~n", [[maps:get(<<"name">>, T) || T <- Tools]]),

    %% 6. Chat with tools
    Messages = [
        #{role => system, content => <<"You are a helpful assistant with file system access. Use the file_list tool to answer questions about directories.">>},
        #{role => user, content => <<"List files in the current directory">>}
    ],
    io:format("User: List files in the current directory~n~n"),

    case beamai:chat_with_tools(K3, Messages) of
        {ok, #{content := Content}, _} ->
            io:format("Assistant: ~ts~n~n", [Content]);
        {error, Reason} ->
            io:format("Error: ~p~n", [Reason])
    end,
    ok.

%% @doc Simple example: Plugin loading and direct invocation (no LLM needed)
-spec run_simple() -> ok.
run_simple() ->
    io:format("=== Simple Plugin Example ===~n~n"),

    %% 1. Create kernel with plugins
    K0 = beamai:kernel(),
    K1 = beamai_plugins:load_all(K0, [
        beamai_plugin_file,
        beamai_plugin_todo
    ]),

    %% 2. Add middleware
    K2 = beamai_plugins:with_middleware(K1, beamai_middleware_presets:minimal()),

    %% 3. List available functions
    Functions = beamai_kernel:list_functions(K2),
    io:format("Available functions:~n"),
    lists:foreach(fun(#{name := Name, plugin := Plugin}) ->
        io:format("  ~s.~s~n", [Plugin, Name])
    end, Functions),
    io:format("~n"),

    %% 4. Direct invocation: read todos
    {ok, TodoResult, _} = beamai:invoke(K2, <<"read_todos">>, #{}),
    io:format("Read todos: ~p~n~n", [TodoResult]),

    %% 5. Direct invocation: list files
    {ok, ListResult, _} = beamai:invoke(K2, <<"file_list">>, #{<<"path">> => <<".">>}),
    io:format("File list count: ~p~n", [maps:get(count, ListResult)]),

    ok.
