%%%-------------------------------------------------------------------
%%% @doc Todo List Middleware - 任务规划与跟踪
%%%
%%% 为 Agent 提供任务规划和跟踪能力，帮助处理复杂的多步骤任务。
%%%
%%% == 功能特性 ==
%%%
%%% - 自动注入 write_todos 工具
%%% - 任务状态跟踪（pending/in_progress/completed）
%%% - 任务持久化
%%% - 引导提示词增强
%%% - 任务完成回调
%%%
%%% == 配置示例 ==
%%%
%%% ```erlang
%%% {middleware_todo_list, #{
%%%     %% 是否在系统提示词中添加任务指导
%%%     add_guidance => true,
%%%
%%%     %% 自定义指导提示词
%%%     guidance_prompt => <<"Use the write_todos tool to plan complex tasks...">>,
%%%
%%%     %% 任务完成回调
%%%     on_task_complete => fun(Task) -> ok,
%%%
%%%     %% 所有任务完成回调
%%%     on_all_complete => fun(Tasks) -> ok,
%%%
%%%     %% 初始任务列表
%%%     initial_todos => [],
%%%
%%%     %% 调试模式
%%%     debug => false
%%% }}
%%% ```
%%%
%%% == 注入的工具 ==
%%%
%%% write_todos - 写入/更新任务列表
%%% ```
%%% Input: {
%%%     "todos": [
%%%         {"content": "任务描述", "status": "pending|in_progress|completed"}
%%%     ]
%%% }
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_todo_list).

-behaviour(beamai_middleware).

%% Middleware 回调
-export([init/1, before_agent/2, after_tools/2]).

%% 工具函数
-export([
    get_todos/1,
    set_todos/2,
    add_todo/2,
    update_todo/3,
    complete_todo/2,
    get_todo_summary/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type todo_status() :: pending | in_progress | completed.

-type todo() :: #{
    content := binary(),
    status := todo_status(),
    created_at => integer(),
    updated_at => integer()
}.

%%====================================================================
%% Middleware 回调
%%====================================================================

%% @doc 初始化 Middleware 状态
-spec init(map()) -> map().
init(Opts) ->
    #{
        add_guidance => maps:get(add_guidance, Opts, true),
        guidance_prompt => maps:get(guidance_prompt, Opts, default_guidance_prompt()),
        on_task_complete => maps:get(on_task_complete, Opts, undefined),
        on_all_complete => maps:get(on_all_complete, Opts, undefined),
        initial_todos => maps:get(initial_todos, Opts, []),
        debug => maps:get(debug, Opts, false)
    }.

%% @doc Agent 开始时注入工具和初始化
-spec before_agent(map(), map()) -> beamai_middleware:middleware_result().
before_agent(State, MwState) ->
    #{add_guidance := AddGuidance,
      guidance_prompt := GuidancePrompt,
      initial_todos := InitialTodos} = MwState,

    %% 注入 write_todos 工具
    Tools = graph:get(State, tools, []),
    TodoTool = build_todo_tool(MwState),
    NewTools = [TodoTool | Tools],

    %% 初始化任务列表
    CurrentTodos = graph:get(State, mw_todos, InitialTodos),

    %% 可选：增强系统提示词
    Updates = #{
        tools => NewTools,
        mw_todos => CurrentTodos
    },

    FinalUpdates = case AddGuidance of
        true ->
            SystemPrompt = graph:get(State, system_prompt, <<>>),
            EnhancedPrompt = <<SystemPrompt/binary, "\n\n", GuidancePrompt/binary>>,
            Updates#{system_prompt => EnhancedPrompt};
        false ->
            Updates
    end,

    {update, FinalUpdates}.

%% @doc 工具执行后检查任务状态
-spec after_tools(map(), map()) -> beamai_middleware:middleware_result().
after_tools(State, MwState) ->
    #{on_task_complete := OnTaskComplete,
      on_all_complete := OnAllComplete,
      debug := Debug} = MwState,

    Todos = graph:get(State, mw_todos, []),

    %% 检查新完成的任务
    PreviousCompleted = graph:get(State, mw_completed_tasks, []),
    CurrentCompleted = [T || T <- Todos, maps:get(status, T) =:= completed],

    NewlyCompleted = lists:filter(fun(T) ->
        not lists:member(T, PreviousCompleted)
    end, CurrentCompleted),

    %% 调用任务完成回调
    lists:foreach(fun(Task) ->
        maybe_call_callback(OnTaskComplete, Task),
        case Debug of
            true ->
                logger:info("[TodoList] 任务完成: ~s",
                           [maps:get(content, Task, <<>>)]);
            false ->
                ok
        end
    end, NewlyCompleted),

    %% 检查是否所有任务都完成
    AllCompleted = length(CurrentCompleted) =:= length(Todos) andalso length(Todos) > 0,
    case AllCompleted andalso NewlyCompleted =/= [] of
        true ->
            maybe_call_callback(OnAllComplete, Todos),
            case Debug of
                true ->
                    logger:info("[TodoList] 所有任务已完成");
                false ->
                    ok
            end;
        false ->
            ok
    end,

    {update, #{mw_completed_tasks => CurrentCompleted}}.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 获取当前任务列表
-spec get_todos(map()) -> [todo()].
get_todos(State) ->
    graph:get(State, mw_todos, []).

%% @doc 设置任务列表
-spec set_todos(map(), [todo()]) -> map().
set_todos(State, Todos) ->
    Now = erlang:system_time(millisecond),
    NormalizedTodos = lists:map(fun(T) ->
        normalize_todo(T, Now)
    end, Todos),
    graph:set(State, mw_todos, NormalizedTodos).

%% @doc 添加任务
-spec add_todo(map(), todo() | binary()) -> map().
add_todo(State, Content) when is_binary(Content) ->
    add_todo(State, #{content => Content, status => pending});
add_todo(State, Todo) when is_map(Todo) ->
    Todos = get_todos(State),
    Now = erlang:system_time(millisecond),
    NormalizedTodo = normalize_todo(Todo, Now),
    set_todos(State, Todos ++ [NormalizedTodo]).

%% @doc 更新任务状态
-spec update_todo(map(), non_neg_integer(), todo_status()) -> map().
update_todo(State, Index, NewStatus) ->
    Todos = get_todos(State),
    case Index >= 0 andalso Index < length(Todos) of
        true ->
            Now = erlang:system_time(millisecond),
            UpdatedTodos = lists:map(fun({I, T}) ->
                case I =:= Index of
                    true -> T#{status => NewStatus, updated_at => Now};
                    false -> T
                end
            end, lists:zip(lists:seq(0, length(Todos) - 1), Todos)),
            graph:set(State, mw_todos, UpdatedTodos);
        false ->
            State
    end.

%% @doc 标记任务为完成
-spec complete_todo(map(), non_neg_integer()) -> map().
complete_todo(State, Index) ->
    update_todo(State, Index, completed).

%% @doc 获取任务摘要
-spec get_todo_summary(map()) -> map().
get_todo_summary(State) ->
    Todos = get_todos(State),
    #{
        total => length(Todos),
        pending => length([T || T <- Todos, maps:get(status, T) =:= pending]),
        in_progress => length([T || T <- Todos, maps:get(status, T) =:= in_progress]),
        completed => length([T || T <- Todos, maps:get(status, T) =:= completed])
    }.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 默认指导提示词
-spec default_guidance_prompt() -> binary().
default_guidance_prompt() ->
    <<"## Task Planning\n\n"
      "For complex tasks that require multiple steps:\n"
      "1. Use the `write_todos` tool to create a task list\n"
      "2. Update task status as you work (pending -> in_progress -> completed)\n"
      "3. Mark tasks complete when finished\n\n"
      "Task statuses:\n"
      "- `pending`: Not started\n"
      "- `in_progress`: Currently working on\n"
      "- `completed`: Finished\n">>.

%% @private 构建 todo 工具
-spec build_todo_tool(map()) -> map().
build_todo_tool(MwState) ->
    #{debug := Debug} = MwState,

    #{
        name => <<"write_todos">>,
        description => <<"Create or update a task list for planning complex operations. "
                        "Use this to break down complex tasks into smaller steps and track progress.">>,
        input_schema => #{
            type => object,
            properties => #{
                <<"todos">> => #{
                    type => array,
                    description => <<"List of tasks">>,
                    items => #{
                        type => object,
                        properties => #{
                            <<"content">> => #{
                                type => string,
                                description => <<"Task description">>
                            },
                            <<"status">> => #{
                                type => string,
                                enum => [<<"pending">>, <<"in_progress">>, <<"completed">>],
                                description => <<"Task status">>
                            }
                        },
                        required => [<<"content">>, <<"status">>]
                    }
                }
            },
            required => [<<"todos">>]
        },
        handler => fun(#{<<"todos">> := TodosInput}, Context) ->
            handle_write_todos(TodosInput, Context, Debug)
        end
    }.

%% @private 处理 write_todos 工具调用
-spec handle_write_todos([map()], map(), boolean()) -> {ok, binary()}.
handle_write_todos(TodosInput, Context, Debug) ->
    %% 转换输入格式
    Now = erlang:system_time(millisecond),
    Todos = lists:map(fun(T) ->
        Content = maps:get(<<"content">>, T, <<>>),
        StatusBin = maps:get(<<"status">>, T, <<"pending">>),
        Status = binary_to_existing_atom(StatusBin, utf8),
        #{
            content => Content,
            status => Status,
            created_at => Now,
            updated_at => Now
        }
    end, TodosInput),

    %% 更新状态（通过返回特殊格式让 middleware 处理）
    %% 注意：这里需要通过 Context 或其他机制更新状态
    %% 简化实现：返回格式化的任务列表

    case Debug of
        true ->
            logger:info("[TodoList] 更新任务列表: ~p 个任务", [length(Todos)]);
        false ->
            ok
    end,

    %% 格式化输出
    Summary = format_todos_summary(Todos),
    {ok, Summary, #{mw_todos => Todos}}.

%% @private 格式化任务摘要
-spec format_todos_summary([todo()]) -> binary().
format_todos_summary(Todos) ->
    Lines = lists:map(fun({Idx, Todo}) ->
        Content = maps:get(content, Todo, <<>>),
        Status = maps:get(status, Todo, pending),
        StatusIcon = case Status of
            completed -> <<"✓">>;
            in_progress -> <<"→">>;
            pending -> <<"○">>
        end,
        io_lib:format("~s ~B. ~s", [StatusIcon, Idx + 1, Content])
    end, lists:zip(lists:seq(0, length(Todos) - 1), Todos)),

    PendingCount = length([T || T <- Todos, maps:get(status, T) =:= pending]),
    InProgressCount = length([T || T <- Todos, maps:get(status, T) =:= in_progress]),
    CompletedCount = length([T || T <- Todos, maps:get(status, T) =:= completed]),

    Summary = io_lib:format(
        "Task list updated:\n~s\n\nSummary: ~B pending, ~B in progress, ~B completed",
        [string:join(Lines, "\n"), PendingCount, InProgressCount, CompletedCount]
    ),

    iolist_to_binary(Summary).

%% @private 规范化任务
-spec normalize_todo(map(), integer()) -> todo().
normalize_todo(Todo, Now) ->
    Content = maps:get(content, Todo, maps:get(<<"content">>, Todo, <<>>)),
    StatusRaw = maps:get(status, Todo, maps:get(<<"status">>, Todo, pending)),
    Status = case StatusRaw of
        S when is_atom(S) -> S;
        <<"pending">> -> pending;
        <<"in_progress">> -> in_progress;
        <<"completed">> -> completed;
        _ -> pending
    end,
    #{
        content => Content,
        status => Status,
        created_at => maps:get(created_at, Todo, Now),
        updated_at => maps:get(updated_at, Todo, Now)
    }.

%% @private 调用回调
-spec maybe_call_callback(function() | undefined, term()) -> ok.
maybe_call_callback(undefined, _) ->
    ok;
maybe_call_callback(Callback, Arg) when is_function(Callback, 1) ->
    try
        Callback(Arg)
    catch
        _:Reason ->
            logger:warning("[TodoList] 回调异常: ~p", [Reason])
    end,
    ok.
