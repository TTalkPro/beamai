%%%-------------------------------------------------------------------
%%% @doc Agent Deep 交互显示工具模块
%%%
%%% 提供交互界面的显示和输入处理函数。
%%%
%%% 核心功能：
%%% - 显示标题和边框
%%% - 显示问题、确认、审批提示
%%% - 读取用户输入
%%% - 验证输入
%%%
%%% 设计原则：
%%% - 纯函数：无副作用（除了 io:format）
%%% - 可复用：多个交互类型共享
%%% - 用户友好：清晰的提示和格式
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_deepagent_cli_ui).

%% API
-export([
    display_title/1,
    display_question/1,
    display_confirmation/1,
    display_approval/1,
    display_options/1,
    display_args/1,
    read_input/0,
    read_yes_no/0
]).

%%====================================================================
%% 显示函数
%%====================================================================

%% @doc 显示标题
%%
%% 根据类型显示不同的标题框。
-spec display_title(atom()) -> ok.
display_title(question) ->
    display_box(<<"需要您的帮助">>);
display_title(confirmation) ->
    display_box(<<"需要您的确认">>);
display_title(approval) ->
    display_box(<<"需要审批工具执行">>);
display_title(_Other) ->
    display_box(<<"Agent 交互">>).

%% @doc 显示问题界面
-spec display_question(map()) -> ok.
display_question(Request) ->
    Question = maps:get(question, Request),
    Context = maps:get(context, Request, <<>>),
    Options = maps:get(options, Request, []),

    display_title(question),
    maybe_display_context(Context),
    io:format("~n问题: ~ts~n", [Question]),
    display_options(Options).

%% @doc 显示确认界面
-spec display_confirmation(map()) -> ok.
display_confirmation(Request) ->
    Action = maps:get(action, Request),
    Reason = maps:get(reason, Request),
    Consequences = maps:get(consequences, Request, <<>>),

    display_title(confirmation),
    io:format("~n操作: ~ts~n", [Action]),
    io:format("原因: ~ts~n", [Reason]),
    maybe_display_consequences(Consequences).

%% @doc 显示审批界面
-spec display_approval(map()) -> ok.
display_approval(Request) ->
    Tool = maps:get(tool, Request),
    Message = maps:get(message, Request),
    Args = maps:get(args, Request, #{}),

    display_title(approval),
    io:format("~n工具: ~ts~n", [Tool]),
    io:format("说明: ~ts~n", [Message]),
    display_args(Args).

%% @doc 显示选项列表
-spec display_options([binary()]) -> ok.
display_options([]) -> ok;
display_options(Options) ->
    io:format("~n可选答案:~n"),
    lists:foreach(fun({Idx, Opt}) ->
        io:format("  [~p] ~ts~n", [Idx, Opt])
    end, lists:zip(lists:seq(1, length(Options)), Options)).

%% @doc 显示参数列表
-spec display_args(map()) -> ok.
display_args(Args) when map_size(Args) =:= 0 -> ok;
display_args(Args) ->
    io:format("参数:~n"),
    maps:foreach(fun(K, V) ->
        io:format("  ~ts: ~ts~n", [K, format_value(V)])
    end, Args).

%%====================================================================
%% 输入函数
%%====================================================================

%% @doc 读取用户输入
%%
%% 统一的输入读取函数，处理错误情况。
-spec read_input() -> {ok, binary()} | {error, term()}.
read_input() ->
    io:format("~n"),
    case io:get_line("> ") of
        {error, Reason} -> {error, Reason};
        eof -> {error, eof};
        Line -> {ok, string:trim(Line)}
    end.

%% @doc 读取 yes/no 输入
%%
%% 专门用于确认场景的输入读取。
-spec read_yes_no() -> {ok, boolean()} | {error, term()}.
read_yes_no() ->
    io:format("~n"),
    case io:get_line("确认执行? (yes/no): ") of
        {error, _} -> {ok, false};
        eof -> {ok, false};
        Line ->
            Answer = string:trim(string:lowercase(Line)),
            {ok, Answer =:= "yes" orelse Answer =:= "y"}
    end.

%% @doc 读取批准输入
%%
%% 专门用于审批场景的输入读取。
-spec read_approval() -> {ok, boolean()} | {error, term()}.
read_approval() ->
    io:format("~n"),
    case io:get_line("批准执行? (yes/no): ") of
        {error, _} -> {ok, false};
        eof -> {ok, false};
        Line ->
            Answer = string:trim(string:lowercase(Line)),
            {ok, Answer =:= "yes" orelse Answer =:= "y"}
    end.

%%====================================================================
%% 私有函数
%%====================================================================

%% @private 显示边框
-spec display_box(binary()) -> ok.
display_box(Title) ->
    Border = binary:copy(<<"═"/utf8>>, 40),
    io:format("~n~ts~n", [<<"╔"/utf8, Border/binary, "╗"/utf8>>]),
    io:format("~ts  ~ts  ~ts~n", [<<"║"/utf8>>, Title, <<"║"/utf8>>]),
    io:format("~ts~n", [<<"╚"/utf8, Border/binary, "╝"/utf8>>]).

%% @private 条件显示上下文
-spec maybe_display_context(binary()) -> ok.
maybe_display_context(<<>>) -> ok;
maybe_display_context(Context) ->
    io:format("~n上下文: ~ts~n", [Context]).

%% @private 条件显示后果
-spec maybe_display_consequences(binary()) -> ok.
maybe_display_consequences(<<>>) -> ok;
maybe_display_consequences(Consequences) ->
    io:format("后果: ~ts~n", [Consequences]).

%% @private 格式化值用于显示
-spec format_value(term()) -> binary().
format_value(V) when is_binary(V) -> V;
format_value(V) when is_list(V) -> list_to_binary(V);
format_value(V) when is_atom(V) -> atom_to_binary(V);
format_value(V) when is_integer(V) -> integer_to_binary(V);
format_value(V) -> iolist_to_binary(io_lib:format("~p", [V])).
