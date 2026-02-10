%%%-------------------------------------------------------------------
%%% @doc LLM 消息构建工具
%%%
%%% 提供规范化的消息构建、访问和验证功能，
%%% 统一项目中 LLM 消息的创建方式。
%%%
%%% == Builder ==
%%%
%%% ```
%%% Msg = beamai_message:system(<<"你是助手">>),
%%% %% => #{role => system, content => <<"你是助手">>}
%%%
%%% Msgs = beamai_message:messages([
%%%     {system, <<"你是助手">>},
%%%     {user, <<"你好">>}
%%% ]).
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_message).

%% === 构建器（Builder）===
-export([system/1, user/1, assistant/1]).
-export([tool_calls/1, tool_result/3]).

%% === 访问器（Accessor）===
-export([role/1, content/1, get_tool_calls/1, tool_call_id/1, name/1]).

%% === 谓词（Predicate）===
-export([is_message/1, is_role/2]).

%% === 组合工具 ===
-export([messages/1]).

%% Types
-export_type([message/0, tool_call/0]).

-type message() :: #{
    role := user | assistant | system | tool,
    content := binary() | null,
    tool_calls => [tool_call()],
    tool_call_id => binary(),
    name => binary()
}.

-type tool_call() :: #{
    id := binary(),
    type := function,
    function := #{
        name := binary(),
        arguments := binary() | map()
    }
}.

%%====================================================================
%% Builder
%%====================================================================

%% @doc 构建 system 消息
-spec system(term()) -> message().
system(Content) ->
    #{role => system, content => to_bin(Content)}.

%% @doc 构建 user 消息
-spec user(term()) -> message().
user(Content) ->
    #{role => user, content => to_bin(Content)}.

%% @doc 构建 assistant 消息
%%
%% Content 为 null 时保持 null（tool_calls 场景）。
-spec assistant(term()) -> message().
assistant(null) ->
    #{role => assistant, content => null};
assistant(Content) ->
    #{role => assistant, content => to_bin(Content)}.

%% @doc 构建 assistant tool_calls 消息
-spec tool_calls([tool_call()]) -> message().
tool_calls(TCs) ->
    #{role => assistant, content => null, tool_calls => TCs}.

%% @doc 构建 tool result 消息
-spec tool_result(binary(), binary(), term()) -> message().
tool_result(Id, Name, Content) ->
    #{role => tool, tool_call_id => Id, name => Name, content => to_bin(Content)}.

%%====================================================================
%% Accessor
%%====================================================================

%% @doc 获取消息角色
-spec role(message()) -> atom().
role(#{role := Role}) -> Role.

%% @doc 获取消息内容
-spec content(message()) -> binary() | null.
content(#{content := Content}) -> Content.

%% @doc 获取 tool_calls 列表
-spec get_tool_calls(message()) -> [tool_call()].
get_tool_calls(#{tool_calls := TCs}) -> TCs;
get_tool_calls(_) -> [].

%% @doc 获取 tool_call_id
-spec tool_call_id(message()) -> binary() | undefined.
tool_call_id(#{tool_call_id := Id}) -> Id;
tool_call_id(_) -> undefined.

%% @doc 获取 tool name
-spec name(message()) -> binary() | undefined.
name(#{name := Name}) -> Name;
name(_) -> undefined.

%%====================================================================
%% Predicate
%%====================================================================

%% @doc 判断是否为合法消息
-spec is_message(term()) -> boolean().
is_message(#{role := Role, content := _}) when
    Role =:= system; Role =:= user; Role =:= assistant; Role =:= tool ->
    true;
is_message(_) ->
    false.

%% @doc 判断消息是否为指定角色
-spec is_role(message(), atom()) -> boolean().
is_role(#{role := Role}, Role) -> true;
is_role(_, _) -> false.

%%====================================================================
%% 组合工具
%%====================================================================

%% @doc 从 tuple 列表快速构建消息列表
%%
%% ```
%% beamai_message:messages([
%%     {system, <<"你是助手">>},
%%     {user, Question}
%% ]).
%% '''
-spec messages([{atom(), term()}]) -> [message()].
messages(Pairs) ->
    [build_one(Pair) || Pair <- Pairs].

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 将各种类型转换为 binary，null 保持 null
-spec to_bin(term()) -> binary().
to_bin(V) ->
    beamai_utils:to_binary(V).

%% @private 根据 tuple 构建单条消息
-spec build_one({atom(), term()}) -> message().
build_one({system, Content}) -> system(Content);
build_one({user, Content}) -> user(Content);
build_one({assistant, Content}) -> assistant(Content).
