%%%-------------------------------------------------------------------
%%% @doc 消息处理模块
%%%
%%% 提供消息创建、验证和转换的纯函数。
%%%
%%% 功能：
%%%   - 消息构造（user/assistant/system/tool）
%%%   - 消息访问器（role/content/tool_calls）
%%%   - 消息谓词（has_tool_calls/is_role）
%%%   - 格式转换（format_for_llm/parse_from_llm）
%%%   - 列表操作（filter_by_role/last_of_role）
%%%   - JSON 序列化（to_json/from_json）
%%%
%%% == Atom 安全策略 ==
%%%
%%% 为避免 atom 泄漏，使用 binary_to_existing_atom + 白名单策略：
%%% 1. 只接受预定义的角色类型（user, assistant, system, tool）
%%% 2. 只接受预定义的工具调用类型（function）
%%% 3. 使用 binary_to_existing_atom 尝试转换
%%% 4. 如果 atom 不存在，返回默认值
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_message).

%% 消息构造器
-export([user/1, assistant/1, system/1, tool/2]).
%% 消息访问器
-export([role/1, content/1, tool_calls/1]).
%% 消息谓词
-export([has_tool_calls/1, is_role/2]).
%% 格式转换
-export([format_for_llm/1, parse_from_llm/1, format_tool_calls_from_llm/1]).
%% 列表操作
-export([filter_by_role/2, last_of_role/2]).
%% 常用操作
-export([prepend_system/2, extract_content/1, extract_last_content/1]).
%% JSON 转换
-export([to_json/1, from_json/1]).

%% 类型定义
-type role_atom() :: user | assistant | system | tool.  %% 预定义角色
-type role() :: role_atom() | binary().  %% 支持 atom 和 binary
-type message() :: #{
    role := role(),                    %% 消息角色
    content := binary() | null,        %% 消息内容
    name => binary(),                  %% 可选：发送者名称
    tool_call_id => binary(),          %% 可选：工具调用 ID
    tool_calls => [tool_call()]        %% 可选：工具调用列表
}.
-type tool_call() :: #{
    id := binary(),                    %% 工具调用 ID
    type := function | binary(),       %% 调用类型，支持 atom 和 binary
    function := #{name := binary(), arguments := binary()}  %% 函数信息
}.

-export_type([role_atom/0, role/0, message/0, tool_call/0]).

%% 预定义的角色类型（白名单）
-define(ALLOWED_ROLES, [<<"user">>, <<"assistant">>, <<"system">>, <<"tool">>]).
%% 预定义的工具调用类型（白名单）
-define(ALLOWED_TYPES, [<<"function">>]).

%%====================================================================
%% 消息构造器
%%====================================================================

%% @doc 创建用户消息
-spec user(binary()) -> message().
user(Content) -> #{role => user, content => Content}.

%% @doc 创建助手消息
-spec assistant(binary()) -> message().
assistant(Content) -> #{role => assistant, content => Content}.

%% @doc 创建系统消息
-spec system(binary()) -> message().
system(Content) -> #{role => system, content => Content}.

%% @doc 创建工具响应消息
-spec tool(binary(), binary()) -> message().
tool(ToolCallId, Content) ->
    #{role => tool, content => Content, tool_call_id => ToolCallId}.

%%====================================================================
%% 消息访问器
%%====================================================================

%% @doc 获取消息角色
-spec role(message()) -> role().
role(#{role := Role}) -> Role.

%% @doc 获取消息内容
-spec content(message()) -> binary() | null.
content(#{content := Content}) -> Content;
content(_) -> null.

%% @doc 获取工具调用列表
-spec tool_calls(message()) -> [tool_call()].
tool_calls(#{tool_calls := Calls}) -> Calls;
tool_calls(_) -> [].

%%====================================================================
%% 消息谓词
%%====================================================================

%% @doc 检查消息是否包含工具调用
-spec has_tool_calls(message()) -> boolean().
has_tool_calls(Msg) -> tool_calls(Msg) =/= [].

%% @doc 检查消息是否为指定角色
-spec is_role(message(), role()) -> boolean().
is_role(#{role := R}, R) -> true;
is_role(_, _) -> false.

%%====================================================================
%% 格式转换
%%====================================================================

%%--------------------------------------------------------------------
%% @doc 安全地将 binary 转换为 role
%%
%% 使用白名单策略：
%% - 预定义角色（user/assistant/system/tool）转换为 atom
%% - 未知角色保留为 binary（避免 atom 泄漏）
%%
%% 参数：
%% - RoleBin: 角色二进制字符串
%%
%% 返回：role_atom() | binary()
%%
%% @end
%%--------------------------------------------------------------------
-spec safe_binary_to_role(binary()) -> role().
safe_binary_to_role(RoleBin) when is_binary(RoleBin) ->
    case lists:member(RoleBin, ?ALLOWED_ROLES) of
        true ->
            %% 预定义角色，转换为 atom（安全，这些是编译时已知的）
            binary_to_existing_atom(RoleBin, utf8);
        false ->
            %% 未知角色，保留为 binary（避免 atom 泄漏）
            logger:info("Unknown role ~p, keeping as binary", [RoleBin]),
            RoleBin
    end.

%%--------------------------------------------------------------------
%% @doc 安全地将 binary 转换为 tool_call type
%%
%% 使用白名单策略：
%% - 预定义类型（function）转换为 atom
%% - 未知类型保留为 binary（避免 atom 泄漏）
%%
%% 参数：
%% - TypeBin: 类型二进制字符串
%%
%% 返回：function | binary()
%%
%% @end
%%--------------------------------------------------------------------
-spec safe_binary_to_type(binary()) -> function | binary().
safe_binary_to_type(TypeBin) when is_binary(TypeBin) ->
    case lists:member(TypeBin, ?ALLOWED_TYPES) of
        true ->
            %% 预定义类型，转换为 atom（安全，这些是编译时已知的）
            binary_to_existing_atom(TypeBin, utf8);
        false ->
            %% 未知类型，保留为 binary（避免 atom 泄漏）
            logger:info("Unknown tool call type ~p, keeping as binary", [TypeBin]),
            TypeBin
    end.

%% @doc 将消息格式化为 LLM API 格式
-spec format_for_llm(message()) -> map().
format_for_llm(#{role := Role} = Msg) ->
    Base = #{<<"role">> => atom_to_binary(Role)},
    add_fields(Msg, Base, [content, name, tool_call_id, tool_calls]).

%% @private 递归添加字段到格式化消息
add_fields(_Msg, Acc, []) -> Acc;
add_fields(Msg, Acc, [content | Rest]) ->
    case maps:get(content, Msg, undefined) of
        undefined -> add_fields(Msg, Acc, Rest);
        null -> add_fields(Msg, Acc#{<<"content">> => null}, Rest);
        V -> add_fields(Msg, Acc#{<<"content">> => V}, Rest)
    end;
add_fields(Msg, Acc, [name | Rest]) ->
    case maps:get(name, Msg, undefined) of
        undefined -> add_fields(Msg, Acc, Rest);
        V -> add_fields(Msg, Acc#{<<"name">> => V}, Rest)
    end;
add_fields(Msg, Acc, [tool_call_id | Rest]) ->
    case maps:get(tool_call_id, Msg, undefined) of
        undefined -> add_fields(Msg, Acc, Rest);
        V -> add_fields(Msg, Acc#{<<"tool_call_id">> => V}, Rest)
    end;
add_fields(Msg, Acc, [tool_calls | Rest]) ->
    case maps:get(tool_calls, Msg, undefined) of
        undefined -> add_fields(Msg, Acc, Rest);
        Calls ->
            Formatted = [format_tool_call(C) || C <- Calls],
            add_fields(Msg, Acc#{<<"tool_calls">> => Formatted}, Rest)
    end.

%% @private 格式化单个工具调用
format_tool_call(#{id := Id, type := Type, function := Fun}) ->
    #{
        <<"id">> => Id,
        <<"type">> => atom_to_binary(Type),
        <<"function">> => #{
            <<"name">> => maps:get(name, Fun),
            <<"arguments">> => maps:get(arguments, Fun)
        }
    }.

%% @doc 从 LLM API 响应解析消息
-spec parse_from_llm(map()) -> message().
parse_from_llm(#{<<"role">> := RoleBin} = Raw) ->
    Role = safe_binary_to_role(RoleBin),
    Base = #{role => Role},
    parse_fields(Raw, Base, [content, tool_calls]).

%% @private 递归解析字段
parse_fields(_Raw, Acc, []) -> Acc;
parse_fields(Raw, Acc, [content | Rest]) ->
    case maps:get(<<"content">>, Raw, undefined) of
        undefined -> parse_fields(Raw, Acc, Rest);
        null -> parse_fields(Raw, Acc#{content => null}, Rest);
        V -> parse_fields(Raw, Acc#{content => V}, Rest)
    end;
parse_fields(Raw, Acc, [tool_calls | Rest]) ->
    case maps:get(<<"tool_calls">>, Raw, undefined) of
        undefined -> parse_fields(Raw, Acc, Rest);
        Calls ->
            Parsed = [parse_tool_call(C) || C <- Calls],
            parse_fields(Raw, Acc#{tool_calls => Parsed}, Rest)
    end.

%% @private 解析单个工具调用
parse_tool_call(#{<<"id">> := Id, <<"type">> := Type, <<"function">> := Fun}) ->
    #{
        id => Id,
        type => safe_binary_to_type(Type),
        function => #{
            name => maps:get(<<"name">>, Fun),
            arguments => maps:get(<<"arguments">>, Fun)
        }
    }.

%% @doc 将 LLM 响应的 tool_calls 转换为消息格式
%% 输入：LLM 返回的 tool_calls（atom key 格式）
%% 输出：beamai_message:tool_call() 格式（binary key）
-spec format_tool_calls_from_llm([map()]) -> [map()].
format_tool_calls_from_llm(ToolCalls) when is_list(ToolCalls) ->
    [format_llm_tool_call(TC) || TC <- ToolCalls];
format_tool_calls_from_llm(_) ->
    [].

%% @private 转换单个 LLM 工具调用
format_llm_tool_call(#{id := Id, name := Name, arguments := Args}) ->
    BinaryArgs = ensure_binary_args(Args),
    #{
        <<"id">> => Id,
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => Name,
            <<"arguments">> => BinaryArgs
        }
    };
format_llm_tool_call(_) ->
    #{
        <<"id">> => <<>>,
        <<"type">> => <<"function">>,
        <<"function">> => #{<<"name">> => <<>>, <<"arguments">> => <<>>}
    }.

%% @private 确保参数为二进制
ensure_binary_args(Bin) when is_binary(Bin) -> Bin;
ensure_binary_args(Map) when is_map(Map) -> jsx:encode(Map);
ensure_binary_args(_) -> <<>>.

%%====================================================================
%% 列表操作
%%====================================================================

%% @doc 按角色过滤消息列表
-spec filter_by_role([message()], role()) -> [message()].
filter_by_role(Messages, Role) ->
    [M || M <- Messages, is_role(M, Role)].

%% @doc 获取指定角色的最后一条消息
-spec last_of_role([message()], role()) -> message() | undefined.
last_of_role(Messages, Role) ->
    case lists:reverse(filter_by_role(Messages, Role)) of
        [Last | _] -> Last;
        [] -> undefined
    end.

%%====================================================================
%% 常用操作
%%====================================================================

%% @doc 在消息列表前添加系统提示
%% 如果系统提示为空，返回原消息列表
-spec prepend_system(binary(), [message()]) -> [message()].
prepend_system(<<>>, Messages) ->
    Messages;
prepend_system(undefined, Messages) ->
    Messages;
prepend_system(SystemPrompt, Messages) ->
    [system(SystemPrompt) | Messages].

%% @doc 提取消息内容
%% 支持 atom key 和 binary key 两种格式
-spec extract_content(message() | map()) -> binary().
extract_content(#{content := Content}) when is_binary(Content) ->
    Content;
extract_content(#{<<"content">> := Content}) when is_binary(Content) ->
    Content;
extract_content(_) ->
    <<>>.

%% @doc 提取最后一条助手消息的内容
-spec extract_last_content([message()]) -> binary().
extract_last_content(Messages) ->
    case last_of_role(Messages, assistant) of
        undefined -> <<>>;
        Msg -> extract_content(Msg)
    end.

%%====================================================================
%% JSON 转换
%%====================================================================

%% @doc 将消息序列化为 JSON
-spec to_json(message()) -> binary().
to_json(Msg) -> jsx:encode(format_for_llm(Msg)).

%% @doc 从 JSON 反序列化消息
-spec from_json(binary()) -> message().
from_json(Json) -> parse_from_llm(jsx:decode(Json, [return_maps])).
