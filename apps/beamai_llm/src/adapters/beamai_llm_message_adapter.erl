%%%-------------------------------------------------------------------
%%% @doc LLM 消息适配器
%%%
%%% 提供统一的消息格式转换，支持不同 LLM Provider 的消息格式。
%%%
%%% == Atom 安全策略 ==
%%%
%%% 为避免 atom 泄漏，使用 binary_to_existing_atom + 白名单策略：
%%% 1. 只接受预定义的角色类型
%%% 2. 使用 binary_to_existing_atom 尝试转换
%%% 3. 如果 atom 不存在，返回默认值
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_message_adapter).

%% API
-export([to_openai/1, from_openai/1]).
-export([to_anthropic/1, from_anthropic/1]).
-export([to_provider/2, from_provider/2]).

%% 类型
-type role_atom() :: user | assistant | system | tool.
-type role() :: role_atom() | binary().  %% 支持 atom 和 binary
-type message() :: #{
    role := role(),
    content := binary() | null,
    name => binary(),
    tool_call_id => binary(),
    tool_calls => [map()],
    prefix => boolean()    %% DeepSeek Chat Prefix Completion（beta）
}.

-export_type([role_atom/0, role/0, message/0]).

%% 预定义的角色类型（白名单）
-define(ALLOWED_ROLES, [<<"user">>, <<"assistant">>, <<"system">>, <<"tool">>]).

%%====================================================================
%%% 统一转换接口
%%====================================================================

%% @doc 转换为指定 Provider 格式
-spec to_provider([message()], atom()) -> [map()].
to_provider(Messages, openai) -> to_openai(Messages);
to_provider(Messages, anthropic) -> to_anthropic(Messages);
to_provider(Messages, ollama) -> to_openai(Messages);  %% Ollama 使用 OpenAI 格式
to_provider(Messages, zhipu) -> to_openai(Messages);   %% 智谱使用 OpenAI 兼容格式
to_provider(Messages, _) -> to_openai(Messages).

%% @doc 从指定 Provider 格式转换
-spec from_provider([map()], atom()) -> [message()].
from_provider(Messages, openai) -> from_openai(Messages);
from_provider(Messages, anthropic) -> from_anthropic(Messages);
from_provider(Messages, ollama) -> from_openai(Messages);
from_provider(Messages, zhipu) -> from_openai(Messages);  %% 智谱使用 OpenAI 兼容格式
from_provider(Messages, _) -> from_openai(Messages).

%%====================================================================
%%% OpenAI 格式转换
%%====================================================================

%% @doc 转换为 OpenAI 消息格式
-spec to_openai([message()]) -> [map()].
to_openai(Messages) ->
    [to_openai_message(M) || M <- Messages].

to_openai_message(#{role := Role, content := Content} = Msg) ->
    Base = #{<<"role">> => to_binary_role(Role), <<"content">> => Content},
    Base1 = maybe_add_field(Base, <<"name">>, Msg, name),
    Base2 = maybe_add_field(Base1, <<"tool_call_id">>, Msg, tool_call_id),
    Base3 = maybe_add_prefix(Base2, Msg),
    maybe_add_tool_calls_openai(Base3, Msg).

%% @private 透传 prefix 标志（DeepSeek Chat Prefix Completion，beta）
%% 末尾 assistant 消息标记 prefix => true 表示强制模型从该内容续写。
%% 仅在显式设置为 true 时透传，其他 Provider 的消息不受影响。
maybe_add_prefix(Base, #{prefix := true}) -> Base#{<<"prefix">> => true};
maybe_add_prefix(Base, _) -> Base.

maybe_add_tool_calls_openai(Base, #{tool_calls := Calls}) when is_list(Calls), Calls =/= [] ->
    Base#{<<"tool_calls">> => [format_tool_call_openai(C) || C <- Calls]};
maybe_add_tool_calls_openai(Base, _) ->
    Base.

format_tool_call_openai(#{id := Id, function := #{name := Name, arguments := Args}}) ->
    #{
        <<"id">> => Id,
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => Name,
            <<"arguments">> => beamai_utils:to_binary(Args)
        }
    };
format_tool_call_openai(#{id := Id, name := Name, arguments := Args}) ->
    #{
        <<"id">> => Id,
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => Name,
            <<"arguments">> => beamai_utils:to_binary(Args)
        }
    }.

%% @doc 从 OpenAI 消息格式转换
-spec from_openai([map()]) -> [message()].
from_openai(Messages) ->
    [from_openai_message(M) || M <- Messages].

from_openai_message(#{<<"role">> := RoleBin} = Msg) ->
    Role = safe_binary_to_role(RoleBin),
    Base = #{role => Role, content => maps:get(<<"content">>, Msg, null)},
    Base1 = maybe_parse_field(Base, name, Msg, <<"name">>),
    Base2 = maybe_parse_field(Base1, tool_call_id, Msg, <<"tool_call_id">>),
    Base3 = maybe_parse_field(Base2, prefix, Msg, <<"prefix">>),
    maybe_parse_tool_calls_openai(Base3, Msg).

maybe_parse_tool_calls_openai(Base, #{<<"tool_calls">> := Calls}) when is_list(Calls) ->
    Base#{tool_calls => [parse_tool_call_openai(C) || C <- Calls]};
maybe_parse_tool_calls_openai(Base, _) ->
    Base.

parse_tool_call_openai(#{<<"id">> := Id, <<"function">> := Func}) ->
    #{
        id => Id,
        type => function,
        function => #{
            name => maps:get(<<"name">>, Func, <<>>),
            arguments => maps:get(<<"arguments">>, Func, <<>>)
        }
    };
parse_tool_call_openai(_) ->
    #{id => <<>>, type => function, function => #{name => <<>>, arguments => <<>>}}.

%%====================================================================
%%% Anthropic 格式转换
%%====================================================================

%% @doc 转换为 Anthropic 消息格式
-spec to_anthropic([message()]) -> [map()].
to_anthropic(Messages) ->
    [to_anthropic_message(M) || M <- Messages].

to_anthropic_message(#{role := tool, content := Content, tool_call_id := Id}) ->
    %% Anthropic 工具结果格式
    #{
        <<"role">> => <<"user">>,
        <<"content">> => [#{
            <<"type">> => <<"tool_result">>,
            <<"tool_use_id">> => Id,
            <<"content">> => Content
        }]
    };
to_anthropic_message(#{role := assistant, tool_calls := Calls} = Msg) when is_list(Calls), Calls =/= [] ->
    %% 包含工具调用的助手消息（可能也有 thinking blocks）
    ThinkingBlocks = format_thinking_blocks(Msg),
    #{
        <<"role">> => <<"assistant">>,
        <<"content">> => ThinkingBlocks ++ [format_tool_use_anthropic(C) || C <- Calls]
    };
to_anthropic_message(#{role := Role, content := Content} = Msg) ->
    case maps:get(content_blocks, Msg, []) of
        Blocks when is_list(Blocks), Blocks =/= [] ->
            %% 有 content_blocks 时保留完整结构（含 thinking 块）
            HasThinking = lists:any(fun(#{type := T}) -> T =:= thinking orelse T =:= redacted_thinking; (_) -> false end, Blocks),
            case HasThinking of
                true ->
                    #{<<"role">> => format_role_anthropic(Role),
                      <<"content">> => [format_content_block_anthropic(B) || B <- Blocks]};
                false ->
                    #{<<"role">> => format_role_anthropic(Role), <<"content">> => Content}
            end;
        _ ->
            #{<<"role">> => format_role_anthropic(Role), <<"content">> => Content}
    end.

format_role_anthropic(user) -> <<"user">>;
format_role_anthropic(assistant) -> <<"assistant">>;
format_role_anthropic(_) -> <<"user">>.

%% @private 格式化 content_block 为 Anthropic API 格式
format_content_block_anthropic(#{type := text, text := T}) ->
    #{<<"type">> => <<"text">>, <<"text">> => T};
format_content_block_anthropic(#{type := thinking, thinking := T, signature := Sig}) ->
    #{<<"type">> => <<"thinking">>, <<"thinking">> => T, <<"signature">> => Sig};
format_content_block_anthropic(#{type := redacted_thinking, data := Data}) ->
    #{<<"type">> => <<"redacted_thinking">>, <<"data">> => Data};
format_content_block_anthropic(#{type := tool_use, id := Id, name := Name, input := Input}) ->
    #{<<"type">> => <<"tool_use">>, <<"id">> => Id, <<"name">> => Name, <<"input">> => Input};
format_content_block_anthropic(_) ->
    #{<<"type">> => <<"text">>, <<"text">> => <<>>}.

%% @private 从消息中提取 thinking blocks 并格式化
format_thinking_blocks(#{content_blocks := Blocks}) when is_list(Blocks) ->
    [format_content_block_anthropic(B) || B = #{type := T} <- Blocks,
     T =:= thinking orelse T =:= redacted_thinking];
format_thinking_blocks(_) ->
    [].

format_tool_use_anthropic(#{id := Id, name := Name, arguments := Args}) ->
    #{
        <<"type">> => <<"tool_use">>,
        <<"id">> => Id,
        <<"name">> => Name,
        <<"input">> => ensure_map(Args)
    };
format_tool_use_anthropic(#{id := Id, function := #{name := Name, arguments := Args}}) ->
    #{
        <<"type">> => <<"tool_use">>,
        <<"id">> => Id,
        <<"name">> => Name,
        <<"input">> => ensure_map(Args)
    }.

%% @doc 从 Anthropic 消息格式转换
-spec from_anthropic([map()]) -> [message()].
from_anthropic(Messages) ->
    lists:flatten([from_anthropic_message(M) || M <- Messages]).

from_anthropic_message(#{<<"role">> := <<"user">>, <<"content">> := Content}) when is_list(Content) ->
    %% 检查是否包含工具结果
    case extract_tool_results(Content) of
        [] -> [#{role => user, content => extract_text(Content)}];
        ToolResults -> ToolResults
    end;
from_anthropic_message(#{<<"role">> := <<"assistant">>, <<"content">> := Content}) when is_list(Content) ->
    %% 检查是否包含工具使用和 thinking blocks
    ToolCalls = extract_tool_uses(Content),
    ContentBlocks = extract_content_blocks(Content),
    Text = extract_text(Content),
    Base = #{role => assistant, content => Text},
    Base1 = case ToolCalls of
        [] -> Base;
        _ -> Base#{tool_calls => ToolCalls}
    end,
    HasThinking = lists:any(fun(#{type := T}) -> T =:= thinking orelse T =:= redacted_thinking; (_) -> false end, ContentBlocks),
    Base2 = case HasThinking of
        true -> Base1#{content_blocks => ContentBlocks};
        false -> Base1
    end,
    [Base2];
from_anthropic_message(#{<<"role">> := RoleBin, <<"content">> := Content}) ->
    Role = safe_binary_to_role(RoleBin),
    [#{role => Role, content => Content}].

extract_tool_results(Content) ->
    [#{role => tool, content => maps:get(<<"content">>, C, <<>>),
       tool_call_id => maps:get(<<"tool_use_id">>, C, <<>>)}
     || C <- Content, maps:get(<<"type">>, C, <<>>) =:= <<"tool_result">>].

extract_tool_uses(Content) ->
    [#{id => maps:get(<<"id">>, C, <<>>),
       type => function,
       function => #{
           name => maps:get(<<"name">>, C, <<>>),
           arguments => jsx:encode(maps:get(<<"input">>, C, #{}))
       }}
     || C <- Content, maps:get(<<"type">>, C, <<>>) =:= <<"tool_use">>].

extract_content_blocks(Content) when is_list(Content) ->
    lists:filtermap(fun
        (#{<<"type">> := <<"text">>, <<"text">> := T}) ->
            {true, #{type => text, text => T}};
        (#{<<"type">> := <<"thinking">>, <<"thinking">> := T, <<"signature">> := Sig}) ->
            {true, #{type => thinking, thinking => T, signature => Sig}};
        (#{<<"type">> := <<"redacted_thinking">>, <<"data">> := Data}) ->
            {true, #{type => redacted_thinking, data => Data}};
        (#{<<"type">> := <<"tool_use">>, <<"id">> := Id, <<"name">> := Name, <<"input">> := Input}) ->
            {true, #{type => tool_use, id => Id, name => Name, input => Input}};
        (_) -> false
    end, Content).

extract_text(Content) when is_list(Content) ->
    Texts = [maps:get(<<"text">>, C, <<>>) || C <- Content,
             maps:get(<<"type">>, C, <<>>) =:= <<"text">>],
    iolist_to_binary(Texts).

%%====================================================================
%%% 内部函数
%%====================================================================

maybe_add_field(Base, Key, Source, Field) ->
    case maps:get(Field, Source, undefined) of
        undefined -> Base;
        Value -> Base#{Key => Value}
    end.

maybe_parse_field(Base, Key, Source, Field) ->
    case maps:get(Field, Source, undefined) of
        undefined -> Base;
        Value -> Base#{Key => Value}
    end.

ensure_map(Args) -> beamai_utils:parse_json(Args).

%%--------------------------------------------------------------------
%% @doc 安全地将 binary 转换为 role atom
%%
%% 使用白名单策略，只接受预定义的角色类型：
%% - 尝试使用 binary_to_existing_atom
%% - 如果不在白名单中，返回默认值 user
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

%% @private 将 role 转换为 binary，兼容 atom 和 binary 输入
-spec to_binary_role(atom() | binary()) -> binary().
to_binary_role(Role) when is_atom(Role) -> atom_to_binary(Role);
to_binary_role(Role) when is_binary(Role) -> Role.
