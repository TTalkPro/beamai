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
-export([to_openai_responses/1]).
-export([to_anthropic/1, from_anthropic/1]).
-export([to_provider/2, from_provider/2]).
-export([extract_system_prompt/1]).

%% 类型
-type role_atom() :: user | assistant | system | tool.
-type role() :: role_atom() | binary().  %% 支持 atom 和 binary

%% 多模态内容部件（content 为列表时的元素），构造函数见 beamai_llm_content
%% - text:     #{type => text, text => binary()}
%% - image:    #{type => image, source => media_source(), detail => binary()}
%% - audio:    #{type => audio, source => media_source(), format => binary()}
%%             （兼容旧形态 #{type => audio, data => binary(), format => binary()}）
%% - video:    #{type => video, source => media_source()}（Qwen-VL / GLM-4V 等）
%% - document: #{type => document, source => media_source(),
%%               filename => binary(), title => binary(), context => binary(),
%%               citations => boolean()}
%%
%% 任意部件均可带 cache_control（Anthropic prompt 缓存断点），
%% 不支持该能力的 Provider 会忽略。
-type media_source() :: beamai_llm_media:media_source().
-type content_part() :: #{type := atom(), _ => _}.
-type content() :: binary() | null | [content_part()].
-type message() :: #{
    role := role(),
    content := content(),
    name => binary(),
    tool_call_id => binary(),
    tool_calls => [map()],
    prefix => boolean(),   %% DeepSeek Chat Prefix Completion（beta）
    partial => boolean()   %% Moonshot / Kimi 部分模式续写
}.

-export_type([role_atom/0, role/0, message/0, content/0, content_part/0, media_source/0]).

%% 预定义的角色类型（白名单）
-define(ALLOWED_ROLES, [<<"user">>, <<"assistant">>, <<"system">>, <<"tool">>]).

%%====================================================================
%%% 统一转换接口
%%====================================================================

%% @doc 转换为指定 Provider 格式
-spec to_provider([message()], atom()) -> [map()].
to_provider(Messages, anthropic) -> to_anthropic(Messages);
%% 其余 Provider 均为 OpenAI 兼容格式（openai / ollama / zhipu / deepseek /
%% dashscope / xai / moonshot / kimi / openrouter / siliconflow ...）
to_provider(Messages, _) -> to_openai(Messages).

%% @doc 提取系统提示（Anthropic 风格 API 需要单独的 system 字段）
%%
%% 取首条 system 消息的 content 作为系统提示，其余消息原样返回；
%% 无 system 消息时返回 {undefined, Messages}。
-spec extract_system_prompt([message()]) -> {term() | undefined, [message()]}.
extract_system_prompt(Messages) ->
    case lists:partition(fun(#{role := R}) -> R =:= system end, Messages) of
        {[], Rest} -> {undefined, Rest};
        {[#{content := C} | _], Rest} -> {C, Rest}
    end.

%% @doc 从指定 Provider 格式转换
-spec from_provider([map()], atom()) -> [message()].
from_provider(Messages, anthropic) -> from_anthropic(Messages);
%% 其余 Provider 均为 OpenAI 兼容格式
from_provider(Messages, _) -> from_openai(Messages).

%%====================================================================
%%% OpenAI 格式转换
%%====================================================================

%% @doc 转换为 OpenAI 消息格式
-spec to_openai([message()]) -> [map()].
to_openai(Messages) ->
    [to_openai_message(M) || M <- Messages].

to_openai_message(#{role := Role, content := Content} = Msg) ->
    Base = #{<<"role">> => to_binary_role(Role), <<"content">> => format_content_openai(Content)},
    Base1 = maybe_add_field(Base, <<"name">>, Msg, name),
    Base2 = maybe_add_field(Base1, <<"tool_call_id">>, Msg, tool_call_id),
    Base3 = maybe_add_prefix(Base2, Msg),
    maybe_add_tool_calls_openai(Base3, Msg).

%% @private 透传续写标志
%% - prefix（DeepSeek Chat Prefix Completion，beta）
%% - partial（Moonshot / Kimi 部分模式）
%% 末尾 assistant 消息标记后表示强制模型从该内容续写。
%% 仅在显式设置为 true 时透传，其他 Provider 的消息不受影响。
maybe_add_prefix(Base, Msg) ->
    Base1 = case maps:get(prefix, Msg, undefined) of
        true -> Base#{<<"prefix">> => true};
        _ -> Base
    end,
    case maps:get(partial, Msg, undefined) of
        true -> Base1#{<<"partial">> => true};
        _ -> Base1
    end.

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

%% @private 格式化消息内容为 OpenAI 格式
%% 纯文本（binary / null）原样透传；列表视为多模态内容部件，逐个转换。
format_content_openai(Content) when is_binary(Content); Content =:= null -> Content;
format_content_openai(Parts) when is_list(Parts) -> [format_part_openai(P) || P <- Parts];
format_content_openai(Other) -> Other.

%% @private 转换单个多模态部件为 OpenAI 格式
format_part_openai(#{type := text, text := T}) ->
    #{<<"type">> => <<"text">>, <<"text">> => T};
format_part_openai(#{type := image, source := #{type := file_id, file_id := Id}}) ->
    %% 已上传到 Files API 的图片按 file 部件引用
    #{<<"type">> => <<"file">>, <<"file">> => #{<<"file_id">> => Id}};
format_part_openai(#{type := image, source := Src} = Part) ->
    ImageUrl0 = #{<<"url">> => media_url_openai(Src)},
    ImageUrl = case maps:get(detail, Part, undefined) of
        undefined -> ImageUrl0;
        Detail -> ImageUrl0#{<<"detail">> => Detail}
    end,
    #{<<"type">> => <<"image_url">>, <<"image_url">> => ImageUrl};
format_part_openai(#{type := audio, data := Data, format := Fmt}) ->
    %% 兼容旧形态：直接给出 base64 数据与格式
    #{<<"type">> => <<"input_audio">>,
      <<"input_audio">> => #{<<"data">> => Data, <<"format">> => Fmt}};
format_part_openai(#{type := audio, source := #{type := base64, data := Data} = Src} = Part) ->
    Fmt = audio_format_of(Part, Src),
    #{<<"type">> => <<"input_audio">>,
      <<"input_audio">> => #{<<"data">> => Data, <<"format">> => Fmt}};
format_part_openai(#{type := audio, source := #{type := url, url := Url} = Src} = Part) ->
    %% Qwen-Omni 等兼容实现允许 input_audio.data 直接给音频 URL
    #{<<"type">> => <<"input_audio">>,
      <<"input_audio">> => #{<<"data">> => Url, <<"format">> => audio_format_of(Part, Src)}};
format_part_openai(#{type := audio, source := #{type := file_id, file_id := Id}}) ->
    #{<<"type">> => <<"file">>, <<"file">> => #{<<"file_id">> => Id}};
format_part_openai(#{type := video, source := #{type := file_id, file_id := Id}}) ->
    #{<<"type">> => <<"file">>, <<"file">> => #{<<"file_id">> => Id}};
format_part_openai(#{type := video, source := Src}) ->
    %% Qwen-VL / GLM-4V 等 OpenAI 兼容 VL 模型的视频输入约定
    #{<<"type">> => <<"video_url">>, <<"video_url">> => #{<<"url">> => media_url_openai(Src)}};
format_part_openai(#{type := document, source := #{type := file_id, file_id := Id}}) ->
    #{<<"type">> => <<"file">>, <<"file">> => #{<<"file_id">> => Id}};
format_part_openai(#{type := document, source := Src} = Part) ->
    File0 = #{<<"file_data">> => media_url_openai(Src)},
    File1 = case maps:get(filename, Part, undefined) of
        undefined -> File0;
        Name -> File0#{<<"filename">> => Name}
    end,
    #{<<"type">> => <<"file">>, <<"file">> => File1};
format_part_openai(#{<<"type">> := _} = Already) ->
    %% 已是 API 格式（binary key），原样透传
    Already;
format_part_openai(_) ->
    #{<<"type">> => <<"text">>, <<"text">> => <<>>}.

%% @private 取音频格式：部件显式声明优先，否则由 MIME 类型推断
audio_format_of(Part, Src) ->
    case maps:get(format, Part, undefined) of
        undefined ->
            case beamai_llm_media:audio_format(beamai_llm_media:media_type(Src)) of
                undefined -> <<"wav">>;
                Fmt -> Fmt
            end;
        Fmt -> Fmt
    end.

%% @private 构建 OpenAI 媒体 URL（base64 转 data URI，或直接 url）
media_url_openai(#{type := base64, media_type := MT, data := Data}) ->
    <<"data:", MT/binary, ";base64,", Data/binary>>;
media_url_openai(#{type := url, url := U}) ->
    U.

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
    Base4 = maybe_parse_field(Base3, partial, Msg, <<"partial">>),
    maybe_parse_tool_calls_openai(Base4, Msg).

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
    %% 包含工具调用的助手消息（可能也有 thinking blocks 和文本）
    ThinkingBlocks = format_thinking_blocks(Msg),
    TextBlocks = text_blocks_anthropic(maps:get(content, Msg, null)),
    #{
        <<"role">> => <<"assistant">>,
        <<"content">> => ThinkingBlocks ++ TextBlocks ++ [format_tool_use_anthropic(C) || C <- Calls]
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
                    #{<<"role">> => format_role_anthropic(Role),
                      <<"content">> => format_content_anthropic(Content)}
            end;
        _ ->
            #{<<"role">> => format_role_anthropic(Role),
              <<"content">> => format_content_anthropic(Content)}
    end.

%% @private 格式化消息内容为 Anthropic 格式
%% 纯文本（binary / null）原样透传；列表视为多模态内容部件，逐个转换。
%% Anthropic 不支持音频输入，audio 部件会被丢弃。
format_content_anthropic(Content) when is_binary(Content); Content =:= null -> Content;
format_content_anthropic(Parts) when is_list(Parts) ->
    lists:filtermap(fun(P) ->
        case format_part_anthropic(P) of
            skip -> false;
            Block -> {true, Block}
        end
    end, Parts);
format_content_anthropic(Other) -> Other.

%% @private 把助手文本转为 Anthropic text 块（tool_calls 消息只携带非空文本）
text_blocks_anthropic(Content) when is_binary(Content), Content =/= <<>> ->
    [#{<<"type">> => <<"text">>, <<"text">> => Content}];
text_blocks_anthropic(_) ->
    [].

%% @private 转换单个多模态部件为 Anthropic 格式
format_part_anthropic(#{type := text, text := T} = Part) ->
    with_cache_control_anthropic(#{<<"type">> => <<"text">>, <<"text">> => T}, Part);
format_part_anthropic(#{type := image, source := Src} = Part) ->
    with_cache_control_anthropic(
        #{<<"type">> => <<"image">>, <<"source">> => media_source_anthropic(Src)}, Part);
format_part_anthropic(#{type := document, source := Src} = Part) ->
    Block0 = #{<<"type">> => <<"document">>,
               <<"source">> => document_source_anthropic(Src)},
    Block1 = case maps:get(citations, Part, false) of
        true -> Block0#{<<"citations">> => #{<<"enabled">> => true}};
        _ -> Block0
    end,
    Block2 = maybe_put_binary(Block1, <<"title">>, maps:get(title, Part, undefined)),
    Block3 = maybe_put_binary(Block2, <<"context">>, maps:get(context, Part, undefined)),
    with_cache_control_anthropic(Block3, Part);
format_part_anthropic(#{type := audio}) ->
    %% Anthropic 暂不支持音频输入，丢弃
    skip;
format_part_anthropic(#{type := video}) ->
    %% Anthropic 暂不支持视频输入，丢弃
    skip;
format_part_anthropic(#{<<"type">> := _} = Already) ->
    %% 已是 API 格式（binary key），原样透传
    Already;
format_part_anthropic(_) ->
    #{<<"type">> => <<"text">>, <<"text">> => <<>>}.

%% @private 在内容块上注入 cache_control（部件显式声明时）
with_cache_control_anthropic(Block, Part) ->
    case maps:get(cache_control, Part, undefined) of
        undefined -> Block;
        CC -> Block#{<<"cache_control">> => CC}
    end.

%% @private 可选字段写入（undefined 时不写）
maybe_put_binary(Block, _Key, undefined) -> Block;
maybe_put_binary(Block, Key, Value) -> Block#{Key => Value}.

%% @private 构建 Anthropic 媒体 source（base64 / url / file_id）
media_source_anthropic(#{type := base64, media_type := MT, data := Data}) ->
    #{<<"type">> => <<"base64">>, <<"media_type">> => MT, <<"data">> => Data};
media_source_anthropic(#{type := url, url := U}) ->
    #{<<"type">> => <<"url">>, <<"url">> => U};
media_source_anthropic(#{type := file_id, file_id := Id}) ->
    #{<<"type">> => <<"file">>, <<"file_id">> => Id}.

%% @private 构建 Anthropic 文档 source
%% 纯文本文档（text/plain）走 text 源，Claude 直接按文本处理并支持引用。
document_source_anthropic(#{type := base64, media_type := <<"text/plain">>, data := Data}) ->
    #{<<"type">> => <<"text">>, <<"media_type">> => <<"text/plain">>,
      <<"data">> => decode_base64_text(Data)};
document_source_anthropic(Src) ->
    media_source_anthropic(Src).

%% @private base64 文本解码失败时原样返回（已是明文）
decode_base64_text(Data) ->
    try base64:decode(Data)
    catch _:_ -> Data
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
           arguments => beamai_utils:encode_json(maps:get(<<"input">>, C, #{}))
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

%%====================================================================
%%% OpenAI Responses API 格式转换
%%====================================================================

%% @doc 转换为 OpenAI Responses API 的输入（`instructions` + `input` 项列表）
%%
%% Responses API 没有 system 消息：所有 system 消息的文本合并成顶层 `instructions`
%% （多条以空行拼接），其余消息逐条展开为输入项：
%%   - user      → `{type: message, role: user, content}'（文本或 input_text /
%%                 input_image / input_file / input_audio 部件）
%%   - assistant → content_blocks 里的 reasoning 项（原样带 id / encrypted_content
%%                 回传，多轮工具调用保留推理上下文）+ 文本 message 项
%%                 （output_text）+ 每个 tool_call 一个 function_call 项
%%   - tool      → `{type: function_call_output, call_id, output}'
%%
%% 返回 `{Instructions | undefined, Items}'。
-spec to_openai_responses([message()]) -> {binary() | undefined, [map()]}.
to_openai_responses(Messages) ->
    {Systems, Rest} = lists:partition(
        fun(#{role := R}) -> to_binary_role(R) =:= <<"system">>; (_) -> false end, Messages),
    Instructions = case [text_of_content(maps:get(content, M, null)) || M <- Systems] of
        [] -> undefined;
        Texts -> iolist_to_binary(lists:join(<<"\n\n">>, [T || T <- Texts, T =/= <<>>]))
    end,
    {Instructions, lists:append([to_responses_items(M) || M <- Rest])}.

%% @private 单条消息 → 零或多个输入项
to_responses_items(#{role := Role} = Msg) ->
    case to_binary_role(Role) of
        <<"tool">> -> [function_call_output_item(Msg)];
        <<"assistant">> -> assistant_items(Msg);
        _ -> [user_message_item(Msg)]
    end.

user_message_item(#{content := Content}) ->
    #{<<"type">> => <<"message">>, <<"role">> => <<"user">>,
      <<"content">> => responses_input_content(Content)}.

%% @private 助手消息：reasoning 块 → 文本 message → function_call 项
assistant_items(Msg) ->
    Reasoning = [reasoning_input_item(B) || #{type := reasoning} = B <- maps:get(content_blocks, Msg, [])],
    Text = text_of_content(maps:get(content, Msg, null)),
    MessageItems = case Text of
        <<>> -> [];
        _ -> [#{<<"type">> => <<"message">>, <<"role">> => <<"assistant">>,
                <<"content">> => [#{<<"type">> => <<"output_text">>, <<"text">> => Text}]}]
    end,
    Calls = [function_call_item(C) || C <- maps:get(tool_calls, Msg, [])],
    Reasoning ++ MessageItems ++ Calls.

%% @private 推理项回传：有 encrypted_content 的（store=false 模式）必须原样带回；
%% 否则仅凭 id（服务端已存储）。文本推理内容一并带上，兼容把 reasoning 映射成
%% reasoning_content 的 OpenAI 兼容网关。
reasoning_input_item(#{id := Id} = Block) ->
    Base = #{<<"type">> => <<"reasoning">>, <<"id">> => Id,
             <<"summary">> => [#{<<"type">> => <<"summary_text">>, <<"text">> => S}
                               || S <- maps:get(summary, Block, []), is_binary(S)]},
    Base1 = case maps:get(encrypted_content, Block, null) of
        EC when is_binary(EC) -> Base#{<<"encrypted_content">> => EC};
        _ -> Base
    end,
    case maps:get(text, Block, <<>>) of
        T when is_binary(T), T =/= <<>> ->
            Base1#{<<"content">> => [#{<<"type">> => <<"reasoning_text">>, <<"text">> => T}]};
        _ -> Base1
    end.

function_call_item(#{id := Id, function := #{name := Name, arguments := Args}}) ->
    #{<<"type">> => <<"function_call">>, <<"call_id">> => Id,
      <<"name">> => Name, <<"arguments">> => arguments_to_binary(Args)};
function_call_item(#{id := Id, name := Name, arguments := Args}) ->
    #{<<"type">> => <<"function_call">>, <<"call_id">> => Id,
      <<"name">> => Name, <<"arguments">> => arguments_to_binary(Args)}.

%% @private tool_calls 的 arguments 可能是 map（统一格式）或 JSON 文本
arguments_to_binary(Args) when is_map(Args) -> beamai_utils:encode_json(Args);
arguments_to_binary(Args) when is_binary(Args) -> Args;
arguments_to_binary(Args) -> beamai_utils:to_binary(Args).

function_call_output_item(#{tool_call_id := Id} = Msg) ->
    #{<<"type">> => <<"function_call_output">>, <<"call_id">> => Id,
      <<"output">> => text_of_content(maps:get(content, Msg, null))};
function_call_output_item(Msg) ->
    function_call_output_item(Msg#{tool_call_id => <<>>}).

%% @private 用户输入内容：纯文本原样；多模态部件逐个转换
responses_input_content(Content) when is_binary(Content) -> Content;
responses_input_content(null) -> <<>>;
responses_input_content(Parts) when is_list(Parts) -> [responses_input_part(P) || P <- Parts];
responses_input_content(Other) -> Other.

responses_input_part(#{type := text, text := T}) ->
    #{<<"type">> => <<"input_text">>, <<"text">> => T};
responses_input_part(#{type := image, source := #{type := file_id, file_id := Id}} = Part) ->
    maybe_detail(#{<<"type">> => <<"input_image">>, <<"file_id">> => Id}, Part);
responses_input_part(#{type := image, source := Src} = Part) ->
    maybe_detail(#{<<"type">> => <<"input_image">>, <<"image_url">> => media_url_openai(Src)}, Part);
responses_input_part(#{type := audio, data := Data, format := Fmt}) ->
    #{<<"type">> => <<"input_audio">>, <<"input_audio">> => #{<<"data">> => Data, <<"format">> => Fmt}};
responses_input_part(#{type := audio, source := #{type := base64, data := Data} = Src} = Part) ->
    #{<<"type">> => <<"input_audio">>,
      <<"input_audio">> => #{<<"data">> => Data, <<"format">> => audio_format_of(Part, Src)}};
responses_input_part(#{type := Type, source := #{type := file_id, file_id := Id}})
  when Type =:= document; Type =:= video; Type =:= audio ->
    #{<<"type">> => <<"input_file">>, <<"file_id">> => Id};
responses_input_part(#{type := Type, source := #{type := url, url := Url}})
  when Type =:= document; Type =:= video; Type =:= audio ->
    #{<<"type">> => <<"input_file">>, <<"file_url">> => Url};
responses_input_part(#{type := Type, source := #{type := base64} = Src} = Part)
  when Type =:= document; Type =:= video ->
    File = #{<<"type">> => <<"input_file">>, <<"file_data">> => media_url_openai(Src)},
    case maps:get(filename, Part, undefined) of
        undefined -> File;
        Name -> File#{<<"filename">> => Name}
    end;
responses_input_part(#{<<"type">> := _} = Already) ->
    %% 已是 Responses API 格式（binary key），原样透传
    Already;
responses_input_part(_) ->
    #{<<"type">> => <<"input_text">>, <<"text">> => <<>>}.

maybe_detail(Part, #{detail := Detail}) -> Part#{<<"detail">> => Detail};
maybe_detail(Part, _) -> Part.

%% @private 取内容的纯文本（多模态列表只取文本部件拼接）
text_of_content(Content) when is_binary(Content) -> Content;
text_of_content(null) -> <<>>;
text_of_content(Parts) when is_list(Parts) ->
    iolist_to_binary([T || P <- Parts, is_map(P),
                           T <- [text_part(P)], is_binary(T)]);
text_of_content(Other) -> beamai_utils:to_binary(Other).

text_part(#{type := text, text := T}) -> T;
text_part(#{<<"type">> := <<"text">>, <<"text">> := T}) -> T;
text_part(#{<<"type">> := <<"input_text">>, <<"text">> := T}) -> T;
text_part(#{<<"type">> := <<"output_text">>, <<"text">> := T}) -> T;
text_part(_) -> undefined.
