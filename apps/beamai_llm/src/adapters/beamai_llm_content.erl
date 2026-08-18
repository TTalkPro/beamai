%%%-------------------------------------------------------------------
%%% @doc 多模态内容部件构造模块
%%%
%%% 为统一消息格式的 content 列表提供可读的构造函数，避免上层手写
%%% 裸 map。构造出的部件由 beamai_llm_message_adapter 转换为各
%%% Provider 的具体格式（OpenAI image_url / Anthropic image 等）。
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% {ok, Img} = beamai_llm_content:image_path(<<"/tmp/chart.png">>),
%%% Msg = beamai_message:user([
%%%     beamai_llm_content:text(<<"这张图说明了什么？"/utf8>>),
%%%     Img
%%% ]),
%%% {ok, Resp} = beamai_chat_completion:chat(Config, [Msg]).
%%% ```
%%%
%%% == 部件形态 ==
%%%
%%% ```erlang
%%% #{type => text,     text => binary()}
%%% #{type => image,    source => media_source(), detail => binary()}
%%% #{type => audio,    source => media_source()}
%%% #{type => video,    source => media_source()}
%%% #{type => document, source => media_source(), filename => binary(),
%%%   title => binary(), context => binary(), citations => boolean()}
%%% ```
%%%
%%% 任意部件都可携带 `cache_control'（Anthropic prompt 缓存断点），
%%% 不支持的 Provider 会忽略该字段。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_content).

%% 文本
-export([text/1]).

%% 图片
-export([image/1, image/2, image_url/1, image_url/2,
         image_base64/2, image_base64/3, image_file/1, image_path/1]).

%% 音频
-export([audio/1, audio/2, audio_base64/2, audio_url/1, audio_file/1, audio_path/1]).

%% 视频
-export([video/1, video/2, video_url/1, video_base64/2, video_path/1]).

%% 文档
-export([document/1, document/2, document_url/1, document_base64/2,
         document_file/1, document_path/1, document_text/1]).

%% 工具函数
-export([cache_breakpoint/1, is_multimodal/1, text_of/1]).

-type media_source() :: beamai_llm_media:media_source().
-type part() :: #{type := atom(), _ => _}.

-export_type([part/0]).

%%====================================================================
%% 文本
%%====================================================================

%% @doc 文本部件
-spec text(binary()) -> part().
text(Text) when is_binary(Text) ->
    #{type => text, text => Text}.

%%====================================================================
%% 图片
%%====================================================================

%% @doc 图片部件（由媒体源或 URL / data URI 构造）
-spec image(media_source() | binary()) -> part().
image(Source) ->
    image(Source, #{}).

%% @doc 图片部件（带选项）
%%
%% 选项：
%%   detail - OpenAI 图片精细度：`<<"low">>' | `<<"high">>' | `<<"auto">>'
%%   cache_control - Anthropic 缓存断点
-spec image(media_source() | binary(), map()) -> part().
image(Source, Opts) ->
    with_opts(#{type => image, source => to_source(Source)}, Opts,
              [detail, cache_control]).

%% @doc 由 URL（或 data URI）构造图片部件
-spec image_url(binary()) -> part().
image_url(Url) ->
    image(beamai_llm_media:url(Url), #{}).

%% @doc 由 URL 构造图片部件并指定精细度
-spec image_url(binary(), binary()) -> part().
image_url(Url, Detail) ->
    image(beamai_llm_media:url(Url), #{detail => Detail}).

%% @doc 由 base64 数据构造图片部件
-spec image_base64(binary(), binary()) -> part().
image_base64(MediaType, Data) ->
    image(beamai_llm_media:base64(MediaType, Data), #{}).

%% @doc 由 base64 数据构造图片部件（带选项）
-spec image_base64(binary(), binary(), map()) -> part().
image_base64(MediaType, Data, Opts) ->
    image(beamai_llm_media:base64(MediaType, Data), Opts).

%% @doc 由供应商 Files API 文件 ID 构造图片部件
-spec image_file(binary()) -> part().
image_file(FileId) ->
    image(beamai_llm_media:file_id(FileId), #{}).

%% @doc 读取本地图片文件构造图片部件
-spec image_path(binary() | string()) -> {ok, part()} | {error, term()}.
image_path(Path) ->
    case beamai_llm_media:from_file(Path) of
        {ok, Source} -> {ok, image(Source, #{})};
        {error, _} = Error -> Error
    end.

%%====================================================================
%% 音频
%%====================================================================

%% @doc 音频部件
-spec audio(media_source() | binary()) -> part().
audio(Source) ->
    audio(Source, #{}).

%% @doc 音频部件（带选项：format / cache_control）
%% 未显式给出 format 时，由媒体源的 MIME 类型推断（wav / mp3）。
-spec audio(media_source() | binary(), map()) -> part().
audio(Source, Opts) ->
    with_opts(#{type => audio, source => to_source(Source)}, Opts,
              [format, cache_control]).

%% @doc 由 base64 数据与格式构造音频部件
%% Format 为 OpenAI input_audio 的格式值（`<<"wav">>' / `<<"mp3">>'）。
-spec audio_base64(binary(), binary()) -> part().
audio_base64(Data, Format) when is_binary(Data), is_binary(Format) ->
    MediaType = <<"audio/", Format/binary>>,
    audio(beamai_llm_media:base64(MediaType, Data), #{format => Format}).

%% @doc 由 URL 构造音频部件（仅部分 Provider 支持音频 URL）
-spec audio_url(binary()) -> part().
audio_url(Url) ->
    audio(beamai_llm_media:url(Url), #{}).

%% @doc 由供应商 Files API 文件 ID 构造音频部件
-spec audio_file(binary()) -> part().
audio_file(FileId) ->
    audio(beamai_llm_media:file_id(FileId), #{}).

%% @doc 读取本地音频文件构造音频部件
-spec audio_path(binary() | string()) -> {ok, part()} | {error, term()}.
audio_path(Path) ->
    case beamai_llm_media:from_file(Path) of
        {ok, Source} -> {ok, audio(Source, #{})};
        {error, _} = Error -> Error
    end.

%%====================================================================
%% 视频
%%====================================================================

%% @doc 视频部件（Qwen-VL / GLM-4V 等支持 video_url 的模型）
-spec video(media_source() | binary()) -> part().
video(Source) ->
    video(Source, #{}).

%% @doc 视频部件（带选项）
-spec video(media_source() | binary(), map()) -> part().
video(Source, Opts) ->
    with_opts(#{type => video, source => to_source(Source)}, Opts,
              [fps, cache_control]).

%% @doc 由 URL 构造视频部件
-spec video_url(binary()) -> part().
video_url(Url) ->
    video(beamai_llm_media:url(Url), #{}).

%% @doc 由 base64 数据构造视频部件
-spec video_base64(binary(), binary()) -> part().
video_base64(MediaType, Data) ->
    video(beamai_llm_media:base64(MediaType, Data), #{}).

%% @doc 读取本地视频文件构造视频部件
-spec video_path(binary() | string()) -> {ok, part()} | {error, term()}.
video_path(Path) ->
    case beamai_llm_media:from_file(Path) of
        {ok, Source} -> {ok, video(Source, #{})};
        {error, _} = Error -> Error
    end.

%%====================================================================
%% 文档
%%====================================================================

%% @doc 文档部件（PDF 等）
-spec document(media_source() | binary()) -> part().
document(Source) ->
    document(Source, #{}).

%% @doc 文档部件（带选项）
%%
%% 选项：
%%   filename  - OpenAI file 部件的文件名
%%   title     - Anthropic 文档标题
%%   context   - Anthropic 文档上下文说明
%%   citations - Anthropic 引用开关（true 时响应携带 citations）
-spec document(media_source() | binary(), map()) -> part().
document(Source, Opts) ->
    with_opts(#{type => document, source => to_source(Source)}, Opts,
              [filename, title, context, citations, cache_control]).

%% @doc 由 URL 构造文档部件
-spec document_url(binary()) -> part().
document_url(Url) ->
    document(beamai_llm_media:url(Url), #{}).

%% @doc 由 base64 数据构造文档部件
-spec document_base64(binary(), binary()) -> part().
document_base64(MediaType, Data) ->
    document(beamai_llm_media:base64(MediaType, Data), #{}).

%% @doc 由供应商 Files API 文件 ID 构造文档部件
-spec document_file(binary()) -> part().
document_file(FileId) ->
    document(beamai_llm_media:file_id(FileId), #{}).

%% @doc 读取本地文档文件构造文档部件（文件名默认取路径末段）
-spec document_path(binary() | string()) -> {ok, part()} | {error, term()}.
document_path(Path) ->
    case beamai_llm_media:from_file(Path) of
        {ok, Source} -> {ok, document(Source, #{filename => basename(Path)})};
        {error, _} = Error -> Error
    end.

%% @doc 纯文本文档部件（Anthropic 的 text 文档源）
-spec document_text(binary()) -> part().
document_text(Text) when is_binary(Text) ->
    document(#{type => base64, media_type => <<"text/plain">>,
               data => base64:encode(Text)}, #{}).

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 在部件上标记 Anthropic 缓存断点
-spec cache_breakpoint(part()) -> part().
cache_breakpoint(Part) when is_map(Part) ->
    Part#{cache_control => #{<<"type">> => <<"ephemeral">>}}.

%% @doc 判断消息内容是否为多模态（content 为部件列表）
-spec is_multimodal(term()) -> boolean().
is_multimodal(Content) when is_list(Content), Content =/= [] -> true;
is_multimodal(_) -> false.

%% @doc 提取内容中的纯文本（多模态部件列表拼接 text 部件）
-spec text_of(term()) -> binary().
text_of(Content) when is_binary(Content) -> Content;
text_of(Parts) when is_list(Parts) ->
    iolist_to_binary([T || #{type := text, text := T} <- Parts]);
text_of(_) -> <<>>.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 允许直接传 URL / data URI 字符串
to_source(Source) when is_binary(Source) -> beamai_llm_media:url(Source);
to_source(Source) when is_map(Source) -> Source.

%% @private 按白名单把选项并入部件
with_opts(Part, Opts, Keys) ->
    lists:foldl(fun(Key, Acc) ->
        case maps:get(Key, Opts, undefined) of
            undefined -> Acc;
            Value -> Acc#{Key => Value}
        end
    end, Part, Keys).

%% @private 取路径末段作为文件名
basename(Path) when is_list(Path) -> basename(list_to_binary(Path));
basename(Path) when is_binary(Path) ->
    lists:last(binary:split(Path, <<"/">>, [global])).
