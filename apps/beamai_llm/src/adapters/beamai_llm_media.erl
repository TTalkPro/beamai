%%%-------------------------------------------------------------------
%%% @doc 多模态媒体工具模块
%%%
%%% 提供多模态输入（图片 / 音频 / 视频 / 文档）所需的媒体源构造、
%%% MIME 类型推断与 data URI 编解码能力。
%%%
%%% == 媒体源（media_source）==
%%%
%%% 统一的三种来源形态，供 beamai_llm_message_adapter 转换为各
%%% Provider 的具体格式：
%%%
%%% ```erlang
%%% #{type => base64,  media_type => <<"image/png">>, data => Base64}
%%% #{type => url,     url => <<"https://...">>}
%%% #{type => file_id, file_id => <<"file-abc">>}     %% 供应商 Files API
%%% ```
%%%
%%% == 类型推断 ==
%%%
%%% 优先按文件扩展名推断，失败时回退到魔数（magic bytes）嗅探，
%%% 对齐 Vercel AI SDK 的 detectMediaType 策略。
%%%
%%% ```erlang
%%% {ok, Src} = beamai_llm_media:from_file(<<"/tmp/cat.png">>),
%%% <<"image/png">> = maps:get(media_type, Src),
%%% DataUri = beamai_llm_media:to_data_uri(Src).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_media).

%% 媒体源构造
-export([base64/1, base64/2, url/1, file_id/1]).
-export([from_file/1, from_binary/1, from_binary/2]).

%% data URI 编解码
-export([to_data_uri/1, from_data_uri/1]).

%% 类型推断
-export([detect_media_type/1, detect_media_type_base64/1]).
-export([media_type_from_extension/1, extension_from_media_type/1]).
-export([top_level_type/1, audio_format/1]).

%% 访问器
-export([media_type/1, is_source/1]).

-type media_source() ::
    #{type := base64, media_type := binary(), data := binary()} |
    #{type := url, url := binary()} |
    #{type := file_id, file_id := binary()}.

-export_type([media_source/0]).

-define(DEFAULT_MEDIA_TYPE, <<"application/octet-stream">>).

%%====================================================================
%% 媒体源构造
%%====================================================================

%% @doc 由 base64 数据构造媒体源（自动嗅探 MIME 类型）
-spec base64(binary()) -> media_source().
base64(Data) when is_binary(Data) ->
    base64(detect_media_type_base64(Data), Data).

%% @doc 由 base64 数据与 MIME 类型构造媒体源
-spec base64(binary() | undefined, binary()) -> media_source().
base64(undefined, Data) ->
    #{type => base64, media_type => ?DEFAULT_MEDIA_TYPE, data => Data};
base64(MediaType, Data) when is_binary(MediaType), is_binary(Data) ->
    #{type => base64, media_type => MediaType, data => Data}.

%% @doc 由 URL 构造媒体源
%% 传入 data URI 时自动解析为 base64 媒体源。
-spec url(binary()) -> media_source().
url(<<"data:", _/binary>> = DataUri) ->
    case from_data_uri(DataUri) of
        {ok, Source} -> Source;
        {error, _} -> #{type => url, url => DataUri}
    end;
url(Url) when is_binary(Url) ->
    #{type => url, url => Url}.

%% @doc 由供应商 Files API 的文件 ID 构造媒体源
-spec file_id(binary()) -> media_source().
file_id(Id) when is_binary(Id) ->
    #{type => file_id, file_id => Id}.

%% @doc 读取本地文件构造 base64 媒体源
%% MIME 类型按扩展名推断，扩展名未知时回退到魔数嗅探。
-spec from_file(binary() | string()) -> {ok, media_source()} | {error, term()}.
from_file(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            MediaType = case media_type_from_extension(to_binary(Path)) of
                undefined -> detect_media_type(Bin);
                MT -> MT
            end,
            {ok, base64(MediaType, base64:encode(Bin))};
        {error, Reason} ->
            {error, {read_file_failed, Path, Reason}}
    end.

%% @doc 由原始二进制构造 base64 媒体源（自动嗅探 MIME 类型）
-spec from_binary(binary()) -> media_source().
from_binary(Bin) when is_binary(Bin) ->
    base64(detect_media_type(Bin), base64:encode(Bin)).

%% @doc 由原始二进制与 MIME 类型构造 base64 媒体源
-spec from_binary(binary(), binary()) -> media_source().
from_binary(Bin, MediaType) when is_binary(Bin), is_binary(MediaType) ->
    base64(MediaType, base64:encode(Bin)).

%%====================================================================
%% data URI 编解码
%%====================================================================

%% @doc 媒体源转 URL 形态
%% base64 源编码为 data URI；url 源原样返回；file_id 源无 URL 表示。
-spec to_data_uri(media_source()) -> binary() | undefined.
to_data_uri(#{type := base64, media_type := MT, data := Data}) ->
    <<"data:", MT/binary, ";base64,", Data/binary>>;
to_data_uri(#{type := url, url := U}) ->
    U;
to_data_uri(_) ->
    undefined.

%% @doc 解析 data URI 为 base64 媒体源
-spec from_data_uri(binary()) -> {ok, media_source()} | {error, term()}.
from_data_uri(<<"data:", Rest/binary>>) ->
    case binary:split(Rest, <<",">>) of
        [Header, Data] ->
            case binary:split(Header, <<";">>, [global]) of
                [MediaType | Flags] ->
                    case lists:member(<<"base64">>, Flags) of
                        true -> {ok, base64(normalize_media_type(MediaType), Data)};
                        false -> {ok, base64(normalize_media_type(MediaType), base64:encode(Data))}
                    end;
                _ ->
                    {error, invalid_data_uri}
            end;
        _ ->
            {error, invalid_data_uri}
    end;
from_data_uri(_) ->
    {error, not_a_data_uri}.

%% @private data URI 头部可能为空（data:;base64,...）
normalize_media_type(<<>>) -> ?DEFAULT_MEDIA_TYPE;
normalize_media_type(MT) -> MT.

%%====================================================================
%% 类型推断
%%====================================================================

%% @doc 按魔数嗅探 MIME 类型（原始二进制）
-spec detect_media_type(binary()) -> binary() | undefined.
detect_media_type(<<16#FF, 16#D8, _/binary>>) -> <<"image/jpeg">>;
detect_media_type(<<"GIF", _/binary>>) -> <<"image/gif">>;
detect_media_type(<<16#89, "PNG", _/binary>>) -> <<"image/png">>;
detect_media_type(<<"RIFF", _:4/binary, "WEBP", _/binary>>) -> <<"image/webp">>;
detect_media_type(<<"RIFF", _:4/binary, "WAVE", _/binary>>) -> <<"audio/wav">>;
detect_media_type(<<"BM", _/binary>>) -> <<"image/bmp">>;
detect_media_type(<<16#49, 16#49, 16#2A, 16#00, _/binary>>) -> <<"image/tiff">>;
detect_media_type(<<16#4D, 16#4D, 16#00, 16#2A, _/binary>>) -> <<"image/tiff">>;
detect_media_type(<<"%PDF", _/binary>>) -> <<"application/pdf">>;
detect_media_type(<<"OggS", _/binary>>) -> <<"audio/ogg">>;
detect_media_type(<<"fLaC", _/binary>>) -> <<"audio/flac">>;
detect_media_type(<<_:4/binary, "ftypavif", _/binary>>) -> <<"image/avif">>;
detect_media_type(<<_:4/binary, "ftypheic", _/binary>>) -> <<"image/heic">>;
detect_media_type(<<_:4/binary, "ftyp", _/binary>>) -> <<"video/mp4">>;
detect_media_type(<<16#1A, 16#45, 16#DF, 16#A3, _/binary>>) -> <<"video/webm">>;
detect_media_type(<<16#FF, B, _/binary>>) when (B band 16#E0) =:= 16#E0 -> <<"audio/mpeg">>;
detect_media_type(_) -> undefined.

%% @doc 按魔数嗅探 MIME 类型（base64 编码数据）
%% 只解码首段（16 字节原始数据对应 24 个 base64 字符）足以覆盖所有签名。
-spec detect_media_type_base64(binary()) -> binary() | undefined.
detect_media_type_base64(Data) when byte_size(Data) >= 24 ->
    <<Prefix:24/binary, _/binary>> = Data,
    safe_detect_base64(Prefix);
detect_media_type_base64(Data) when is_binary(Data) ->
    %% 不足一个完整解码块时按 4 字节对齐截断，避免 base64 解码报错
    Aligned = binary:part(Data, 0, byte_size(Data) - (byte_size(Data) rem 4)),
    safe_detect_base64(Aligned);
detect_media_type_base64(_) ->
    undefined.

%% @private base64 解码失败（非法字符 / 已是原始数据）时返回 undefined
safe_detect_base64(<<>>) ->
    undefined;
safe_detect_base64(Prefix) ->
    try detect_media_type(base64:decode(Prefix))
    catch _:_ -> undefined
    end.

%% @doc 按文件名 / URL 扩展名推断 MIME 类型
-spec media_type_from_extension(binary()) -> binary() | undefined.
media_type_from_extension(Path) when is_binary(Path) ->
    %% 去掉 query / fragment，只看路径末段扩展名
    Clean = hd(binary:split(hd(binary:split(Path, <<"?">>)), <<"#">>)),
    case binary:split(Clean, <<".">>, [global]) of
        [_] -> undefined;
        Parts -> extension_media_type(string:lowercase(lists:last(Parts)))
    end;
media_type_from_extension(_) ->
    undefined.

%% @doc MIME 类型对应的常用扩展名
-spec extension_from_media_type(binary()) -> binary() | undefined.
extension_from_media_type(<<"image/jpeg">>) -> <<"jpg">>;
extension_from_media_type(<<"image/png">>) -> <<"png">>;
extension_from_media_type(<<"image/gif">>) -> <<"gif">>;
extension_from_media_type(<<"image/webp">>) -> <<"webp">>;
extension_from_media_type(<<"image/bmp">>) -> <<"bmp">>;
extension_from_media_type(<<"image/tiff">>) -> <<"tiff">>;
extension_from_media_type(<<"image/avif">>) -> <<"avif">>;
extension_from_media_type(<<"image/heic">>) -> <<"heic">>;
extension_from_media_type(<<"application/pdf">>) -> <<"pdf">>;
extension_from_media_type(<<"audio/wav">>) -> <<"wav">>;
extension_from_media_type(<<"audio/mpeg">>) -> <<"mp3">>;
extension_from_media_type(<<"audio/mp3">>) -> <<"mp3">>;
extension_from_media_type(<<"audio/ogg">>) -> <<"ogg">>;
extension_from_media_type(<<"audio/flac">>) -> <<"flac">>;
extension_from_media_type(<<"audio/mp4">>) -> <<"m4a">>;
extension_from_media_type(<<"audio/webm">>) -> <<"webm">>;
extension_from_media_type(<<"video/mp4">>) -> <<"mp4">>;
extension_from_media_type(<<"video/webm">>) -> <<"webm">>;
extension_from_media_type(<<"text/plain">>) -> <<"txt">>;
extension_from_media_type(_) -> undefined.

%% @private 扩展名 → MIME
extension_media_type(<<"jpg">>) -> <<"image/jpeg">>;
extension_media_type(<<"jpeg">>) -> <<"image/jpeg">>;
extension_media_type(<<"png">>) -> <<"image/png">>;
extension_media_type(<<"gif">>) -> <<"image/gif">>;
extension_media_type(<<"webp">>) -> <<"image/webp">>;
extension_media_type(<<"bmp">>) -> <<"image/bmp">>;
extension_media_type(<<"tif">>) -> <<"image/tiff">>;
extension_media_type(<<"tiff">>) -> <<"image/tiff">>;
extension_media_type(<<"avif">>) -> <<"image/avif">>;
extension_media_type(<<"heic">>) -> <<"image/heic">>;
extension_media_type(<<"pdf">>) -> <<"application/pdf">>;
extension_media_type(<<"wav">>) -> <<"audio/wav">>;
extension_media_type(<<"mp3">>) -> <<"audio/mpeg">>;
extension_media_type(<<"ogg">>) -> <<"audio/ogg">>;
extension_media_type(<<"flac">>) -> <<"audio/flac">>;
extension_media_type(<<"m4a">>) -> <<"audio/mp4">>;
extension_media_type(<<"aac">>) -> <<"audio/aac">>;
extension_media_type(<<"mp4">>) -> <<"video/mp4">>;
extension_media_type(<<"webm">>) -> <<"video/webm">>;
extension_media_type(<<"mov">>) -> <<"video/quicktime">>;
extension_media_type(<<"txt">>) -> <<"text/plain">>;
extension_media_type(<<"md">>) -> <<"text/markdown">>;
extension_media_type(<<"csv">>) -> <<"text/csv">>;
extension_media_type(<<"json">>) -> <<"application/json">>;
extension_media_type(_) -> undefined.

%% @doc 取 MIME 顶层类型（image / audio / video / application / text）
-spec top_level_type(binary() | undefined) -> atom().
top_level_type(undefined) -> unknown;
top_level_type(MediaType) when is_binary(MediaType) ->
    case binary:split(MediaType, <<"/">>) of
        [<<"image">> | _] -> image;
        [<<"audio">> | _] -> audio;
        [<<"video">> | _] -> video;
        [<<"text">> | _] -> text;
        [<<"application">> | _] -> application;
        _ -> unknown
    end;
top_level_type(_) -> unknown.

%% @doc MIME 类型映射为 OpenAI input_audio 的 format 值
%% OpenAI 仅接受 wav / mp3，其余格式原样返回子类型交由服务端判定。
-spec audio_format(binary() | undefined) -> binary() | undefined.
audio_format(undefined) -> undefined;
audio_format(<<"audio/wav">>) -> <<"wav">>;
audio_format(<<"audio/x-wav">>) -> <<"wav">>;
audio_format(<<"audio/wave">>) -> <<"wav">>;
audio_format(<<"audio/mpeg">>) -> <<"mp3">>;
audio_format(<<"audio/mp3">>) -> <<"mp3">>;
audio_format(<<"audio/", Sub/binary>>) -> Sub;
audio_format(_) -> undefined.

%%====================================================================
%% 访问器
%%====================================================================

%% @doc 取媒体源的 MIME 类型（url / file_id 源无类型信息）
-spec media_type(media_source()) -> binary() | undefined.
media_type(#{media_type := MT}) -> MT;
media_type(#{type := url, url := U}) -> media_type_from_extension(U);
media_type(_) -> undefined.

%% @doc 判断是否为合法媒体源
-spec is_source(term()) -> boolean().
is_source(#{type := base64, data := _}) -> true;
is_source(#{type := url, url := _}) -> true;
is_source(#{type := file_id, file_id := _}) -> true;
is_source(_) -> false.

%%====================================================================
%% 内部函数
%%====================================================================

to_binary(B) when is_binary(B) -> B;
to_binary(L) when is_list(L) -> list_to_binary(L);
to_binary(A) when is_atom(A) -> atom_to_binary(A, utf8).
