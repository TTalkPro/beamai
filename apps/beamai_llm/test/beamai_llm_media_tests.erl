%%%-------------------------------------------------------------------
%%% @doc 多模态媒体工具与内容构造单元测试
%%%
%%% 覆盖：
%%%   - MIME 类型嗅探（魔数 / 扩展名）与 data URI 编解码
%%%   - beamai_llm_content 构造出的部件形态
%%%   - 新增多模态映射：图片 detail、file_id、视频、
%%%     音频格式推断、Anthropic 文档 title/context/cache_control
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_media_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% MIME 类型嗅探
%%====================================================================

detect_png_test() ->
    Png = <<16#89, "PNG", 13, 10, 26, 10, 0, 0, 0, 13>>,
    ?assertEqual(<<"image/png">>, beamai_llm_media:detect_media_type(Png)).

detect_jpeg_test() ->
    ?assertEqual(<<"image/jpeg">>, beamai_llm_media:detect_media_type(<<16#FF, 16#D8, 16#FF, 16#E0>>)).

detect_pdf_test() ->
    ?assertEqual(<<"application/pdf">>, beamai_llm_media:detect_media_type(<<"%PDF-1.7\n">>)).

detect_wav_test() ->
    Wav = <<"RIFF", 36:32/little, "WAVEfmt ">>,
    ?assertEqual(<<"audio/wav">>, beamai_llm_media:detect_media_type(Wav)).

detect_webp_test() ->
    Webp = <<"RIFF", 100:32/little, "WEBPVP8 ">>,
    ?assertEqual(<<"image/webp">>, beamai_llm_media:detect_media_type(Webp)).

detect_unknown_test() ->
    ?assertEqual(undefined, beamai_llm_media:detect_media_type(<<"not a known header">>)).

detect_from_base64_test() ->
    Png = <<16#89, "PNG", 13, 10, 26, 10, 0, 0, 0, 13, 0, 0, 0, 0, 0, 0>>,
    B64 = base64:encode(Png),
    ?assertEqual(<<"image/png">>, beamai_llm_media:detect_media_type_base64(B64)).

extension_mapping_test() ->
    ?assertEqual(<<"image/png">>, beamai_llm_media:media_type_from_extension(<<"/tmp/a.PNG">>)),
    ?assertEqual(<<"image/jpeg">>, beamai_llm_media:media_type_from_extension(<<"https://x/y.jpg?v=1">>)),
    ?assertEqual(undefined, beamai_llm_media:media_type_from_extension(<<"/tmp/noext">>)),
    ?assertEqual(<<"pdf">>, beamai_llm_media:extension_from_media_type(<<"application/pdf">>)).

top_level_type_test() ->
    ?assertEqual(image, beamai_llm_media:top_level_type(<<"image/png">>)),
    ?assertEqual(audio, beamai_llm_media:top_level_type(<<"audio/wav">>)),
    ?assertEqual(unknown, beamai_llm_media:top_level_type(undefined)).

audio_format_test() ->
    ?assertEqual(<<"wav">>, beamai_llm_media:audio_format(<<"audio/wav">>)),
    ?assertEqual(<<"mp3">>, beamai_llm_media:audio_format(<<"audio/mpeg">>)),
    ?assertEqual(<<"flac">>, beamai_llm_media:audio_format(<<"audio/flac">>)).

%%====================================================================
%% data URI
%%====================================================================

data_uri_roundtrip_test() ->
    Source = beamai_llm_media:base64(<<"image/png">>, <<"AAAA">>),
    Uri = beamai_llm_media:to_data_uri(Source),
    ?assertEqual(<<"data:image/png;base64,AAAA">>, Uri),
    ?assertEqual({ok, Source}, beamai_llm_media:from_data_uri(Uri)).

url_with_data_uri_becomes_base64_source_test() ->
    Source = beamai_llm_media:url(<<"data:image/gif;base64,BBBB">>),
    ?assertEqual(#{type => base64, media_type => <<"image/gif">>, data => <<"BBBB">>}, Source).

from_data_uri_invalid_test() ->
    ?assertEqual({error, not_a_data_uri}, beamai_llm_media:from_data_uri(<<"https://x/y.png">>)).

from_file_test() ->
    Path = "/tmp/beamai_media_from_file_test.png",
    Png = <<16#89, "PNG", 13, 10, 26, 10, 0, 0, 0, 13>>,
    ok = file:write_file(Path, Png),
    {ok, Source} = beamai_llm_media:from_file(Path),
    ?assertEqual(<<"image/png">>, maps:get(media_type, Source)),
    ?assertEqual(base64:encode(Png), maps:get(data, Source)),
    ok = file:delete(Path).

from_file_missing_test() ->
    ?assertMatch({error, {read_file_failed, _, enoent}},
                 beamai_llm_media:from_file(<<"/nonexistent/beamai/x.png">>)).

%%====================================================================
%% 内容部件构造
%%====================================================================

content_text_test() ->
    ?assertEqual(#{type => text, text => <<"hi">>}, beamai_llm_content:text(<<"hi">>)).

content_image_url_with_detail_test() ->
    Part = beamai_llm_content:image_url(<<"https://x/y.png">>, <<"high">>),
    ?assertEqual(image, maps:get(type, Part)),
    ?assertEqual(<<"high">>, maps:get(detail, Part)),
    ?assertEqual(#{type => url, url => <<"https://x/y.png">>}, maps:get(source, Part)).

content_document_options_test() ->
    Part = beamai_llm_content:document(
        beamai_llm_media:base64(<<"application/pdf">>, <<"UERG">>),
        #{filename => <<"a.pdf">>, title => <<"标题"/utf8>>, citations => true}),
    ?assertEqual(<<"a.pdf">>, maps:get(filename, Part)),
    ?assertEqual(<<"标题"/utf8>>, maps:get(title, Part)),
    ?assert(maps:get(citations, Part)).

content_cache_breakpoint_test() ->
    Part = beamai_llm_content:cache_breakpoint(beamai_llm_content:text(<<"x">>)),
    ?assertEqual(#{<<"type">> => <<"ephemeral">>}, maps:get(cache_control, Part)).

content_text_of_test() ->
    Parts = [beamai_llm_content:text(<<"a">>),
             beamai_llm_content:image_url(<<"https://x/y.png">>),
             beamai_llm_content:text(<<"b">>)],
    ?assertEqual(<<"ab">>, beamai_llm_content:text_of(Parts)),
    ?assertEqual(<<"plain">>, beamai_llm_content:text_of(<<"plain">>)).

%%====================================================================
%% OpenAI 侧新增映射
%%====================================================================

openai_image_detail_test() ->
    Content = [beamai_llm_content:image_url(<<"https://x/y.png">>, <<"low">>)],
    [Msg] = beamai_llm_message_adapter:to_openai([#{role => user, content => Content}]),
    [Part] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"image_url">>, maps:get(<<"type">>, Part)),
    ?assertEqual(<<"low">>, maps:get(<<"detail">>, maps:get(<<"image_url">>, Part))).

openai_image_file_id_test() ->
    Content = [beamai_llm_content:image_file(<<"file-abc">>)],
    [Msg] = beamai_llm_message_adapter:to_openai([#{role => user, content => Content}]),
    [Part] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"file">>, maps:get(<<"type">>, Part)),
    ?assertEqual(#{<<"file_id">> => <<"file-abc">>}, maps:get(<<"file">>, Part)).

openai_video_url_test() ->
    Content = [beamai_llm_content:video_url(<<"https://x/y.mp4">>)],
    [Msg] = beamai_llm_message_adapter:to_openai([#{role => user, content => Content}]),
    [Part] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"video_url">>, maps:get(<<"type">>, Part)),
    ?assertEqual(<<"https://x/y.mp4">>, maps:get(<<"url">>, maps:get(<<"video_url">>, Part))).

openai_audio_format_inferred_test() ->
    Content = [beamai_llm_content:audio(
        beamai_llm_media:base64(<<"audio/mpeg">>, <<"ZZZZ">>))],
    [Msg] = beamai_llm_message_adapter:to_openai([#{role => user, content => Content}]),
    [Part] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"input_audio">>, maps:get(<<"type">>, Part)),
    Audio = maps:get(<<"input_audio">>, Part),
    ?assertEqual(<<"mp3">>, maps:get(<<"format">>, Audio)),
    ?assertEqual(<<"ZZZZ">>, maps:get(<<"data">>, Audio)).

openai_audio_url_test() ->
    Content = [beamai_llm_content:audio_url(<<"https://x/y.mp3">>)],
    [Msg] = beamai_llm_message_adapter:to_openai([#{role => user, content => Content}]),
    [Part] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"input_audio">>, maps:get(<<"type">>, Part)),
    Audio = maps:get(<<"input_audio">>, Part),
    ?assertEqual(<<"https://x/y.mp3">>, maps:get(<<"data">>, Audio)),
    %% 格式由 URL 扩展名推断
    ?assertEqual(<<"mp3">>, maps:get(<<"format">>, Audio)).

openai_audio_legacy_shape_test() ->
    %% 旧形态（data + format）继续可用
    Content = [#{type => audio, data => <<"ZZZZ">>, format => <<"wav">>}],
    [Msg] = beamai_llm_message_adapter:to_openai([#{role => user, content => Content}]),
    [Part] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"wav">>, maps:get(<<"format">>, maps:get(<<"input_audio">>, Part))).

openai_document_file_id_test() ->
    Content = [beamai_llm_content:document_file(<<"file-pdf">>)],
    [Msg] = beamai_llm_message_adapter:to_openai([#{role => user, content => Content}]),
    [Part] = maps:get(<<"content">>, Msg),
    ?assertEqual(#{<<"file_id">> => <<"file-pdf">>}, maps:get(<<"file">>, Part)).

%%====================================================================
%% Anthropic 侧新增映射
%%====================================================================

anthropic_image_file_id_test() ->
    Content = [beamai_llm_content:image_file(<<"file-abc">>)],
    [Msg] = beamai_llm_message_adapter:to_anthropic([#{role => user, content => Content}]),
    [Block] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"image">>, maps:get(<<"type">>, Block)),
    ?assertEqual(#{<<"type">> => <<"file">>, <<"file_id">> => <<"file-abc">>},
                 maps:get(<<"source">>, Block)).

anthropic_document_metadata_test() ->
    Content = [beamai_llm_content:document(
        beamai_llm_media:base64(<<"application/pdf">>, <<"UERG">>),
        #{title => <<"报告"/utf8>>, context => <<"季度数据"/utf8>>, citations => true})],
    [Msg] = beamai_llm_message_adapter:to_anthropic([#{role => user, content => Content}]),
    [Block] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"document">>, maps:get(<<"type">>, Block)),
    ?assertEqual(<<"报告"/utf8>>, maps:get(<<"title">>, Block)),
    ?assertEqual(<<"季度数据"/utf8>>, maps:get(<<"context">>, Block)),
    ?assertEqual(#{<<"enabled">> => true}, maps:get(<<"citations">>, Block)).

anthropic_document_text_source_test() ->
    Content = [beamai_llm_content:document_text(<<"纯文本内容"/utf8>>)],
    [Msg] = beamai_llm_message_adapter:to_anthropic([#{role => user, content => Content}]),
    [Block] = maps:get(<<"content">>, Msg),
    Source = maps:get(<<"source">>, Block),
    ?assertEqual(<<"text">>, maps:get(<<"type">>, Source)),
    ?assertEqual(<<"text/plain">>, maps:get(<<"media_type">>, Source)),
    ?assertEqual(<<"纯文本内容"/utf8>>, maps:get(<<"data">>, Source)).

anthropic_cache_control_test() ->
    Content = [beamai_llm_content:cache_breakpoint(beamai_llm_content:text(<<"长上下文"/utf8>>))],
    [Msg] = beamai_llm_message_adapter:to_anthropic([#{role => user, content => Content}]),
    [Block] = maps:get(<<"content">>, Msg),
    ?assertEqual(#{<<"type">> => <<"ephemeral">>}, maps:get(<<"cache_control">>, Block)).

anthropic_video_dropped_test() ->
    Content = [beamai_llm_content:text(<<"a">>), beamai_llm_content:video_url(<<"https://x/y.mp4">>)],
    [Msg] = beamai_llm_message_adapter:to_anthropic([#{role => user, content => Content}]),
    ?assertEqual(1, length(maps:get(<<"content">>, Msg))).

%%====================================================================
%% 续写标志
%%====================================================================

partial_flag_passthrough_test() ->
    [Msg] = beamai_llm_message_adapter:to_openai(
        [#{role => assistant, content => <<"续写"/utf8>>, partial => true}]),
    ?assert(maps:get(<<"partial">>, Msg)),
    ?assertNot(maps:is_key(<<"prefix">>, Msg)).

prefix_flag_still_works_test() ->
    [Msg] = beamai_llm_message_adapter:to_openai(
        [#{role => assistant, content => <<"x">>, prefix => true}]),
    ?assert(maps:get(<<"prefix">>, Msg)).
