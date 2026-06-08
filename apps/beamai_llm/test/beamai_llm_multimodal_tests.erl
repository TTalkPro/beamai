%%%-------------------------------------------------------------------
%%% @doc 多模态消息适配单元测试
%%%
%%% 覆盖 message_adapter 对 content 列表（多模态部件）的转换：
%%%   - 图片（base64 / url）→ OpenAI image_url / Anthropic image
%%%   - 音频 → OpenAI input_audio（Anthropic 丢弃）
%%%   - PDF 文档 → Anthropic document（OpenAI file）
%%%   - 纯文本字符串保持原样透传（向后兼容）
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_multimodal_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 向后兼容：纯文本字符串不受影响
%%====================================================================

plain_text_passthrough_openai_test() ->
    [Msg] = beamai_llm_message_adapter:to_openai([#{role => user, content => <<"hello">>}]),
    ?assertEqual(<<"hello">>, maps:get(<<"content">>, Msg)).

plain_text_passthrough_anthropic_test() ->
    [Msg] = beamai_llm_message_adapter:to_anthropic([#{role => user, content => <<"hello">>}]),
    ?assertEqual(<<"hello">>, maps:get(<<"content">>, Msg)).

%%====================================================================
%% 图片：base64
%%====================================================================

image_base64_openai_test() ->
    Content = [
        #{type => text, text => <<"这是什么？"/utf8>>},
        #{type => image, source => #{type => base64, media_type => <<"image/png">>, data => <<"AAAA">>}}
    ],
    [Msg] = beamai_llm_message_adapter:to_openai([#{role => user, content => Content}]),
    [TextPart, ImgPart] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"text">>, maps:get(<<"type">>, TextPart)),
    ?assertEqual(<<"image_url">>, maps:get(<<"type">>, ImgPart)),
    Url = maps:get(<<"url">>, maps:get(<<"image_url">>, ImgPart)),
    ?assertEqual(<<"data:image/png;base64,AAAA">>, Url).

image_base64_anthropic_test() ->
    Content = [
        #{type => image, source => #{type => base64, media_type => <<"image/jpeg">>, data => <<"BBBB">>}}
    ],
    [Msg] = beamai_llm_message_adapter:to_anthropic([#{role => user, content => Content}]),
    [ImgBlock] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"image">>, maps:get(<<"type">>, ImgBlock)),
    Src = maps:get(<<"source">>, ImgBlock),
    ?assertEqual(<<"base64">>, maps:get(<<"type">>, Src)),
    ?assertEqual(<<"image/jpeg">>, maps:get(<<"media_type">>, Src)),
    ?assertEqual(<<"BBBB">>, maps:get(<<"data">>, Src)).

%%====================================================================
%% 图片：url
%%====================================================================

image_url_openai_test() ->
    Content = [#{type => image, source => #{type => url, url => <<"https://x/y.png">>}}],
    [Msg] = beamai_llm_message_adapter:to_openai([#{role => user, content => Content}]),
    [ImgPart] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"https://x/y.png">>, maps:get(<<"url">>, maps:get(<<"image_url">>, ImgPart))).

image_url_anthropic_test() ->
    Content = [#{type => image, source => #{type => url, url => <<"https://x/y.png">>}}],
    [Msg] = beamai_llm_message_adapter:to_anthropic([#{role => user, content => Content}]),
    [ImgBlock] = maps:get(<<"content">>, Msg),
    Src = maps:get(<<"source">>, ImgBlock),
    ?assertEqual(<<"url">>, maps:get(<<"type">>, Src)),
    ?assertEqual(<<"https://x/y.png">>, maps:get(<<"url">>, Src)).

%%====================================================================
%% 音频（OpenAI input_audio；Anthropic 丢弃）
%%====================================================================

audio_openai_test() ->
    Content = [#{type => audio, data => <<"ZZZZ">>, format => <<"wav">>}],
    [Msg] = beamai_llm_message_adapter:to_openai([#{role => user, content => Content}]),
    [AudioPart] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"input_audio">>, maps:get(<<"type">>, AudioPart)),
    IA = maps:get(<<"input_audio">>, AudioPart),
    ?assertEqual(<<"ZZZZ">>, maps:get(<<"data">>, IA)),
    ?assertEqual(<<"wav">>, maps:get(<<"format">>, IA)).

audio_dropped_for_anthropic_test() ->
    Content = [
        #{type => text, text => <<"听一下"/utf8>>},
        #{type => audio, data => <<"ZZZZ">>, format => <<"wav">>}
    ],
    [Msg] = beamai_llm_message_adapter:to_anthropic([#{role => user, content => Content}]),
    Blocks = maps:get(<<"content">>, Msg),
    %% 音频被丢弃，仅剩文本块
    ?assertEqual(1, length(Blocks)),
    ?assertEqual(<<"text">>, maps:get(<<"type">>, hd(Blocks))).

%%====================================================================
%% PDF 文档（Anthropic document；OpenAI file）
%%====================================================================

document_anthropic_test() ->
    Content = [#{type => document,
                 source => #{type => base64, media_type => <<"application/pdf">>, data => <<"PDF==">>}}],
    [Msg] = beamai_llm_message_adapter:to_anthropic([#{role => user, content => Content}]),
    [DocBlock] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"document">>, maps:get(<<"type">>, DocBlock)),
    Src = maps:get(<<"source">>, DocBlock),
    ?assertEqual(<<"application/pdf">>, maps:get(<<"media_type">>, Src)),
    %% 默认不带 citations
    ?assertNot(maps:is_key(<<"citations">>, DocBlock)).

document_with_citations_anthropic_test() ->
    Content = [#{type => document,
                 source => #{type => base64, media_type => <<"application/pdf">>, data => <<"PDF==">>},
                 citations => true}],
    [Msg] = beamai_llm_message_adapter:to_anthropic([#{role => user, content => Content}]),
    [DocBlock] = maps:get(<<"content">>, Msg),
    ?assertEqual(#{<<"enabled">> => true}, maps:get(<<"citations">>, DocBlock)).

document_openai_file_test() ->
    Content = [#{type => document,
                 source => #{type => base64, media_type => <<"application/pdf">>, data => <<"PDF==">>},
                 filename => <<"a.pdf">>}],
    [Msg] = beamai_llm_message_adapter:to_openai([#{role => user, content => Content}]),
    [FilePart] = maps:get(<<"content">>, Msg),
    ?assertEqual(<<"file">>, maps:get(<<"type">>, FilePart)),
    File = maps:get(<<"file">>, FilePart),
    ?assertEqual(<<"a.pdf">>, maps:get(<<"filename">>, File)),
    ?assertEqual(<<"data:application/pdf;base64,PDF==">>, maps:get(<<"file_data">>, File)).

%%====================================================================
%% 混合内容顺序保持
%%====================================================================

mixed_parts_order_preserved_test() ->
    Content = [
        #{type => text, text => <<"a">>},
        #{type => image, source => #{type => url, url => <<"u1">>}},
        #{type => text, text => <<"b">>}
    ],
    [Msg] = beamai_llm_message_adapter:to_openai([#{role => user, content => Content}]),
    Parts = maps:get(<<"content">>, Msg),
    Types = [maps:get(<<"type">>, P) || P <- Parts],
    ?assertEqual([<<"text">>, <<"image_url">>, <<"text">>], Types).
