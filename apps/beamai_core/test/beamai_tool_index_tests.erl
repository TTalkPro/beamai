%%%-------------------------------------------------------------------
%%% @doc 工具索引测试（regex / keyword 两后端 + 指纹）
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_index_tests).

-include_lib("eunit/include/eunit.hrl").

%% 构造工具（handler 从不被调用，索引只看 name/description/tag）
tool(Name, Desc, Tags) ->
    #{name => Name, handler => fun(_) -> {ok, ok} end,
      description => Desc, tag => Tags}.

%% 8 个工具的样例集
tools() ->
    [tool(<<"get_weather">>, <<"Get the current weather for a city">>, [<<"weather">>]),
     tool(<<"sendEmail">>, <<"Send an email message to a recipient">>, [<<"comms">>]),
     tool(<<"list_files">>, <<"List files in a directory">>, [<<"fs">>]),
     tool(<<"read_file">>, <<"Read the contents of a file">>, [<<"fs">>]),
     tool(<<"create_ticket">>, <<"Create a support ticket">>, [<<"jira">>]),
     tool(<<"search_web">>, <<"Search the web for information">>, [<<"web">>]),
     tool(<<"convert_currency">>, <<"Convert an amount between currencies">>, [<<"finance">>]),
     tool(<<"restart_server">>, <<"Restart a production server">>, [<<"ops">>])].

regex_index() -> beamai_tool_index:new(beamai_tool_index_regex, tools(), #{}).
keyword_index() -> beamai_tool_index:new(beamai_tool_index_keyword, tools(), #{}).

%% regex：8 个工具里捞出唯一相关的那个
regex_finds_relevant_test() ->
    ?assertEqual([<<"get_weather">>],
                 beamai_tool_index:search(regex_index(), <<"weather">>, 5)).

%% keyword：同样捞出最相关的那个（排第一）
keyword_finds_relevant_test() ->
    [First | _] = beamai_tool_index:search(keyword_index(), <<"weather">>, 5),
    ?assertEqual(<<"get_weather">>, First).

%% regex：MaxResults 截断（file 命中两个，只要一个）
regex_truncates_test() ->
    ?assertEqual([<<"list_files">>],
                 beamai_tool_index:search(regex_index(), <<"file">>, 1)).

%% keyword：MaxResults 截断
keyword_truncates_test() ->
    ?assertEqual(1, length(beamai_tool_index:search(keyword_index(), <<"file">>, 1))).

%% regex：非法正则不崩，降级字面匹配后无命中
regex_invalid_pattern_no_crash_test() ->
    ?assertEqual([], beamai_tool_index:search(regex_index(), <<"[unclosed">>, 5)).

%% regex：非法正则降级为字面子串匹配 —— 字面存在则应命中
regex_invalid_pattern_falls_back_to_literal_test() ->
    H = beamai_tool_index:new(beamai_tool_index_regex,
                              [tool(<<"probe">>, <<"matches a [bracket literal">>, [])], #{}),
    ?assertEqual([<<"probe">>], beamai_tool_index:search(H, <<"[bracket">>, 5)).

%% keyword：snake_case 查询命中 camelCase 工具名
keyword_snake_query_hits_camel_tool_test() ->
    [First | _] = beamai_tool_index:search(keyword_index(), <<"send_email">>, 3),
    ?assertEqual(<<"sendEmail">>, First).

%% keyword：camelCase 查询命中 snake_case 工具名
keyword_camel_query_hits_snake_tool_test() ->
    [First | _] = beamai_tool_index:search(keyword_index(), <<"getWeather">>, 3),
    ?assertEqual(<<"get_weather">>, First).

%% keyword：name 命中排在「仅 description 命中」之前
keyword_name_hit_outranks_description_hit_test() ->
    H = beamai_tool_index:new(beamai_tool_index_keyword,
                              [tool(<<"send_email">>, <<"Send a note about the weather">>, []),
                               tool(<<"weather_lookup">>, <<"Fetch data for a place">>, [])], #{}),
    ?assertEqual([<<"weather_lookup">>, <<"send_email">>],
                 beamai_tool_index:search(H, <<"weather">>, 5)).

%% regex：name 命中排在「仅 description 命中」之前
regex_name_hit_outranks_description_hit_test() ->
    H = beamai_tool_index:new(beamai_tool_index_regex,
                              [tool(<<"send_email">>, <<"Send a note about the weather">>, []),
                               tool(<<"weather_lookup">>, <<"Fetch data for a place">>, [])], #{}),
    ?assertEqual([<<"weather_lookup">>, <<"send_email">>],
                 beamai_tool_index:search(H, <<"weather">>, 5)).

%% regex：缺省不区分大小写
regex_case_insensitive_by_default_test() ->
    ?assertEqual([<<"get_weather">>],
                 beamai_tool_index:search(regex_index(), <<"WEATHER">>, 5)).

%% regex：开了 case_sensitive 则大小写不符不命中
regex_case_sensitive_opt_test() ->
    H = beamai_tool_index:new(beamai_tool_index_regex, tools(), #{case_sensitive => true}),
    ?assertEqual([], beamai_tool_index:search(H, <<"WEATHER">>, 5)).

%% 两后端：查询无匹配 → []
no_match_returns_empty_test() ->
    ?assertEqual([], beamai_tool_index:search(regex_index(), <<"zzzznope">>, 5)),
    ?assertEqual([], beamai_tool_index:search(keyword_index(), <<"zzzznope">>, 5)).

%% 两后端：空工具集 → []
empty_tool_set_returns_empty_test() ->
    R = beamai_tool_index:new(beamai_tool_index_regex, [], #{}),
    K = beamai_tool_index:new(beamai_tool_index_keyword, [], #{}),
    ?assertEqual([], beamai_tool_index:search(R, <<"weather">>, 5)),
    ?assertEqual([], beamai_tool_index:search(K, <<"weather">>, 5)).

%%====================================================================
%% 中文（CJK 二元组切词）
%%
%% 注意：中文 binary 字面量一律写 /utf8。源码按 UTF-8 读入，
%% `<<"天气">>` 会把码点截断成单字节（得 <<41,20>>）而非 UTF-8 编码。
%%====================================================================

%% 中文工具集
cn_tools() ->
    [tool(<<"get_weather">>, <<"查询指定城市的天气预报"/utf8>>, []),
     tool(<<"send_email">>, <<"给收件人发送一封电子邮件"/utf8>>, [])].

cn_keyword_index() -> beamai_tool_index:new(beamai_tool_index_keyword, cn_tools(), #{}).

%% keyword：中文查询召回中文描述的工具（二元组「天气」对上）
keyword_chinese_query_test() ->
    ?assertEqual([<<"get_weather">>],
                 beamai_tool_index:search(cn_keyword_index(), <<"天气"/utf8>>, 5)).

%% keyword：多词中文查询，最相关的排第一
keyword_chinese_multiword_query_test() ->
    [First | _] = beamai_tool_index:search(cn_keyword_index(), <<"查询天气"/utf8>>, 5),
    ?assertEqual(<<"get_weather">>, First).

%% keyword：中文查询召回另一个工具（验证不是恰好只有一个能中）
keyword_chinese_query_other_tool_test() ->
    ?assertEqual([<<"send_email">>],
                 beamai_tool_index:search(cn_keyword_index(), <<"邮件"/utf8>>, 5)).

%% keyword：单字 CJK run 不被丢弃 —— 单字查询能召回单字命名的工具
keyword_single_cjk_char_query_test() ->
    H = beamai_tool_index:new(beamai_tool_index_keyword,
                              [tool(<<"天"/utf8>>, <<"单字工具"/utf8>>, []),
                               tool(<<"other">>, <<"无关"/utf8>>, [])], #{}),
    ?assertEqual([<<"天"/utf8>>], beamai_tool_index:search(H, <<"天"/utf8>>, 5)).

%% keyword：单字查询命中不了多字文档 —— 二元组索引的已知局限，钉住行为
%% （文档 `天气预报` 只出 `天气`/`气预`/`预报`，没有 `天` 这个词元）
%% 对齐 Lucene CJKAnalyzer 缺省语义（outputUnigrams=false）。
keyword_single_cjk_char_misses_multichar_doc_test() ->
    ?assertEqual([], beamai_tool_index:search(cn_keyword_index(), <<"天"/utf8>>, 5)).

%% keyword：中文无匹配仍返回 []
keyword_chinese_no_match_test() ->
    ?assertEqual([], beamai_tool_index:search(cn_keyword_index(), <<"数据库"/utf8>>, 5)).

%% keyword：CJK 与 ASCII 双向断开 —— 混排文本两侧都能召回
keyword_mixed_cjk_ascii_splits_test() ->
    H = beamai_tool_index:new(beamai_tool_index_keyword,
                              [tool(<<"mixed">>, <<"天气weather 混排"/utf8>>, [])], #{}),
    %% 糊成一个词元的话，这两个查询都会落空
    ?assertEqual([<<"mixed">>], beamai_tool_index:search(H, <<"weather">>, 5)),
    ?assertEqual([<<"mixed">>], beamai_tool_index:search(H, <<"天气"/utf8>>, 5)).

%% keyword：中文工具名同样吃 name_boost（name 命中压过仅描述命中）
keyword_chinese_name_boost_test() ->
    H = beamai_tool_index:new(beamai_tool_index_keyword,
                              [tool(<<"发送邮件"/utf8>>, <<"把消息发出去"/utf8>>, []),
                               tool(<<"other_tool">>, <<"这个工具跟邮件无关但提到了邮件"/utf8>>, [])],
                              #{}),
    [First | _] = beamai_tool_index:search(H, <<"邮件"/utf8>>, 5),
    ?assertEqual(<<"发送邮件"/utf8>>, First).

%% 中文标点作边界，不混进词元
keyword_chinese_punctuation_is_boundary_test() ->
    H = beamai_tool_index:new(beamai_tool_index_keyword,
                              [tool(<<"p">>, <<"查询天气。发送邮件！"/utf8>>, [])], #{}),
    ?assertEqual([<<"p">>], beamai_tool_index:search(H, <<"天气"/utf8>>, 5)),
    ?assertEqual([<<"p">>], beamai_tool_index:search(H, <<"邮件"/utf8>>, 5)).

%% 指纹：SHA-256，32 字节
fingerprint_is_sha256_test() ->
    ?assertEqual(32, byte_size(beamai_tool_index:fingerprint(tools()))).

%% 指纹：与工具列表顺序无关
fingerprint_stable_under_reordering_test() ->
    ?assertEqual(beamai_tool_index:fingerprint(tools()),
                 beamai_tool_index:fingerprint(lists:reverse(tools()))).

%% 指纹：description 变化则指纹变化
fingerprint_changes_on_description_change_test() ->
    [T | Rest] = tools(),
    Changed = [T#{description => <<"totally different description">>} | Rest],
    ?assertNotEqual(beamai_tool_index:fingerprint(tools()),
                    beamai_tool_index:fingerprint(Changed)).

%% 指纹：分隔符防串扰 —— 字段拼接歧义不得碰撞
fingerprint_no_delimiter_collision_test() ->
    A = [tool(<<"ab">>, <<"c">>, [])],
    B = [tool(<<"a">>, <<"bc">>, [])],
    ?assertNotEqual(beamai_tool_index:fingerprint(A), beamai_tool_index:fingerprint(B)).

%% 指纹：空工具集也能算
fingerprint_empty_test() ->
    ?assertEqual(32, byte_size(beamai_tool_index:fingerprint([]))).
