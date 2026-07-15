%%%-------------------------------------------------------------------
%%% @doc 工具索引后端：BM25 关键词打分
%%%
%%% 把每个工具的 name + description + tag 拼成一篇「文档」，用 BM25
%%% 对查询词打分排序。相比 beamai_tool_index_regex 的命中/不命中二元判定，
%%% 这里给出连续相关度：命中稀有词（高 IDF）比命中「get」「the」这种
%%% 满场跑的词更值钱，短文档命中比长文档命中更值钱。
%%%
%%% **标识符切词是召回率的命门**：工具名是标识符不是自然语言，
%%% `get_weather` 与 `getWeather` 必须都切成 `[get, weather]`，否则模型
%%% 问「weather」时整个工具都召不回来。切词规则见 tokenize/1。
%%%
%%% **中文走二元组（bigram）切分**：中文词间无空格，而本项目的工具描述
%%% 以中文为主——中文召不回来等于这个后端废掉一半。连续 CJK 字符按相邻
%%% 二元组展开：`查询天气` → `["查询","询天","天气"]`。索引与查询共用同一个
%%% tokenize/1，故查询 `天气` 切出 `["天气"]`，与描述里的 `天气` 对上、召回成功。
%%% 这是 Lucene CJKAnalyzer 的做法，也是没有分词词典时的标准答案：一元组
%%% 召回过宽（「天」命中一切含天的词），二元组无需词典即有可用精度。
%%% CJK 自成字符类，与 ASCII 双向断开（`天气weather` → `天气` + `weather`）。
%%%
%%% 二元组的代价要写明：它**跨词边界过度生成**——`市的`、`的天` 并不是词，
%%% 却照样入索引，既拉低精度（无意义词元可能碰巧命中），又让索引变大
%%% （N 字的 run 产出 N-1 个词元，中文文档的 len 因此显著高于英文）。
%%% 它终究**不是真分词器**：切不出「天气预报」这样的完整词，也没有词性、
%%% 停用词、同义词。要真分词得换带词典的后端。
%%%
%%% 还有一处已知局限（同 Lucene CJKAnalyzer 缺省行为 outputUnigrams=false）：
%%% **单字查询命中不了多字文档**——`天气预报` 只出二元组，索引里没有 `天`
%%% 这个词元，故查 `天` 召回为空（查 `天气` 正常）。代价换来的是索引不翻倍、
%%% 精度不被单字噪声拖垮；中文单字本就歧义极大，作检索词价值有限。
%%% 单字 run 自身仍原样保留，故单字查询能召回单字命名的工具。
%%%
%%% **name 权重加成**：name 命中理应压过「仅 description 命中」，但
%%% 二者在同一篇文档里 tf 都是 1，裸 BM25 只能靠文档长度分高下——太脆。
%%% 故按 BM25F 的思路做字段加权：name 的词按 name_boost 重复计入
%%% （tf 与 dl 同步放大，仍是一篇自洽的文档）。
%%%
%%% 排序规则：分数降序；同分按 name 字典序（确定性）。零分文档剔除。
%%%
%%% Opts：
%%%   - k1         => number()      词频饱和系数，缺省 1.2
%%%   - b          => number()      文档长度归一化强度，缺省 0.75
%%%   - name_boost => pos_integer() name 字段权重，缺省 3
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_index_keyword).

-behaviour(beamai_tool_index).

%% beamai_tool_index 回调
-export([build/2, search/3]).

%% Types
-export_type([state/0]).

-define(DEFAULT_K1, 1.2).
-define(DEFAULT_B, 0.75).
-define(DEFAULT_NAME_BOOST, 3).

-type token() :: binary().

%% 切词 run：一段连续同类字符（word = 拉丁/数字，cjk = 中日韩文字）
-type run_class() :: word | cjk.
-type run() :: {run_class(), [char()]}.

-type doc() :: #{
    name := binary(),
    tf := #{token() => pos_integer()},
    len := non_neg_integer()
}.

-opaque state() :: #{
    docs := [doc()],
    idf := #{token() => float()},
    avgdl := float(),
    k1 := float(),
    b := float()
}.

%%====================================================================
%% beamai_tool_index 回调
%%====================================================================

%% @doc 构建索引：切词、统计词频，并预计算 IDF 与平均文档长度
%%
%% IDF 与 avgdl 是语料级全局量，只能在这里算——search/3 拿到的是单条
%% 查询，无从知晓语料分布。
-spec build([beamai_tool:tool_spec()], map()) -> state().
build(Tools, Opts) ->
    NameBoost = maps:get(name_boost, Opts, ?DEFAULT_NAME_BOOST),
    Docs = [build_doc(T, NameBoost) || T <- Tools],
    N = length(Docs),
    TotalLen = lists:sum([Len || #{len := Len} <- Docs]),
    AvgDl = case N of
        0 -> 0.0;
        _ -> TotalLen / N
    end,
    #{
        docs => Docs,
        idf => build_idf(Docs, N),
        avgdl => AvgDl,
        k1 => to_float(maps:get(k1, Opts, ?DEFAULT_K1)),
        b => to_float(maps:get(b, Opts, ?DEFAULT_B))
    }.

%% @doc 检索：BM25 打分，分数降序，剔除零分，至多 MaxResults 个
-spec search(binary(), pos_integer(), state()) -> [binary()].
search(_Query, _MaxResults, #{docs := []}) ->
    [];
search(Query, MaxResults, State) ->
    case lists:usort(tokenize(Query)) of
        %% 查询里没有任何可用词元（如纯标点）→ 无从打分
        [] ->
            [];
        QTokens ->
            #{docs := Docs} = State,
            Scored = lists:foldl(fun(Doc, Acc) ->
                case score(Doc, QTokens, State) of
                    Score when Score > 0.0 -> [{-Score, maps:get(name, Doc)} | Acc];
                    _ -> Acc
                end
            end, [], Docs),
            %% 取负分入元组：升序排列即分数降序，同分自然落到 name 字典序
            [Name || {_NegScore, Name} <- lists:sublist(lists:sort(Scored), MaxResults)]
    end.

%%====================================================================
%% 内部函数 - 建索引
%%====================================================================

%% @private 单个工具 → 文档（name 按 NameBoost 重复计入以加权）
-spec build_doc(beamai_tool:tool_spec(), pos_integer()) -> doc().
build_doc(Tool, NameBoost) ->
    Name = beamai_tool:get_name(Tool),
    NameTokens = lists:append(lists:duplicate(NameBoost, tokenize(Name))),
    Tokens = NameTokens
        ++ tokenize(maps:get(description, Tool, <<>>))
        ++ tag_tokens(beamai_tool:get_tag(Tool)),
    #{
        name => Name,
        tf => count_tokens(Tokens),
        len => length(Tokens)
    }.

%% @private tag 可为单个 binary、binary 列表或 undefined（见 beamai_tool:get_tag/1）
-spec tag_tokens(binary() | [binary()] | undefined) -> [token()].
tag_tokens(undefined) -> [];
tag_tokens(Tags) when is_list(Tags) -> lists:flatmap(fun tokenize/1, Tags);
tag_tokens(Tag) when is_binary(Tag) -> tokenize(Tag).

%% @private 词元列表 → 词频 map
-spec count_tokens([token()]) -> #{token() => pos_integer()}.
count_tokens(Tokens) ->
    lists:foldl(fun(Tok, Acc) ->
        maps:update_with(Tok, fun(C) -> C + 1 end, 1, Acc)
    end, #{}, Tokens).

%% @private 预计算各词元的 IDF（先数文档频率 DF，再逐个换算）
-spec build_idf([doc()], non_neg_integer()) -> #{token() => float()}.
build_idf(Docs, N) ->
    Df = lists:foldl(fun(#{tf := Tf}, Acc) ->
        %% 按文档去重计数：一篇文档里出现几次都只算一次
        maps:fold(fun(Tok, _Count, Inner) ->
            maps:update_with(Tok, fun(C) -> C + 1 end, 1, Inner)
        end, Acc, Tf)
    end, #{}, Docs),
    maps:map(fun(_Tok, DocFreq) -> idf(N, DocFreq) end, Df).

%% @private IDF（Lucene 式平滑：加 1 保证恒为正，避免高频词得负分反向排序）
-spec idf(non_neg_integer(), pos_integer()) -> float().
idf(N, DocFreq) ->
    math:log(1 + (N - DocFreq + 0.5) / (DocFreq + 0.5)).

%%====================================================================
%% 内部函数 - 打分
%%====================================================================

%% @private 文档对查询的 BM25 总分（各查询词得分之和）
-spec score(doc(), [token()], state()) -> float().
score(#{tf := Tf, len := Len}, QTokens, #{idf := Idf, avgdl := AvgDl, k1 := K1, b := B}) ->
    lists:foldl(fun(Tok, Acc) ->
        case maps:get(Tok, Tf, 0) of
            %% 文档里没这个词 → 该词贡献 0（顺带避开除法）
            0 -> Acc;
            Freq -> Acc + term_score(Freq, Len, maps:get(Tok, Idf, 0.0), AvgDl, K1, B)
        end
    end, 0.0, QTokens).

%% @private 单个词元的 BM25 得分：IDF * tf 饱和项 * 文档长度归一化
-spec term_score(pos_integer(), non_neg_integer(), float(), float(), float(), float()) -> float().
term_score(Freq, Len, Idf, AvgDl, K1, B) ->
    Norm = case AvgDl > 0.0 of
        true -> Len / AvgDl;
        false -> 1.0
    end,
    Idf * (Freq * (K1 + 1)) / (Freq + K1 * (1 - B + B * Norm)).

%%====================================================================
%% 内部函数 - 切词
%%====================================================================

%% @private 切词：两趟——先按字符类切 run，再按类展开词元，统一小写
%%
%% 第一趟 split_runs/1：把字符流切成连续同类 run（word / cjk），
%% 类间自动断开，标点等 other 类作边界并丢弃。
%% 第二趟 expand_run/1：word run 走 camelCase/snake_case 拆分，
%% cjk run 走二元组展开。
%%
%% 分两趟而非单趟穿线，是因为两类文字的切分规则毫无共性——
%% 前者靠大小写与分隔符找边界，后者根本没有边界可找、只能滑窗。
-spec tokenize(binary()) -> [token()].
tokenize(Bin) when is_binary(Bin) ->
    Chars = case unicode:characters_to_list(Bin) of
        L when is_list(L) -> L;
        _ -> binary_to_list(Bin)   %% 非法编码兜底：按字节切
    end,
    lists:flatmap(fun expand_run/1, split_runs(Chars));
tokenize(_) ->
    [].

%%--------------------------------------------------------------------
%% 第一趟：切 run
%%--------------------------------------------------------------------

%% @private 字符流 → 连续同类 run 列表（other 类丢弃且作边界）
-spec split_runs([char()]) -> [run()].
split_runs(Chars) ->
    lists:reverse(do_runs(Chars, none, [], [])).

%% @private 切 run 主循环（Cur 为当前 run 的逆序原始字符）
-spec do_runs([char()], run_class() | none, [char()], [run()]) -> [run()].
do_runs([], Class, Cur, Acc) ->
    close_run(Class, Cur, Acc);
do_runs([C | Rest], Class, Cur, Acc) ->
    case run_class(C) of
        %% 标点/空白：断开当前 run，字符本身丢弃
        other ->
            do_runs(Rest, none, [], close_run(Class, Cur, Acc));
        %% 同类续接
        Class ->
            do_runs(Rest, Class, [C | Cur], Acc);
        %% 换类（如 天气weather 的 cjk→word）：断开，另起新 run
        New ->
            do_runs(Rest, New, [C], close_run(Class, Cur, Acc))
    end.

%% @private 收束一个 run（空 run 丢弃）
-spec close_run(run_class() | none, [char()], [run()]) -> [run()].
close_run(_Class, [], Acc) -> Acc;
close_run(Class, Cur, Acc) -> [{Class, lists:reverse(Cur)} | Acc].

%% @private run 级字符分类
%%
%% CJK 自成一类，故与 ASCII 在**两个方向**上都断开（`天气weather` 切成
%% `天气` 与 `weather` 两个 run，而非糊成一坨）。
%% 非 ASCII 非 CJK（带音标拉丁、西里尔、希腊…）归 word：这些文字有空格
%% 分词，按 word 处理即可，不该套二元组。
-spec run_class(char()) -> run_class() | other.
%% CJK 标点与全角形式（。、！？「」／…）：边界，不入词
run_class(C) when C >= 16#3000, C =< 16#303F -> other;
run_class(C) when C >= 16#FF00, C =< 16#FF60 -> other;
run_class(C) when C >= 16#FFE0, C =< 16#FFEF -> other;
%% CJK 文字
run_class(C) when C >= 16#3040, C =< 16#30FF -> cjk;    %% 平假名 / 片假名
run_class(C) when C >= 16#3400, C =< 16#4DBF -> cjk;    %% CJK 扩展 A
run_class(C) when C >= 16#4E00, C =< 16#9FFF -> cjk;    %% CJK 统一表意文字（常用汉字全在此）
run_class(C) when C >= 16#AC00, C =< 16#D7AF -> cjk;    %% 谚文音节
run_class(C) when C >= 16#F900, C =< 16#FAFF -> cjk;    %% CJK 兼容表意文字
run_class(C) when C >= 16#FF61, C =< 16#FF9F -> cjk;    %% 半角片假名
run_class(C) when C >= 16#20000, C =< 16#2FA1F -> cjk;  %% CJK 扩展 B~F
%% ASCII 字母数字
run_class(C) when C >= $a, C =< $z -> word;
run_class(C) when C >= $A, C =< $Z -> word;
run_class(C) when C >= $0, C =< $9 -> word;
%% 其余非 ASCII：按词内字符
run_class(C) when C > 127 -> word;
run_class(_) -> other.

%%--------------------------------------------------------------------
%% 第二趟：展开 run
%%--------------------------------------------------------------------

%% @private run → 词元列表（按类分派）
-spec expand_run(run()) -> [token()].
expand_run({word, Chars}) -> lists:reverse(split_tokens(Chars, [], []));
expand_run({cjk, Chars}) -> bigrams(Chars).

%% @private CJK run → 相邻二元组
%%
%% 单字 run（如查询 `<<"天">>`）原样保留为一个词元——否则单字查询切不出
%% 任何词元，直接零召回。
-spec bigrams([char()]) -> [token()].
bigrams([C]) -> [lower_bin([C])];
bigrams(Chars) -> bigrams(Chars, []).

%% @private 二元组滑窗：N 字出 N-1 个词元
-spec bigrams([char()], [token()]) -> [token()].
bigrams([A, B | Rest], Acc) -> bigrams([B | Rest], [lower_bin([A, B]) | Acc]);
bigrams(_, Acc) -> lists:reverse(Acc).

%% @private word run 内的 camelCase / snake_case 拆分主循环
%%
%% 只吃 word run（CJK 已在第一趟分流走），故此处的规则一如既往：
%%   - 小写/数字 后接 大写 → 切开（`getWeather` → get, Weather）
%%   - 大写 后接 大写+小写 → 在末个大写前切开（`HTTPServer` → HTTP, Server）
%% Cur 为当前词的**逆序原始字符**，大小写留到 emit 时归一。
%% other 分支在 run 内已不可能触发（第一趟已滤净），保留作防御。
-spec split_tokens([char()], [char()], [token()]) -> [token()].
split_tokens([], Cur, Acc) ->
    emit(Cur, Acc);
split_tokens([C | Rest], Cur, Acc) ->
    case char_class(C) of
        other ->
            split_tokens(Rest, [], emit(Cur, Acc));
        upper ->
            case boundary_before_upper(Cur, Rest) of
                true -> split_tokens(Rest, [C], emit(Cur, Acc));
                false -> split_tokens(Rest, [C | Cur], Acc)
            end;
        _ ->
            split_tokens(Rest, [C | Cur], Acc)
    end.

%% @private 当前大写字符前是否该切开（Cur 为逆序，故其头部即前一字符）
-spec boundary_before_upper([char()], [char()]) -> boolean().
boundary_before_upper([], _Rest) ->
    %% 词首的大写不算边界
    false;
boundary_before_upper([Prev | _], Rest) ->
    case char_class(Prev) of
        %% getWeather / utf8Encode
        lower -> true;
        digit -> true;
        %% HTTPServer：仅当这个大写后面跟着小写时才切（HTTP|Server）
        upper ->
            case Rest of
                [Next | _] -> char_class(Next) =:= lower;
                [] -> false
            end;
        _ -> false
    end.

%% @private word run 内的字符分类（仅服务 camelCase 判定）
-spec char_class(char()) -> lower | upper | digit | other.
char_class(C) when C >= $a, C =< $z -> lower;
char_class(C) when C >= $A, C =< $Z -> upper;
char_class(C) when C >= $0, C =< $9 -> digit;
%% 非 ASCII 非 CJK（带音标拉丁、西里尔…；CJK 到不了这里，已在第一趟分流）：
%% 视作词内小写字符，不触发 camelCase 边界
char_class(C) when C > 127 -> lower;
char_class(_) -> other.

%% @private 收词：逆序字符还原、转小写、入结果（空词丢弃）
-spec emit([char()], [token()]) -> [token()].
emit([], Acc) ->
    Acc;
emit(Cur, Acc) ->
    [lower_bin(lists:reverse(Cur)) | Acc].

%% @private 字符列表 → 小写 binary
-spec lower_bin([char()]) -> binary().
lower_bin(Chars) ->
    case unicode:characters_to_binary(string:lowercase(Chars)) of
        Bin when is_binary(Bin) -> Bin;
        _ -> <<>>
    end.

%%====================================================================
%% 内部函数 - 杂项
%%====================================================================

%% @private Opts 里的系数允许写整数（如 k1 => 2），统一为 float
-spec to_float(number()) -> float().
to_float(N) when is_integer(N) -> float(N);
to_float(N) when is_float(N) -> N.
