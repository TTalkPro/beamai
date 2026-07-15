%%%-------------------------------------------------------------------
%%% @doc 工具索引后端：正则 / 子串匹配
%%%
%%% 把查询当正则去匹配工具的 name 与 description，最朴素也最可预期的
%%% 后端——无打分模型、无语料统计，命中即入选，适合工具集不大或查询
%%% 由人（而非模型）给出的场景。想要词频相关度请用 beamai_tool_index_keyword。
%%%
%%% 排序规则（二级，全确定性）：
%%%   1. name 命中优先于「仅 description 命中」；
%%%   2. 同级按 name 字典序——保证同一查询每次返回同一顺序。
%%%
%%% **非法正则降级为字面子串匹配**：Query 由 LLM 生成，`<<"[unclosed">>`
%%% 这类输入是常态而非异常。此处不崩、不报错，退化成把查询当普通字符串
%%% 找子串——对模型来说「找不到」远好于「工具调用炸了」。
%%%
%%% Opts：
%%%   - case_sensitive => boolean()  区分大小写，缺省 false
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_index_regex).

-behaviour(beamai_tool_index).

%% beamai_tool_index 回调
-export([build/2, search/3]).

%% Types
-export_type([state/0]).

-type entry() :: {Name :: binary(), Desc :: binary()}.

-opaque state() :: #{
    entries := [entry()],
    case_sensitive := boolean()
}.

%% 命中等级：0 = name 命中，1 = 仅 description 命中
-type rank() :: 0 | 1.

%%====================================================================
%% beamai_tool_index 回调
%%====================================================================

%% @doc 构建索引：仅抽取 name/description，无预计算
-spec build([beamai_tool:tool_spec()], map()) -> state().
build(Tools, Opts) ->
    #{
        entries => [{beamai_tool:get_name(T), maps:get(description, T, <<>>)} || T <- Tools],
        case_sensitive => maps:get(case_sensitive, Opts, false)
    }.

%% @doc 检索：匹配 name/description，name 命中优先，同级按 name 排序
-spec search(binary(), pos_integer(), state()) -> [binary()].
search(_Query, _MaxResults, #{entries := []}) ->
    [];
search(Query, MaxResults, #{entries := Entries, case_sensitive := CaseSensitive}) ->
    Match = matcher(Query, CaseSensitive),
    Ranked = lists:foldl(fun({Name, Desc}, Acc) ->
        case rank(Match, Name, Desc) of
            nomatch -> Acc;
            Rank -> [{Rank, Name} | Acc]
        end
    end, [], Entries),
    %% {Rank, Name} 元组序即「先按命中等级、再按名字」的目标序
    [Name || {_Rank, Name} <- lists:sublist(lists:sort(Ranked), MaxResults)].

%%====================================================================
%% 内部函数 - 排序
%%====================================================================

%% @private 定级：name 命中 → 0；仅 description 命中 → 1；都不中 → nomatch
-spec rank(fun((binary()) -> boolean()), binary(), binary()) -> rank() | nomatch.
rank(Match, Name, Desc) ->
    case Match(Name) of
        true ->
            0;
        false ->
            case Match(Desc) of
                true -> 1;
                false -> nomatch
            end
    end.

%%====================================================================
%% 内部函数 - 匹配器
%%====================================================================

%% @private 构造匹配器：优先编译为正则，失败则降级字面子串
-spec matcher(binary(), boolean()) -> fun((binary()) -> boolean()).
matcher(Query, CaseSensitive) ->
    Opts = case CaseSensitive of
        true -> [unicode];
        false -> [unicode, caseless]
    end,
    case compile(Query, Opts) of
        {ok, MP} -> fun(Subject) -> regex_match(MP, Subject) end;
        error -> literal_matcher(Query, CaseSensitive)
    end.

%% @private 编译正则；非法模式（乃至非法编码）一律归为 error，绝不外泄异常
-spec compile(binary(), [term()]) -> {ok, re:mp()} | error.
compile(Query, Opts) ->
    try re:compile(Query, Opts) of
        {ok, MP} -> {ok, MP};
        {error, _} -> error
    catch
        _:_ -> error
    end.

%% @private 跑正则；主体非法编码等异常视为不匹配
-spec regex_match(re:mp(), binary()) -> boolean().
regex_match(MP, Subject) ->
    try re:run(Subject, MP, [{capture, none}]) of
        match -> true;
        nomatch -> false
    catch
        _:_ -> false
    end.

%% @private 字面子串匹配器（正则编译失败时的降级路径）
-spec literal_matcher(binary(), boolean()) -> fun((binary()) -> boolean()).
literal_matcher(<<>>, _CaseSensitive) ->
    %% 空模式：binary:match/2 对空 pattern 会 badarg，直接视为全命中
    fun(_Subject) -> true end;
literal_matcher(Query, true) ->
    fun(Subject) -> binary:match(Subject, Query) =/= nomatch end;
literal_matcher(Query, false) ->
    Lowered = lower(Query),
    fun(Subject) -> binary:match(lower(Subject), Lowered) =/= nomatch end.

%% @private 转小写（Unicode 感知；非法编码原样返回）
-spec lower(binary()) -> binary().
lower(Bin) ->
    try unicode:characters_to_binary(string:lowercase(Bin)) of
        Lowered when is_binary(Lowered) -> Lowered;
        _ -> Bin
    catch
        _:_ -> Bin
    end.
