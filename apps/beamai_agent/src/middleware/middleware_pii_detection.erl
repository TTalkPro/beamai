%%%-------------------------------------------------------------------
%%% @doc PII Detection Middleware - 敏感信息检测
%%%
%%% 检测并处理敏感个人信息（PII），支持多种处理策略。
%%%
%%% == 支持的 PII 类型 ==
%%%
%%% - email: 电子邮箱地址
%%% - phone: 电话号码（支持国际格式）
%%% - credit_card: 信用卡号码
%%% - ssn: 社会安全号码（美国）
%%% - id_card: 身份证号码（中国）
%%% - ip_address: IP 地址（IPv4/IPv6）
%%% - url: URL 地址
%%% - password: 密码关键词
%%%
%%% == 处理策略 ==
%%%
%%% - block: 阻止包含 PII 的消息
%%% - redact: 完全删除 PII
%%% - mask: 部分遮掩（如 ****@email.com）
%%% - hash: 哈希替换
%%% - warn: 仅警告，不修改
%%%
%%% == 配置示例 ==
%%%
%%% ```erlang
%%% {middleware_pii_detection, #{
%%%     %% 检测的 PII 类型
%%%     detect_types => [email, phone, credit_card, id_card],
%%%
%%%     %% 处理策略
%%%     strategy => mask,  %% block | redact | mask | hash | warn
%%%
%%%     %% 每种类型的自定义策略
%%%     type_strategies => #{
%%%         credit_card => block,
%%%         email => mask
%%%     },
%%%
%%%     %% 自定义检测器
%%%     custom_detectors => [
%%%         {api_key, <<"sk-[a-zA-Z0-9]{32,}">>}
%%%     ],
%%%
%%%     %% 检测位置
%%%     check_input => true,    %% 检查用户输入
%%%     check_output => true,   %% 检查模型输出
%%%
%%%     %% 回调
%%%     on_detect => fun(PIIType, Value, Location) -> ok,
%%%
%%%     %% 调试模式
%%%     debug => false
%%% }}
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_pii_detection).

-behaviour(beamai_middleware).

%% Middleware 回调
-export([init/1, before_model/2, after_model/2]).

%% 工具函数
-export([
    detect_pii/2,
    mask_pii/2,
    redact_pii/2,
    get_detection_stats/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

-type pii_type() :: email | phone | credit_card | ssn | id_card |
                    ip_address | url | password | atom().

-type strategy() :: block | redact | mask | hash | warn.

-type pii_match() :: #{
    type := pii_type(),
    value := binary(),
    start_pos := non_neg_integer(),
    end_pos := non_neg_integer()
}.

%%====================================================================
%% Middleware 回调
%%====================================================================

%% @doc 初始化 Middleware 状态
-spec init(map()) -> map().
init(Opts) ->
    #{
        detect_types => maps:get(detect_types, Opts, default_detect_types()),
        strategy => maps:get(strategy, Opts, warn),
        type_strategies => maps:get(type_strategies, Opts, #{}),
        custom_detectors => maps:get(custom_detectors, Opts, []),
        check_input => maps:get(check_input, Opts, true),
        check_output => maps:get(check_output, Opts, true),
        on_detect => maps:get(on_detect, Opts, undefined),
        debug => maps:get(debug, Opts, false)
    }.

%% @doc 模型调用前检查输入
-spec before_model(map(), map()) -> beamai_middleware:middleware_result().
before_model(State, MwState) ->
    #{check_input := CheckInput} = MwState,

    case CheckInput of
        false ->
            ok;
        true ->
            Messages = graph:get(State, messages, []),
            case Messages of
                [] ->
                    ok;
                _ ->
                    %% 检查最后一条用户消息
                    LastMsg = lists:last(Messages),
                    check_message(LastMsg, input, State, MwState)
            end
    end.

%% @doc 模型调用后检查输出
-spec after_model(map(), map()) -> beamai_middleware:middleware_result().
after_model(State, MwState) ->
    #{check_output := CheckOutput} = MwState,

    case CheckOutput of
        false ->
            ok;
        true ->
            Response = graph:get(State, llm_response, undefined),
            case Response of
                undefined ->
                    ok;
                _ ->
                    Content = extract_response_content(Response),
                    check_content(Content, output, State, MwState)
            end
    end.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 检测文本中的 PII
-spec detect_pii(binary(), map()) -> [pii_match()].
detect_pii(Text, MwState) ->
    #{detect_types := DetectTypes, custom_detectors := CustomDetectors} = MwState,

    %% 内置检测器
    BuiltinMatches = lists:flatmap(fun(Type) ->
        detect_type(Text, Type)
    end, DetectTypes),

    %% 自定义检测器
    CustomMatches = lists:flatmap(fun({Type, Pattern}) ->
        detect_custom(Text, Type, Pattern)
    end, CustomDetectors),

    BuiltinMatches ++ CustomMatches.

%% @doc 遮掩 PII
-spec mask_pii(binary(), [pii_match()]) -> binary().
mask_pii(Text, Matches) ->
    %% 按位置降序排序，从后往前替换
    SortedMatches = lists:sort(fun(A, B) ->
        maps:get(start_pos, A) > maps:get(start_pos, B)
    end, Matches),

    lists:foldl(fun(Match, Acc) ->
        #{type := Type, value := Value, start_pos := Start, end_pos := End} = Match,
        MaskedValue = mask_value(Type, Value),
        <<Before:Start/binary, _:((End - Start))/binary, After/binary>> = Acc,
        <<Before/binary, MaskedValue/binary, After/binary>>
    end, Text, SortedMatches).

%% @doc 删除 PII
-spec redact_pii(binary(), [pii_match()]) -> binary().
redact_pii(Text, Matches) ->
    SortedMatches = lists:sort(fun(A, B) ->
        maps:get(start_pos, A) > maps:get(start_pos, B)
    end, Matches),

    lists:foldl(fun(Match, Acc) ->
        #{type := Type, start_pos := Start, end_pos := End} = Match,
        Redacted = iolist_to_binary(["[", atom_to_list(Type), "_REDACTED]"]),
        <<Before:Start/binary, _:((End - Start))/binary, After/binary>> = Acc,
        <<Before/binary, Redacted/binary, After/binary>>
    end, Text, SortedMatches).

%% @doc 获取检测统计
-spec get_detection_stats(map()) -> map().
get_detection_stats(State) ->
    #{
        total_detections => graph:get(State, mw_pii_total_detections, 0),
        blocked_count => graph:get(State, mw_pii_blocked_count, 0),
        by_type => graph:get(State, mw_pii_by_type, #{})
    }.

%%====================================================================
%% 内部函数 - 检测
%%====================================================================

%% @private 默认检测类型
-spec default_detect_types() -> [pii_type()].
default_detect_types() ->
    [email, phone, credit_card, id_card, ip_address].

%% @private 检测特定类型的 PII
-spec detect_type(binary(), pii_type()) -> [pii_match()].
detect_type(Text, email) ->
    Pattern = "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}",
    find_matches(Text, email, Pattern);

detect_type(Text, phone) ->
    %% 支持多种电话格式
    Pattern = "(?:\\+?\\d{1,3}[-.\\s]?)?(?:\\(?\\d{2,4}\\)?[-.\\s]?)?\\d{3,4}[-.\\s]?\\d{4}",
    find_matches(Text, phone, Pattern);

detect_type(Text, credit_card) ->
    %% 信用卡号（13-19位数字，可能有分隔符）
    Pattern = "\\b(?:\\d{4}[-.\\s]?){3,4}\\d{1,4}\\b",
    find_matches(Text, credit_card, Pattern);

detect_type(Text, ssn) ->
    %% 美国社会安全号码
    Pattern = "\\b\\d{3}-\\d{2}-\\d{4}\\b",
    find_matches(Text, ssn, Pattern);

detect_type(Text, id_card) ->
    %% 中国身份证号码（18位或15位）
    Pattern = "\\b[1-9]\\d{5}(?:18|19|20)\\d{2}(?:0[1-9]|1[0-2])(?:0[1-9]|[12]\\d|3[01])\\d{3}[\\dXx]\\b",
    find_matches(Text, id_card, Pattern);

detect_type(Text, ip_address) ->
    %% IPv4 地址
    Pattern = "\\b(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\\b",
    find_matches(Text, ip_address, Pattern);

detect_type(Text, url) ->
    Pattern = "https?://[^\\s<>\"{}|\\\\^`\\[\\]]+",
    find_matches(Text, url, Pattern);

detect_type(Text, password) ->
    %% 检测密码相关的模式
    Pattern = "(?:password|passwd|pwd|secret|token|api_key|apikey)\\s*[:=]\\s*[^\\s]+",
    find_matches(Text, password, Pattern);

detect_type(_Text, _Type) ->
    [].

%% @private 自定义检测器
-spec detect_custom(binary(), atom(), binary() | string()) -> [pii_match()].
detect_custom(Text, Type, Pattern) when is_binary(Pattern) ->
    detect_custom(Text, Type, binary_to_list(Pattern));
detect_custom(Text, Type, Pattern) ->
    find_matches(Text, Type, Pattern).

%% @private 查找匹配
-spec find_matches(binary(), pii_type(), string()) -> [pii_match()].
find_matches(Text, Type, Pattern) ->
    case re:compile(Pattern, [caseless]) of
        {ok, MP} ->
            find_all_matches(Text, Type, MP, 0, []);
        {error, _} ->
            []
    end.

%% @private 递归查找所有匹配
-spec find_all_matches(binary(), pii_type(), term(), non_neg_integer(), [pii_match()]) ->
    [pii_match()].
find_all_matches(Text, Type, MP, Offset, Acc) ->
    case re:run(Text, MP, [{offset, Offset}, {capture, first, index}]) of
        {match, [{Start, Len}]} ->
            Value = binary:part(Text, Start, Len),
            Match = #{
                type => Type,
                value => Value,
                start_pos => Start,
                end_pos => Start + Len
            },
            find_all_matches(Text, Type, MP, Start + Len, [Match | Acc]);
        nomatch ->
            lists:reverse(Acc)
    end.

%%====================================================================
%% 内部函数 - 处理
%%====================================================================

%% @private 检查消息
-spec check_message(map(), atom(), map(), map()) -> beamai_middleware:middleware_result().
check_message(#{<<"content">> := Content}, Location, State, MwState) when is_binary(Content) ->
    check_content(Content, Location, State, MwState);
check_message(#{content := Content}, Location, State, MwState) when is_binary(Content) ->
    check_content(Content, Location, State, MwState);
check_message(_, _, _, _) ->
    ok.

%% @private 检查内容
-spec check_content(binary(), atom(), map(), map()) -> beamai_middleware:middleware_result().
check_content(Content, Location, State, MwState) ->
    #{strategy := DefaultStrategy,
      type_strategies := TypeStrategies,
      on_detect := OnDetect,
      debug := Debug} = MwState,

    Matches = detect_pii(Content, MwState),

    case Matches of
        [] ->
            ok;
        _ ->
            %% 调用检测回调
            lists:foreach(fun(#{type := Type, value := Value}) ->
                maybe_call_on_detect(OnDetect, Type, Value, Location)
            end, Matches),

            %% 调试日志
            case Debug of
                true ->
                    logger:info("[PIIDetection] 检测到 ~p 处 PII，位置: ~p",
                               [length(Matches), Location]);
                false ->
                    ok
            end,

            %% 更新统计
            NewTotal = graph:get(State, mw_pii_total_detections, 0) + length(Matches),
            ByType = update_type_stats(Matches, graph:get(State, mw_pii_by_type, #{})),

            %% 根据策略处理
            handle_matches(Matches, Content, Location, DefaultStrategy,
                          TypeStrategies, State, NewTotal, ByType)
    end.

%% @private 处理匹配
-spec handle_matches([pii_match()], binary(), atom(), strategy(), map(),
                     map(), non_neg_integer(), map()) ->
    beamai_middleware:middleware_result().
handle_matches(Matches, Content, Location, DefaultStrategy, TypeStrategies,
               State, NewTotal, ByType) ->
    %% 检查是否有任何匹配需要阻止
    ShouldBlock = lists:any(fun(#{type := Type}) ->
        Strategy = maps:get(Type, TypeStrategies, DefaultStrategy),
        Strategy =:= block
    end, Matches),

    case ShouldBlock of
        true ->
            %% 阻止消息
            BlockedCount = graph:get(State, mw_pii_blocked_count, 0) + 1,
            {halt, {pii_detected, #{
                location => Location,
                types => [maps:get(type, M) || M <- Matches],
                blocked_count => BlockedCount
            }}};
        false ->
            %% 处理内容（mask/redact/hash）
            ProcessedContent = process_content(Content, Matches, DefaultStrategy, TypeStrategies),

            Updates = #{
                mw_pii_total_detections => NewTotal,
                mw_pii_by_type => ByType
            },

            case Location of
                input ->
                    %% 更新消息
                    Messages = graph:get(State, messages, []),
                    case Messages of
                        [] ->
                            {update, Updates};
                        _ ->
                            LastMsg = lists:last(Messages),
                            UpdatedMsg = update_message_content(LastMsg, ProcessedContent),
                            NewMessages = lists:droplast(Messages) ++ [UpdatedMsg],
                            {update, Updates#{messages => NewMessages}}
                    end;
                output ->
                    %% 更新响应
                    Response = graph:get(State, llm_response, undefined),
                    UpdatedResponse = update_response_content(Response, ProcessedContent),
                    {update, Updates#{llm_response => UpdatedResponse}}
            end
    end.

%% @private 处理内容
-spec process_content(binary(), [pii_match()], strategy(), map()) -> binary().
process_content(Content, Matches, DefaultStrategy, TypeStrategies) ->
    lists:foldl(fun(Match, Acc) ->
        #{type := Type} = Match,
        Strategy = maps:get(Type, TypeStrategies, DefaultStrategy),
        apply_strategy(Acc, Match, Strategy)
    end, Content, lists:reverse(Matches)).

%% @private 应用策略
-spec apply_strategy(binary(), pii_match(), strategy()) -> binary().
apply_strategy(Content, _Match, warn) ->
    Content;
apply_strategy(Content, Match, mask) ->
    mask_single(Content, Match);
apply_strategy(Content, Match, redact) ->
    redact_single(Content, Match);
apply_strategy(Content, Match, hash) ->
    hash_single(Content, Match);
apply_strategy(Content, _Match, block) ->
    Content.

%% @private 遮掩值
-spec mask_value(pii_type(), binary()) -> binary().
mask_value(email, Value) ->
    case binary:split(Value, <<"@">>) of
        [Local, Domain] ->
            MaskedLocal = mask_string(Local, 2),
            <<MaskedLocal/binary, "@", Domain/binary>>;
        _ ->
            mask_string(Value, 3)
    end;
mask_value(phone, Value) ->
    Len = byte_size(Value),
    case Len > 4 of
        true ->
            Last4 = binary:part(Value, Len - 4, 4),
            Stars = binary:copy(<<"*">>, Len - 4),
            <<Stars/binary, Last4/binary>>;
        false ->
            <<"****">>
    end;
mask_value(credit_card, Value) ->
    %% 只显示最后4位
    Clean = re:replace(Value, "[^0-9]", "", [global, {return, binary}]),
    Len = byte_size(Clean),
    case Len >= 4 of
        true ->
            Last4 = binary:part(Clean, Len - 4, 4),
            <<"****-****-****-", Last4/binary>>;
        false ->
            <<"****-****-****-****">>
    end;
mask_value(id_card, Value) ->
    Len = byte_size(Value),
    case Len >= 4 of
        true ->
            First4 = binary:part(Value, 0, 4),
            Last4 = binary:part(Value, Len - 4, 4),
            Stars = binary:copy(<<"*">>, Len - 8),
            <<First4/binary, Stars/binary, Last4/binary>>;
        false ->
            binary:copy(<<"*">>, Len)
    end;
mask_value(ip_address, _Value) ->
    <<"***.***.***.***">>;
mask_value(_Type, Value) ->
    mask_string(Value, 3).

%% @private 遮掩字符串
-spec mask_string(binary(), non_neg_integer()) -> binary().
mask_string(Value, KeepChars) ->
    Len = byte_size(Value),
    case Len > KeepChars * 2 of
        true ->
            First = binary:part(Value, 0, KeepChars),
            Last = binary:part(Value, Len - KeepChars, KeepChars),
            Stars = binary:copy(<<"*">>, Len - KeepChars * 2),
            <<First/binary, Stars/binary, Last/binary>>;
        false ->
            binary:copy(<<"*">>, Len)
    end.

%% @private 单个遮掩
-spec mask_single(binary(), pii_match()) -> binary().
mask_single(Content, #{type := Type, value := Value, start_pos := Start, end_pos := End}) ->
    MaskedValue = mask_value(Type, Value),
    <<Before:Start/binary, _:((End - Start))/binary, After/binary>> = Content,
    <<Before/binary, MaskedValue/binary, After/binary>>.

%% @private 单个删除
-spec redact_single(binary(), pii_match()) -> binary().
redact_single(Content, #{type := Type, start_pos := Start, end_pos := End}) ->
    Redacted = iolist_to_binary(["[", atom_to_list(Type), "_REDACTED]"]),
    <<Before:Start/binary, _:((End - Start))/binary, After/binary>> = Content,
    <<Before/binary, Redacted/binary, After/binary>>.

%% @private 单个哈希
-spec hash_single(binary(), pii_match()) -> binary().
hash_single(Content, #{type := Type, value := Value, start_pos := Start, end_pos := End}) ->
    Hash = crypto:hash(sha256, Value),
    HashHex = binary:encode_hex(binary:part(Hash, 0, 8)),
    Hashed = iolist_to_binary(["[", atom_to_list(Type), "_", HashHex, "]"]),
    <<Before:Start/binary, _:((End - Start))/binary, After/binary>> = Content,
    <<Before/binary, Hashed/binary, After/binary>>.

%% @private 更新类型统计
-spec update_type_stats([pii_match()], map()) -> map().
update_type_stats(Matches, Stats) ->
    lists:foldl(fun(#{type := Type}, Acc) ->
        Count = maps:get(Type, Acc, 0),
        Acc#{Type => Count + 1}
    end, Stats, Matches).

%% @private 提取响应内容
-spec extract_response_content(map()) -> binary().
extract_response_content(#{<<"content">> := Content}) when is_binary(Content) ->
    Content;
extract_response_content(#{content := Content}) when is_binary(Content) ->
    Content;
extract_response_content(#{<<"content">> := ContentList}) when is_list(ContentList) ->
    extract_text_from_content_list(ContentList);
extract_response_content(_) ->
    <<>>.

%% @private 从内容列表提取文本
-spec extract_text_from_content_list([map()]) -> binary().
extract_text_from_content_list(ContentList) ->
    Texts = lists:filtermap(fun
        (#{<<"type">> := <<"text">>, <<"text">> := Text}) -> {true, Text};
        (#{type := text, text := Text}) -> {true, Text};
        (_) -> false
    end, ContentList),
    iolist_to_binary(Texts).

%% @private 更新消息内容
-spec update_message_content(map(), binary()) -> map().
update_message_content(#{<<"content">> := _} = Msg, NewContent) ->
    Msg#{<<"content">> => NewContent};
update_message_content(#{content := _} = Msg, NewContent) ->
    Msg#{content => NewContent};
update_message_content(Msg, _) ->
    Msg.

%% @private 更新响应内容
-spec update_response_content(map(), binary()) -> map().
update_response_content(#{<<"content">> := ContentList} = Response, NewContent)
  when is_list(ContentList) ->
    %% 替换第一个文本块
    NewContentList = update_first_text_block(ContentList, NewContent),
    Response#{<<"content">> => NewContentList};
update_response_content(#{<<"content">> := _} = Response, NewContent) ->
    Response#{<<"content">> => NewContent};
update_response_content(#{content := _} = Response, NewContent) ->
    Response#{content => NewContent};
update_response_content(Response, _) ->
    Response.

%% @private 更新第一个文本块
-spec update_first_text_block([map()], binary()) -> [map()].
update_first_text_block([], _NewContent) ->
    [];
update_first_text_block([#{<<"type">> := <<"text">>} = Block | Rest], NewContent) ->
    [Block#{<<"text">> => NewContent} | Rest];
update_first_text_block([Block | Rest], NewContent) ->
    [Block | update_first_text_block(Rest, NewContent)].

%% @private 调用检测回调
-spec maybe_call_on_detect(function() | undefined, pii_type(), binary(), atom()) -> ok.
maybe_call_on_detect(undefined, _, _, _) ->
    ok;
maybe_call_on_detect(OnDetect, Type, Value, Location) when is_function(OnDetect, 3) ->
    try
        OnDetect(Type, Value, Location)
    catch
        _:Reason ->
            logger:warning("[PIIDetection] 检测回调异常: ~p", [Reason])
    end,
    ok.
