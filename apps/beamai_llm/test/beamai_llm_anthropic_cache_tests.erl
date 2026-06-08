%%%-------------------------------------------------------------------
%%% @doc Anthropic Prompt 缓存与内置 Web Search 单元测试
%%%
%%% 覆盖：
%%%   - cache_control 注入策略（system_only / tools_only / system_and_tools /
%%%     conversation / none），含 TTL
%%%   - 内置 Web Search 工具注入
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_llm_anthropic_cache_tests).

-include_lib("eunit/include/eunit.hrl").

-define(SYS_MSG, #{role => system, content => <<"你是助手"/utf8>>}).
-define(USER_MSG, #{role => user, content => <<"hi">>}).
-define(TOOL, #{name => <<"get_weather">>, description => <<"天气"/utf8>>,
                parameters => #{<<"type">> => <<"object">>, <<"properties">> => #{}}}).

build(Config, Request) ->
    beamai_llm_provider_anthropic:build_request_body(Config, Request).

%%====================================================================
%% 默认（无缓存）
%%====================================================================

no_cache_by_default_test() ->
    Body = build(#{}, #{messages => [?SYS_MSG, ?USER_MSG]}),
    %% system 为纯字符串，未注入 cache_control
    ?assertEqual(<<"你是助手"/utf8>>, maps:get(<<"system">>, Body)).

cache_none_explicit_test() ->
    Body = build(#{cache_control => none}, #{messages => [?SYS_MSG, ?USER_MSG]}),
    ?assert(is_binary(maps:get(<<"system">>, Body))).

%%====================================================================
%% system 缓存
%%====================================================================

system_only_caches_system_test() ->
    Body = build(#{cache_control => system_only}, #{messages => [?SYS_MSG, ?USER_MSG]}),
    [SysBlock] = maps:get(<<"system">>, Body),
    ?assertEqual(<<"text">>, maps:get(<<"type">>, SysBlock)),
    ?assertEqual(<<"你是助手"/utf8>>, maps:get(<<"text">>, SysBlock)),
    ?assertEqual(#{<<"type">> => <<"ephemeral">>}, maps:get(<<"cache_control">>, SysBlock)).

system_only_does_not_cache_tools_test() ->
    Body = build(#{cache_control => system_only},
                 #{messages => [?SYS_MSG, ?USER_MSG], tools => [?TOOL]}),
    [Tool] = maps:get(<<"tools">>, Body),
    ?assertNot(maps:is_key(<<"cache_control">>, Tool)).

%%====================================================================
%% tools 缓存
%%====================================================================

tools_only_caches_last_tool_test() ->
    T2 = #{name => <<"calc">>, description => <<"计算"/utf8>>,
           parameters => #{<<"type">> => <<"object">>, <<"properties">> => #{}}},
    Body = build(#{cache_control => tools_only},
                 #{messages => [?USER_MSG], tools => [?TOOL, T2]}),
    [First, Last] = maps:get(<<"tools">>, Body),
    ?assertNot(maps:is_key(<<"cache_control">>, First)),
    ?assertEqual(#{<<"type">> => <<"ephemeral">>}, maps:get(<<"cache_control">>, Last)).

%%====================================================================
%% system_and_tools 同时缓存
%%====================================================================

system_and_tools_caches_both_test() ->
    Body = build(#{cache_control => system_and_tools},
                 #{messages => [?SYS_MSG, ?USER_MSG], tools => [?TOOL]}),
    [SysBlock] = maps:get(<<"system">>, Body),
    ?assert(maps:is_key(<<"cache_control">>, SysBlock)),
    [Tool] = maps:get(<<"tools">>, Body),
    ?assert(maps:is_key(<<"cache_control">>, Tool)).

%%====================================================================
%% conversation 缓存（最后一条消息末块）
%%====================================================================

conversation_caches_last_message_test() ->
    Body = build(#{cache_control => conversation},
                 #{messages => [?USER_MSG,
                                #{role => assistant, content => <<"a">>},
                                #{role => user, content => <<"再说"/utf8>>}]}),
    Messages = maps:get(<<"messages">>, Body),
    Last = lists:last(Messages),
    %% 最后一条消息 content 被转为带 cache_control 的 block 列表
    [Block] = maps:get(<<"content">>, Last),
    ?assertEqual(<<"再说"/utf8>>, maps:get(<<"text">>, Block)),
    ?assertEqual(#{<<"type">> => <<"ephemeral">>}, maps:get(<<"cache_control">>, Block)),
    %% 前面的消息不受影响（仍为字符串）
    First = hd(Messages),
    ?assert(is_binary(maps:get(<<"content">>, First))).

%%====================================================================
%% TTL 支持
%%====================================================================

cache_with_ttl_test() ->
    Body = build(#{cache_control => #{strategy => system_only, ttl => <<"1h">>}},
                 #{messages => [?SYS_MSG, ?USER_MSG]}),
    [SysBlock] = maps:get(<<"system">>, Body),
    ?assertEqual(#{<<"type">> => <<"ephemeral">>, <<"ttl">> => <<"1h">>},
                 maps:get(<<"cache_control">>, SysBlock)).

%%====================================================================
%% Web Search 工具注入
%%====================================================================

web_search_enabled_test() ->
    Body = build(#{web_search => true}, #{messages => [?USER_MSG]}),
    [Tool] = maps:get(<<"tools">>, Body),
    ?assertEqual(<<"web_search_20250305">>, maps:get(<<"type">>, Tool)),
    ?assertEqual(<<"web_search">>, maps:get(<<"name">>, Tool)).

web_search_with_options_test() ->
    Body = build(#{web_search => #{max_uses => 3, allowed_domains => [<<"docs.x.com">>]}},
                 #{messages => [?USER_MSG]}),
    [Tool] = maps:get(<<"tools">>, Body),
    ?assertEqual(3, maps:get(<<"max_uses">>, Tool)),
    ?assertEqual([<<"docs.x.com">>], maps:get(<<"allowed_domains">>, Tool)).

web_search_coexists_with_user_tools_test() ->
    Body = build(#{web_search => true},
                 #{messages => [?USER_MSG], tools => [?TOOL]}),
    Tools = maps:get(<<"tools">>, Body),
    ?assertEqual(2, length(Tools)),
    Types = [maps:get(<<"type">>, T, maps:get(<<"name">>, T)) || T <- Tools],
    ?assert(lists:member(<<"web_search_20250305">>, Types)).

web_search_disabled_no_tool_test() ->
    Body = build(#{web_search => false}, #{messages => [?USER_MSG]}),
    ?assertNot(maps:is_key(<<"tools">>, Body)).
