%%%-------------------------------------------------------------------
%%% @doc API Key 认证单元测试
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_auth_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% 测试设置
%%====================================================================

setup() ->
    %% 启动认证服务
    {ok, Pid} = beamai_a2a_auth:start_link(#{}),
    Pid.

cleanup(Pid) ->
    %% 停止服务
    case is_process_alive(Pid) of
        true -> beamai_a2a_auth:stop();
        false -> ok
    end.

%%====================================================================
%% 测试套件
%%====================================================================

auth_test_() ->
    {foreach,
     fun setup/0,
     fun cleanup/1,
     [
        {"添加 API Key 测试", fun test_add_key/0},
        {"自动生成 API Key 测试", fun test_add_key_auto_generate/0},
        {"删除 API Key 测试", fun test_remove_key/0},
        {"获取 API Key 测试", fun test_get_key/0},
        {"列出 API Keys 测试", fun test_list_keys/0},
        {"更新 API Key 测试", fun test_update_key/0},
        {"验证 API Key 测试", fun test_validate_key/0},
        {"无效 API Key 测试", fun test_invalid_key/0},
        {"过期 API Key 测试", fun test_expired_key/0},
        {"统计信息测试", fun test_stats/0}
     ]}.

%%====================================================================
%% Key 管理测试
%%====================================================================

test_add_key() ->
    ApiKey = <<"test-api-key-1">>,
    Metadata = #{
        name => <<"Test Key">>,
        permissions => [read, write]
    },

    %% 添加 Key
    {ok, KeyInfo} = beamai_a2a_auth:add_key(ApiKey, Metadata),

    %% 验证返回的信息
    ?assertEqual(ApiKey, maps:get(key, KeyInfo)),
    ?assertEqual(<<"Test Key">>, maps:get(name, KeyInfo)),
    ?assertEqual([read, write], maps:get(permissions, KeyInfo)),
    ?assert(maps:is_key(created_at, KeyInfo)),

    ok.

test_add_key_auto_generate() ->
    Metadata = #{name => <<"Auto Generated Key">>},

    %% 自动生成 Key
    {ok, KeyInfo} = beamai_a2a_auth:add_key(Metadata),

    %% 验证 Key 格式
    Key = maps:get(key, KeyInfo),
    ?assert(is_binary(Key)),
    ?assert(byte_size(Key) > 10),
    %% 应该以 ak_ 开头
    ?assertEqual(<<"ak_">>, binary:part(Key, 0, 3)),

    ok.

test_remove_key() ->
    ApiKey = <<"test-key-to-remove">>,

    %% 添加 Key
    {ok, _} = beamai_a2a_auth:add_key(ApiKey, #{name => <<"Remove Test">>}),

    %% 验证存在
    {ok, _} = beamai_a2a_auth:get_key(ApiKey),

    %% 删除 Key
    ok = beamai_a2a_auth:remove_key(ApiKey),

    %% 验证不存在
    {error, not_found} = beamai_a2a_auth:get_key(ApiKey),

    ok.

test_get_key() ->
    ApiKey = <<"test-key-get">>,
    Metadata = #{
        name => <<"Get Test Key">>,
        permissions => all,
        metadata => #{env => <<"test">>}
    },

    %% 添加 Key
    {ok, _} = beamai_a2a_auth:add_key(ApiKey, Metadata),

    %% 获取 Key
    {ok, KeyInfo} = beamai_a2a_auth:get_key(ApiKey),

    ?assertEqual(ApiKey, maps:get(key, KeyInfo)),
    ?assertEqual(<<"Get Test Key">>, maps:get(name, KeyInfo)),
    ?assertEqual(all, maps:get(permissions, KeyInfo)),

    ok.

test_list_keys() ->
    %% 添加多个 Keys
    {ok, _} = beamai_a2a_auth:add_key(<<"list-key-1">>, #{name => <<"Key 1">>}),
    {ok, _} = beamai_a2a_auth:add_key(<<"list-key-2">>, #{name => <<"Key 2">>}),
    {ok, _} = beamai_a2a_auth:add_key(<<"list-key-3">>, #{name => <<"Key 3">>}),

    %% 列出 Keys
    Keys = beamai_a2a_auth:list_keys(),

    ?assert(length(Keys) >= 3),

    %% 验证 Key 被遮盖
    lists:foreach(fun(K) ->
        MaskedKey = maps:get(key, K),
        ?assert(binary:match(MaskedKey, <<"***">>) =/= nomatch)
    end, Keys),

    ok.

test_update_key() ->
    ApiKey = <<"test-key-update">>,

    %% 添加 Key
    {ok, _} = beamai_a2a_auth:add_key(ApiKey, #{
        name => <<"Original Name">>,
        permissions => [read]
    }),

    %% 更新 Key
    ok = beamai_a2a_auth:update_key(ApiKey, #{
        name => <<"Updated Name">>,
        permissions => [read, write, admin]
    }),

    %% 验证更新
    {ok, KeyInfo} = beamai_a2a_auth:get_key(ApiKey),
    ?assertEqual(<<"Updated Name">>, maps:get(name, KeyInfo)),
    ?assertEqual([read, write, admin], maps:get(permissions, KeyInfo)),

    ok.

%%====================================================================
%% 认证测试
%%====================================================================

test_validate_key() ->
    ApiKey = <<"validate-test-key">>,

    %% 添加 Key
    {ok, _} = beamai_a2a_auth:add_key(ApiKey, #{name => <<"Validate Test">>}),

    %% 验证 Key
    {ok, KeyInfo} = beamai_a2a_auth:validate_key(ApiKey),

    ?assertEqual(ApiKey, maps:get(key, KeyInfo)),
    ?assert(maps:get(use_count, KeyInfo) >= 1),

    ok.

test_invalid_key() ->
    %% 验证不存在的 Key
    {error, invalid_api_key} = beamai_a2a_auth:validate_key(<<"nonexistent-key">>),

    ok.

test_expired_key() ->
    ApiKey = <<"expired-test-key">>,

    %% 添加已过期的 Key
    PastTime = erlang:system_time(millisecond) - 1000,  %% 1 秒前
    {ok, _} = beamai_a2a_auth:add_key(ApiKey, #{
        name => <<"Expired Key">>,
        expires_at => PastTime
    }),

    %% 验证应该失败
    {error, key_expired} = beamai_a2a_auth:validate_key(ApiKey),

    ok.

test_stats() ->
    %% 添加一些 Keys
    {ok, _} = beamai_a2a_auth:add_key(<<"stats-key-1">>, #{name => <<"Stats 1">>}),
    {ok, _} = beamai_a2a_auth:add_key(<<"stats-key-2">>, #{name => <<"Stats 2">>}),

    %% 验证一个 Key
    beamai_a2a_auth:validate_key(<<"stats-key-1">>),

    %% 验证一个无效 Key
    beamai_a2a_auth:validate_key(<<"invalid-key">>),

    %% 获取统计
    Stats = beamai_a2a_auth:stats(),

    ?assert(maps:is_key(auth_success, Stats)),
    ?assert(maps:is_key(auth_failed, Stats)),
    ?assert(maps:is_key(keys_created, Stats)),
    ?assert(maps:is_key(keys_count, Stats)),

    ok.

%%====================================================================
%% 请求头认证测试
%%====================================================================

header_auth_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = beamai_a2a_auth:start_link(#{}),
         Pid
     end,
     fun(Pid) ->
         case is_process_alive(Pid) of
             true -> beamai_a2a_auth:stop();
             false -> ok
         end
     end,
     [
        {"Authorization Bearer 头认证", fun test_auth_bearer_header/0},
        {"Authorization ApiKey 头认证", fun test_auth_apikey_header/0},
        {"X-API-Key 头认证", fun test_auth_x_api_key_header/0},
        {"缺少认证头", fun test_missing_auth_header/0},
        {"匿名访问测试", fun test_anonymous_access/0}
     ]}.

test_auth_bearer_header() ->
    ApiKey = <<"bearer-test-key">>,
    {ok, _} = beamai_a2a_auth:add_key(ApiKey, #{name => <<"Bearer Test">>}),

    %% 使用 Bearer 认证
    Headers = [{<<"authorization">>, <<"Bearer bearer-test-key">>}],
    {ok, KeyInfo} = beamai_a2a_auth:authenticate(Headers),

    ?assertEqual(ApiKey, maps:get(key, KeyInfo)),

    ok.

test_auth_apikey_header() ->
    ApiKey = <<"apikey-test-key">>,
    {ok, _} = beamai_a2a_auth:add_key(ApiKey, #{name => <<"ApiKey Test">>}),

    %% 使用 ApiKey 认证
    Headers = [{<<"authorization">>, <<"ApiKey apikey-test-key">>}],
    {ok, KeyInfo} = beamai_a2a_auth:authenticate(Headers),

    ?assertEqual(ApiKey, maps:get(key, KeyInfo)),

    ok.

test_auth_x_api_key_header() ->
    ApiKey = <<"x-api-key-test">>,
    {ok, _} = beamai_a2a_auth:add_key(ApiKey, #{name => <<"X-API-Key Test">>}),

    %% 使用 X-API-Key 头
    Headers = [{<<"x-api-key">>, ApiKey}],
    {ok, KeyInfo} = beamai_a2a_auth:authenticate(Headers),

    ?assertEqual(ApiKey, maps:get(key, KeyInfo)),

    ok.

test_missing_auth_header() ->
    %% 没有认证头
    Headers = [{<<"content-type">>, <<"application/json">>}],
    {error, missing_api_key} = beamai_a2a_auth:authenticate(Headers),

    ok.

test_anonymous_access() ->
    %% 设置允许匿名访问
    ok = beamai_a2a_auth:set_config(#{allow_anonymous => true}),

    %% 没有认证头应该成功
    Headers = [{<<"content-type">>, <<"application/json">>}],
    {ok, Info} = beamai_a2a_auth:authenticate(Headers),

    ?assertEqual(true, maps:get(anonymous, Info)),

    %% 恢复配置
    ok = beamai_a2a_auth:set_config(#{allow_anonymous => false}),

    ok.

%%====================================================================
%% 配置测试
%%====================================================================

config_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = beamai_a2a_auth:start_link(#{}),
         Pid
     end,
     fun(Pid) ->
         case is_process_alive(Pid) of
             true -> beamai_a2a_auth:stop();
             false -> ok
         end
     end,
     [
        {"获取默认配置", fun test_default_config/0},
        {"设置配置", fun test_set_config/0},
        {"检查是否启用", fun test_is_enabled/0},
        {"禁用认证", fun test_disabled_auth/0}
     ]}.

test_default_config() ->
    Config = beamai_a2a_auth:get_config(),

    ?assertEqual(true, maps:get(enabled, Config)),
    ?assertEqual(false, maps:get(allow_anonymous, Config)),

    ok.

test_set_config() ->
    %% 设置新配置
    ok = beamai_a2a_auth:set_config(#{
        allow_anonymous => true,
        custom_option => <<"test">>
    }),

    Config = beamai_a2a_auth:get_config(),
    ?assertEqual(true, maps:get(allow_anonymous, Config)),
    ?assertEqual(<<"test">>, maps:get(custom_option, Config)),

    ok.

test_is_enabled() ->
    ?assertEqual(true, beamai_a2a_auth:is_enabled()),

    ok = beamai_a2a_auth:set_config(#{enabled => false}),
    ?assertEqual(false, beamai_a2a_auth:is_enabled()),

    ok = beamai_a2a_auth:set_config(#{enabled => true}),
    ?assertEqual(true, beamai_a2a_auth:is_enabled()),

    ok.

test_disabled_auth() ->
    %% 禁用认证
    ok = beamai_a2a_auth:set_config(#{enabled => false}),

    %% 任何请求都应该成功
    Headers = [],
    {ok, Info} = beamai_a2a_auth:authenticate(Headers),

    ?assertEqual(true, maps:get(anonymous, Info)),

    %% 恢复配置
    ok = beamai_a2a_auth:set_config(#{enabled => true}),

    ok.

%%====================================================================
%% 权限测试
%%====================================================================

permission_test_() ->
    {setup,
     fun() ->
         {ok, Pid} = beamai_a2a_auth:start_link(#{}),
         Pid
     end,
     fun(Pid) ->
         case is_process_alive(Pid) of
             true -> beamai_a2a_auth:stop();
             false -> ok
         end
     end,
     [
        {"权限检查 - 全部权限", fun test_permission_all/0},
        {"权限检查 - 特定权限", fun test_permission_specific/0},
        {"权限检查 - 权限不足", fun test_permission_insufficient/0}
     ]}.

test_permission_all() ->
    ApiKey = <<"perm-all-key">>,
    {ok, _} = beamai_a2a_auth:add_key(ApiKey, #{
        name => <<"All Permissions">>,
        permissions => all
    }),

    %% all 权限应该通过任何权限检查
    Headers = [{<<"authorization">>, <<"Bearer perm-all-key">>}],
    {ok, _} = beamai_a2a_auth:authenticate(Headers, #{required_permission => admin}),

    ok.

test_permission_specific() ->
    ApiKey = <<"perm-specific-key">>,
    {ok, _} = beamai_a2a_auth:add_key(ApiKey, #{
        name => <<"Specific Permissions">>,
        permissions => [read, write]
    }),

    %% 应该通过 read 权限检查
    Headers = [{<<"authorization">>, <<"Bearer perm-specific-key">>}],
    {ok, _} = beamai_a2a_auth:authenticate(Headers, #{required_permission => read}),

    ok.

test_permission_insufficient() ->
    ApiKey = <<"perm-limited-key">>,
    {ok, _} = beamai_a2a_auth:add_key(ApiKey, #{
        name => <<"Limited Permissions">>,
        permissions => [read]
    }),

    %% 应该无法通过 admin 权限检查
    Headers = [{<<"authorization">>, <<"Bearer perm-limited-key">>}],
    {error, insufficient_permissions} = beamai_a2a_auth:authenticate(Headers, #{required_permission => admin}),

    ok.
