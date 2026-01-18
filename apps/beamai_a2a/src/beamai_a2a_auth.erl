%%%-------------------------------------------------------------------
%%% @doc A2A 认证模块
%%%
%%% 提供 API Key 认证功能，支持：
%%% - API Key 验证
%%% - Key 管理（添加、删除、列出）
%%% - 多 Key 支持（带元数据）
%%% - Key 过期时间
%%% - 权限控制（可选）
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 添加 API Key
%%% {ok, KeyInfo} = beamai_a2a_auth:add_key(<<"my-api-key">>, #{
%%%     name => <<"Production Key">>,
%%%     permissions => [read, write],
%%%     expires_at => undefined  %% 永不过期
%%% }).
%%%
%%% %% 验证请求
%%% Headers = [{<<"authorization">>, <<"Bearer my-api-key">>}],
%%% case beamai_a2a_auth:authenticate(Headers) of
%%%     {ok, KeyInfo} -> proceed;
%%%     {error, Reason} -> reject
%%% end.
%%%
%%% %% 或直接验证 Key
%%% case beamai_a2a_auth:validate_key(<<"my-api-key">>) of
%%%     {ok, KeyInfo} -> proceed;
%%%     {error, Reason} -> reject
%%% end.
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_auth).

-behaviour(gen_server).

%% 避免与 erlang:get/1 冲突
-compile({no_auto_import,[get/1]}).

%% API 导出
-export([
    start_link/0,
    start_link/1,
    stop/0,

    %% 认证
    authenticate/1,
    authenticate/2,
    validate_key/1,

    %% Key 管理
    add_key/1,
    add_key/2,
    remove_key/1,
    get_key/1,
    list_keys/0,
    update_key/2,

    %% 配置
    set_config/1,
    get_config/0,
    is_enabled/0,

    %% 统计
    stats/0
]).

%% gen_server 回调
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-include_lib("beamai_core/include/beamai_common.hrl").

%% 常量定义
-define(SERVER, ?MODULE).
-define(TABLE, beamai_a2a_auth_keys).

%% 服务器状态
-record(state, {
    config :: map(),
    stats :: map()
}).

%% API Key 记录
-record(api_key, {
    key :: binary(),
    name :: binary(),
    permissions :: [atom()] | all,
    expires_at :: non_neg_integer() | undefined,
    created_at :: non_neg_integer(),
    last_used_at :: non_neg_integer() | undefined,
    use_count :: non_neg_integer(),
    metadata :: map()
}).

%%====================================================================
%% API
%%====================================================================

%% @doc 启动认证服务
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    start_link(#{}).

%% @doc 启动认证服务（带配置）
%%
%% @param Config 配置选项
%%   - enabled: 是否启用认证（默认 true）
%%   - allow_anonymous: 是否允许匿名访问（默认 false）
%%   - header_name: 自定义头名称（默认支持 Authorization 和 X-API-Key）
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
start_link(Config) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Config, []).

%% @doc 停止服务
-spec stop() -> ok.
stop() ->
    gen_server:stop(?SERVER).

%% @doc 从请求头认证
%%
%% 支持的头格式：
%% - Authorization: Bearer <api-key>
%% - Authorization: ApiKey <api-key>
%% - X-API-Key: <api-key>
%%
%% @param Headers 请求头列表 [{Name, Value}]
%% @returns {ok, KeyInfo} | {error, Reason}
-spec authenticate(list()) -> {ok, map()} | {error, term()}.
authenticate(Headers) ->
    authenticate(Headers, #{}).

%% @doc 从请求头认证（带选项）
-spec authenticate(list(), map()) -> {ok, map()} | {error, term()}.
authenticate(Headers, Opts) ->
    gen_server:call(?SERVER, {authenticate, Headers, Opts}, ?DEFAULT_TIMEOUT).

%% @doc 验证 API Key
-spec validate_key(binary()) -> {ok, map()} | {error, term()}.
validate_key(ApiKey) ->
    gen_server:call(?SERVER, {validate_key, ApiKey}, ?DEFAULT_TIMEOUT).

%% @doc 添加 API Key（自动生成）
-spec add_key(map()) -> {ok, map()} | {error, term()}.
add_key(Metadata) ->
    ApiKey = generate_api_key(),
    add_key(ApiKey, Metadata).

%% @doc 添加 API Key
%%
%% @param ApiKey API Key 字符串
%% @param Metadata 元数据
%%   - name: Key 名称
%%   - permissions: 权限列表（默认 all）
%%   - expires_at: 过期时间戳（可选）
%% @returns {ok, KeyInfo} | {error, Reason}
-spec add_key(binary(), map()) -> {ok, map()} | {error, term()}.
add_key(ApiKey, Metadata) ->
    gen_server:call(?SERVER, {add_key, ApiKey, Metadata}, ?DEFAULT_TIMEOUT).

%% @doc 删除 API Key
-spec remove_key(binary()) -> ok | {error, term()}.
remove_key(ApiKey) ->
    gen_server:call(?SERVER, {remove_key, ApiKey}, ?DEFAULT_TIMEOUT).

%% @doc 获取 API Key 信息
-spec get_key(binary()) -> {ok, map()} | {error, not_found}.
get_key(ApiKey) ->
    gen_server:call(?SERVER, {get_key, ApiKey}, ?DEFAULT_TIMEOUT).

%% @doc 列出所有 API Key
-spec list_keys() -> [map()].
list_keys() ->
    gen_server:call(?SERVER, list_keys, ?DEFAULT_TIMEOUT).

%% @doc 更新 API Key 元数据
-spec update_key(binary(), map()) -> ok | {error, term()}.
update_key(ApiKey, Updates) ->
    gen_server:call(?SERVER, {update_key, ApiKey, Updates}, ?DEFAULT_TIMEOUT).

%% @doc 设置认证配置
-spec set_config(map()) -> ok.
set_config(Config) ->
    gen_server:call(?SERVER, {set_config, Config}, ?DEFAULT_TIMEOUT).

%% @doc 获取认证配置
-spec get_config() -> map().
get_config() ->
    gen_server:call(?SERVER, get_config, ?DEFAULT_TIMEOUT).

%% @doc 检查认证是否启用
-spec is_enabled() -> boolean().
is_enabled() ->
    gen_server:call(?SERVER, is_enabled, ?DEFAULT_TIMEOUT).

%% @doc 获取统计信息
-spec stats() -> map().
stats() ->
    gen_server:call(?SERVER, stats, ?DEFAULT_TIMEOUT).

%%====================================================================
%% gen_server 回调
%%====================================================================

%% @private
init(Config) ->
    %% 创建 ETS 表
    ets:new(?TABLE, [
        named_table,
        public,
        set,
        {keypos, #api_key.key},
        {read_concurrency, true}
    ]),

    %% 默认配置
    DefaultConfig = #{
        enabled => true,
        allow_anonymous => false,
        header_names => [<<"authorization">>, <<"x-api-key">>]
    },

    State = #state{
        config = maps:merge(DefaultConfig, Config),
        stats = #{
            auth_success => 0,
            auth_failed => 0,
            keys_created => 0,
            keys_removed => 0
        }
    },

    {ok, State}.

%% @private
handle_call({authenticate, Headers, Opts}, _From, State) ->
    case do_authenticate(Headers, Opts, State) of
        {ok, KeyInfo} ->
            NewStats = maps:update_with(auth_success, fun(V) -> V + 1 end, State#state.stats),
            {reply, {ok, KeyInfo}, State#state{stats = NewStats}};
        {error, Reason} ->
            NewStats = maps:update_with(auth_failed, fun(V) -> V + 1 end, State#state.stats),
            {reply, {error, Reason}, State#state{stats = NewStats}}
    end;

handle_call({validate_key, ApiKey}, _From, State) ->
    case do_validate_key(ApiKey) of
        {ok, KeyInfo} ->
            NewStats = maps:update_with(auth_success, fun(V) -> V + 1 end, State#state.stats),
            {reply, {ok, KeyInfo}, State#state{stats = NewStats}};
        {error, Reason} ->
            NewStats = maps:update_with(auth_failed, fun(V) -> V + 1 end, State#state.stats),
            {reply, {error, Reason}, State#state{stats = NewStats}}
    end;

handle_call({add_key, ApiKey, Metadata}, _From, State) ->
    case do_add_key(ApiKey, Metadata) of
        {ok, KeyInfo} ->
            NewStats = maps:update_with(keys_created, fun(V) -> V + 1 end, State#state.stats),
            {reply, {ok, KeyInfo}, State#state{stats = NewStats}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({remove_key, ApiKey}, _From, State) ->
    case do_remove_key(ApiKey) of
        ok ->
            NewStats = maps:update_with(keys_removed, fun(V) -> V + 1 end, State#state.stats),
            {reply, ok, State#state{stats = NewStats}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_key, ApiKey}, _From, State) ->
    Result = do_get_key(ApiKey),
    {reply, Result, State};

handle_call(list_keys, _From, State) ->
    Keys = do_list_keys(),
    {reply, Keys, State};

handle_call({update_key, ApiKey, Updates}, _From, State) ->
    Result = do_update_key(ApiKey, Updates),
    {reply, Result, State};

handle_call({set_config, NewConfig}, _From, State) ->
    MergedConfig = maps:merge(State#state.config, NewConfig),
    {reply, ok, State#state{config = MergedConfig}};

handle_call(get_config, _From, State) ->
    {reply, State#state.config, State};

handle_call(is_enabled, _From, State) ->
    Enabled = maps:get(enabled, State#state.config, true),
    {reply, Enabled, State};

handle_call(stats, _From, State) ->
    BaseStats = State#state.stats,
    FullStats = BaseStats#{
        keys_count => ets:info(?TABLE, size)
    },
    {reply, FullStats, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

%% @private
handle_cast(_Msg, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ets:delete(?TABLE),
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 从请求头认证
do_authenticate(Headers, Opts, State) ->
    Config = State#state.config,
    Enabled = maps:get(enabled, Config, true),
    AllowAnonymous = maps:get(allow_anonymous, Config, false),

    case Enabled of
        false ->
            %% 认证禁用，允许所有请求
            {ok, #{anonymous => true}};
        true ->
            case extract_api_key(Headers, Config) of
                {ok, ApiKey} ->
                    do_validate_key_with_permission(ApiKey, Opts);
                {error, no_key} when AllowAnonymous ->
                    {ok, #{anonymous => true}};
                {error, no_key} ->
                    {error, missing_api_key};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @private 从请求头提取 API Key
extract_api_key(Headers, Config) ->
    HeaderNames = maps:get(header_names, Config, [<<"authorization">>, <<"x-api-key">>]),
    extract_api_key_from_headers(Headers, HeaderNames).

extract_api_key_from_headers([], _) ->
    {error, no_key};
extract_api_key_from_headers([{Name, Value} | Rest], HeaderNames) ->
    LowerName = string:lowercase(Name),
    case lists:member(LowerName, HeaderNames) of
        true ->
            case parse_auth_header(LowerName, Value) of
                {ok, Key} -> {ok, Key};
                {error, _} -> extract_api_key_from_headers(Rest, HeaderNames)
            end;
        false ->
            extract_api_key_from_headers(Rest, HeaderNames)
    end.

%% @private 解析认证头
parse_auth_header(<<"authorization">>, Value) ->
    case binary:split(Value, <<" ">>) of
        [Type, Key] ->
            case string:lowercase(Type) of
                <<"bearer">> -> {ok, Key};
                <<"apikey">> -> {ok, Key};
                _ -> {error, invalid_auth_type}
            end;
        _ ->
            {error, invalid_auth_format}
    end;
parse_auth_header(<<"x-api-key">>, Value) ->
    {ok, Value};
parse_auth_header(_, _) ->
    {error, unknown_header}.

%% @private 验证 API Key
do_validate_key(ApiKey) ->
    do_validate_key_with_permission(ApiKey, #{}).

do_validate_key_with_permission(ApiKey, Opts) ->
    case ets:lookup(?TABLE, ApiKey) of
        [] ->
            {error, invalid_api_key};
        [KeyRecord] ->
            case is_key_valid(KeyRecord) of
                true ->
                    %% 更新使用统计并获取更新后的记录
                    UpdatedRecord = update_key_usage(KeyRecord),
                    %% 检查权限
                    RequiredPermission = maps:get(required_permission, Opts, undefined),
                    case check_permission(KeyRecord, RequiredPermission) of
                        true ->
                            {ok, key_record_to_map(UpdatedRecord)};
                        false ->
                            {error, insufficient_permissions}
                    end;
                {false, Reason} ->
                    {error, Reason}
            end
    end.

%% @private 检查 Key 是否有效
is_key_valid(#api_key{expires_at = undefined}) ->
    true;
is_key_valid(#api_key{expires_at = ExpiresAt}) ->
    Now = erlang:system_time(millisecond),
    case Now < ExpiresAt of
        true -> true;
        false -> {false, key_expired}
    end.

%% @private 检查权限
check_permission(_, undefined) -> true;
check_permission(#api_key{permissions = all}, _) -> true;
check_permission(#api_key{permissions = Perms}, Required) ->
    lists:member(Required, Perms).

%% @private 更新使用统计，返回更新后的记录
update_key_usage(#api_key{key = Key, use_count = Count} = KeyRecord) ->
    Now = erlang:system_time(millisecond),
    ets:update_element(?TABLE, Key, [
        {#api_key.last_used_at, Now},
        {#api_key.use_count, Count + 1}
    ]),
    KeyRecord#api_key{last_used_at = Now, use_count = Count + 1}.

%% @private 添加 API Key
do_add_key(ApiKey, Metadata) ->
    case ets:lookup(?TABLE, ApiKey) of
        [_] ->
            {error, key_already_exists};
        [] ->
            Now = erlang:system_time(millisecond),
            KeyRecord = #api_key{
                key = ApiKey,
                name = maps:get(name, Metadata, <<"Unnamed Key">>),
                permissions = maps:get(permissions, Metadata, all),
                expires_at = maps:get(expires_at, Metadata, undefined),
                created_at = Now,
                last_used_at = undefined,
                use_count = 0,
                metadata = maps:get(metadata, Metadata, #{})
            },
            ets:insert(?TABLE, KeyRecord),
            {ok, key_record_to_map(KeyRecord)}
    end.

%% @private 删除 API Key
do_remove_key(ApiKey) ->
    case ets:lookup(?TABLE, ApiKey) of
        [] ->
            {error, not_found};
        _ ->
            ets:delete(?TABLE, ApiKey),
            ok
    end.

%% @private 获取 API Key
do_get_key(ApiKey) ->
    case ets:lookup(?TABLE, ApiKey) of
        [] ->
            {error, not_found};
        [KeyRecord] ->
            {ok, key_record_to_map(KeyRecord)}
    end.

%% @private 列出所有 Key
do_list_keys() ->
    ets:foldl(
        fun(KeyRecord, Acc) ->
            [key_record_to_map_safe(KeyRecord) | Acc]
        end,
        [],
        ?TABLE
    ).

%% @private 更新 API Key
do_update_key(ApiKey, Updates) ->
    case ets:lookup(?TABLE, ApiKey) of
        [] ->
            {error, not_found};
        [KeyRecord] ->
            UpdatedRecord = apply_updates(KeyRecord, Updates),
            ets:insert(?TABLE, UpdatedRecord),
            ok
    end.

%% @private 应用更新
apply_updates(Record, Updates) ->
    Record#api_key{
        name = maps:get(name, Updates, Record#api_key.name),
        permissions = maps:get(permissions, Updates, Record#api_key.permissions),
        expires_at = maps:get(expires_at, Updates, Record#api_key.expires_at),
        metadata = maps:merge(Record#api_key.metadata, maps:get(metadata, Updates, #{}))
    }.

%% @private 生成 API Key
generate_api_key() ->
    %% 生成 32 字节随机数，Base64 编码
    Bytes = crypto:strong_rand_bytes(32),
    Base = base64:encode(Bytes),
    %% 移除不适合 URL 的字符
    Cleaned = binary:replace(Base, [<<"+">>, <<"/">>, <<"=">>], <<>>, [global]),
    <<"ak_", Cleaned/binary>>.

%% @private 将 Key 记录转换为 map（包含 Key）
key_record_to_map(#api_key{} = R) ->
    #{
        key => R#api_key.key,
        name => R#api_key.name,
        permissions => R#api_key.permissions,
        expires_at => R#api_key.expires_at,
        created_at => R#api_key.created_at,
        last_used_at => R#api_key.last_used_at,
        use_count => R#api_key.use_count,
        metadata => R#api_key.metadata
    }.

%% @private 将 Key 记录转换为 map（隐藏 Key）
key_record_to_map_safe(#api_key{} = R) ->
    #{
        key => mask_key(R#api_key.key),
        name => R#api_key.name,
        permissions => R#api_key.permissions,
        expires_at => R#api_key.expires_at,
        created_at => R#api_key.created_at,
        last_used_at => R#api_key.last_used_at,
        use_count => R#api_key.use_count
    }.

%% @private 遮盖 Key
mask_key(Key) when byte_size(Key) > 8 ->
    Prefix = binary:part(Key, 0, 4),
    Suffix = binary:part(Key, byte_size(Key) - 4, 4),
    <<Prefix/binary, "***", Suffix/binary>>;
mask_key(_) ->
    <<"***">>.
