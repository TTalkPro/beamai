%%%-------------------------------------------------------------------
%%% @doc Agent Card 模块
%%%
%%% 提供 A2A Agent Card 的生成、解析和管理功能。
%%% Agent Card 是 A2A 协议的核心发现机制，描述 Agent 的能力。
%%%
%%% == 功能 ==
%%%
%%% - 从 Agent 配置自动生成 Agent Card
%%% - 将 Tools 转换为 Skills
%%% - 序列化/反序列化 JSON
%%% - 验证 Agent Card 格式
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 从配置生成 Agent Card
%%% Config = #{
%%%     name => <<"inventory-agent">>,
%%%     description => <<"库存管理 Agent">>,
%%%     url => <<"https://example.com/a2a">>,
%%%     tools => [CheckStockTool, ReserveItemsTool]
%%% },
%%% {ok, Card} = beamai_a2a_card:generate(Config).
%%%
%%% %% 转换为 JSON
%%% JsonBin = beamai_a2a_card:to_json(Card).
%%%
%%% %% 从 JSON 解析
%%% {ok, Card2} = beamai_a2a_card:from_json(JsonBin).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_a2a_card).

%% API 导出
-export([
    generate/1,
    to_json/1,
    from_json/1,
    to_map/1,
    from_map/1,
    tools_to_skills/1,
    validate/1
]).

%% A2A 协议版本
-define(PROTOCOL_VERSION, <<"0.3.0">>).

%% 默认 MIME 类型
-define(DEFAULT_INPUT_MODES, [<<"text/plain">>, <<"application/json">>]).
-define(DEFAULT_OUTPUT_MODES, [<<"text/plain">>, <<"application/json">>]).

%%====================================================================
%% API
%%====================================================================

%% @doc 从 Agent 配置生成 Agent Card
%%
%% 配置选项：
%% - name: Agent 名称（必需）
%% - description: Agent 描述（必需）
%% - url: A2A 端点 URL（必需）
%% - version: Agent 版本（可选，默认 "1.0.0"）
%% - tools: 工具列表（可选，自动转换为 Skills）
%% - provider: 提供者信息（可选）
%% - capabilities: 能力配置（可选）
%%
%% @param Config Agent 配置
%% @returns {ok, AgentCard} | {error, Reason}
-spec generate(map()) -> {ok, map()} | {error, term()}.
generate(Config) ->
    try
        %% 必需字段
        Name = maps:get(name, Config),
        Description = maps:get(description, Config),
        Url = maps:get(url, Config),

        %% 可选字段
        Version = maps:get(version, Config, <<"1.0.0">>),
        Tools = maps:get(tools, Config, []),
        Provider = maps:get(provider, Config, undefined),
        Capabilities = maps:get(capabilities, Config, default_capabilities()),

        %% 转换工具为技能
        Skills = tools_to_skills(Tools),

        %% 构建 Agent Card
        Card = #{
            name => ensure_binary(Name),
            description => ensure_binary(Description),
            url => ensure_binary(Url),
            version => ensure_binary(Version),
            protocol_version => ?PROTOCOL_VERSION,
            capabilities => Capabilities,
            default_input_modes => ?DEFAULT_INPUT_MODES,
            default_output_modes => ?DEFAULT_OUTPUT_MODES,
            skills => Skills
        },

        %% 添加可选的 provider
        Card1 = case Provider of
            undefined -> Card;
            _ -> Card#{provider => Provider}
        end,

        {ok, Card1}
    catch
        error:{badkey, Key} ->
            {error, {missing_required_field, Key}};
        _:Reason ->
            {error, Reason}
    end.

%% @doc 将 Agent Card 转换为 JSON 字符串
%%
%% @param Card Agent Card map
%% @returns JSON 二进制字符串
-spec to_json(map()) -> binary().
to_json(Card) ->
    JsonMap = to_map(Card),
    jsx:encode(JsonMap, []).

%% @doc 从 JSON 字符串解析 Agent Card
%%
%% @param JsonBin JSON 二进制字符串
%% @returns {ok, AgentCard} | {error, Reason}
-spec from_json(binary()) -> {ok, map()} | {error, term()}.
from_json(JsonBin) ->
    try
        JsonMap = jsx:decode(JsonBin, [return_maps]),
        from_map(JsonMap)
    catch
        _:Reason ->
            {error, {json_decode_error, Reason}}
    end.

%% @doc 将内部 Agent Card 格式转换为 JSON 兼容格式
%%
%% 将 atom keys 转换为 binary keys，符合 A2A JSON 规范。
-spec to_map(map()) -> map().
to_map(Card) ->
    #{
        <<"name">> => maps:get(name, Card),
        <<"description">> => maps:get(description, Card),
        <<"url">> => maps:get(url, Card),
        <<"version">> => maps:get(version, Card, <<"1.0.0">>),
        <<"protocolVersion">> => maps:get(protocol_version, Card, ?PROTOCOL_VERSION),
        <<"capabilities">> => capabilities_to_map(maps:get(capabilities, Card, #{})),
        <<"defaultInputModes">> => maps:get(default_input_modes, Card, ?DEFAULT_INPUT_MODES),
        <<"defaultOutputModes">> => maps:get(default_output_modes, Card, ?DEFAULT_OUTPUT_MODES),
        <<"skills">> => [skill_to_map(S) || S <- maps:get(skills, Card, [])]
    }.

%% @doc 从 JSON 格式解析为内部 Agent Card 格式
%%
%% 将 binary keys 转换为 atom keys。
-spec from_map(map()) -> {ok, map()} | {error, term()}.
from_map(JsonMap) ->
    try
        Card = #{
            name => maps:get(<<"name">>, JsonMap),
            description => maps:get(<<"description">>, JsonMap),
            url => maps:get(<<"url">>, JsonMap),
            version => maps:get(<<"version">>, JsonMap, <<"1.0.0">>),
            protocol_version => maps:get(<<"protocolVersion">>, JsonMap, ?PROTOCOL_VERSION),
            capabilities => map_to_capabilities(maps:get(<<"capabilities">>, JsonMap, #{})),
            default_input_modes => maps:get(<<"defaultInputModes">>, JsonMap, ?DEFAULT_INPUT_MODES),
            default_output_modes => maps:get(<<"defaultOutputModes">>, JsonMap, ?DEFAULT_OUTPUT_MODES),
            skills => [map_to_skill(S) || S <- maps:get(<<"skills">>, JsonMap, [])]
        },
        {ok, Card}
    catch
        error:{badkey, Key} ->
            {error, {missing_required_field, Key}};
        _:Reason ->
            {error, Reason}
    end.

%% @doc 将工具列表转换为技能列表
%%
%% Tool 格式（来自 beamai_agent）：
%% #{
%%     name => <<"tool_name">>,
%%     description => <<"Tool description">>,
%%     parameters => #{...}
%% }
%%
%% 转换为 Skill 格式：
%% #{
%%     id => <<"tool_name">>,
%%     name => <<"Tool Name">>,
%%     description => <<"Tool description">>,
%%     tags => [...],
%%     examples => [...]
%% }
-spec tools_to_skills([map()]) -> [map()].
tools_to_skills(Tools) ->
    [tool_to_skill(T) || T <- Tools].

%% @doc 验证 Agent Card 格式
%%
%% @param Card Agent Card
%% @returns ok | {error, Reason}
-spec validate(map()) -> ok | {error, term()}.
validate(Card) ->
    RequiredFields = [name, description, url],
    case check_required_fields(Card, RequiredFields) of
        ok ->
            validate_skills(maps:get(skills, Card, []));
        Error ->
            Error
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 默认能力配置
default_capabilities() ->
    #{
        streaming => false,
        push_notifications => false,
        state_transition_history => true,
        extended_agent_card => false
    }.

%% @private 确保值为 binary
ensure_binary(V) when is_binary(V) -> V;
ensure_binary(V) when is_list(V) -> list_to_binary(V);
ensure_binary(V) when is_atom(V) -> atom_to_binary(V, utf8);
ensure_binary(V) -> V.

%% @private 将单个工具转换为技能
tool_to_skill(Tool) ->
    Name = maps:get(name, Tool, maps:get(<<"name">>, Tool, <<"unknown">>)),
    Description = maps:get(description, Tool, maps:get(<<"description">>, Tool, <<>>)),

    %% 生成人类可读的名称
    HumanName = tool_name_to_human(Name),

    %% 从描述中提取标签
    Tags = extract_tags(Description),

    #{
        id => ensure_binary(Name),
        name => HumanName,
        description => ensure_binary(Description),
        tags => Tags,
        examples => [],
        input_modes => ?DEFAULT_INPUT_MODES,
        output_modes => ?DEFAULT_OUTPUT_MODES
    }.

%% @private 将工具名转换为人类可读格式
%% check_stock -> "Check Stock"
tool_name_to_human(Name) when is_binary(Name) ->
    Words = binary:split(Name, [<<"_">>, <<"-">>], [global]),
    Capitalized = [capitalize(W) || W <- Words],
    iolist_to_binary(lists:join(<<" ">>, Capitalized));
tool_name_to_human(Name) when is_atom(Name) ->
    tool_name_to_human(atom_to_binary(Name, utf8));
tool_name_to_human(Name) ->
    ensure_binary(Name).

%% @private 首字母大写
capitalize(<<>>) -> <<>>;
capitalize(<<First:8, Rest/binary>>) when First >= $a, First =< $z ->
    <<(First - 32):8, Rest/binary>>;
capitalize(Bin) -> Bin.

%% @private 从描述中提取标签
extract_tags(Description) when is_binary(Description) ->
    %% 简单实现：提取常见关键词
    Keywords = [<<"query">>, <<"search">>, <<"create">>, <<"update">>,
                <<"delete">>, <<"get">>, <<"list">>, <<"check">>,
                <<"reserve">>, <<"book">>, <<"cancel">>],
    LowerDesc = string:lowercase(Description),
    [K || K <- Keywords, binary:match(LowerDesc, K) =/= nomatch];
extract_tags(_) ->
    [].

%% @private 将 Skill 转换为 JSON map
skill_to_map(Skill) ->
    #{
        <<"id">> => maps:get(id, Skill),
        <<"name">> => maps:get(name, Skill),
        <<"description">> => maps:get(description, Skill),
        <<"tags">> => maps:get(tags, Skill, []),
        <<"examples">> => maps:get(examples, Skill, []),
        <<"inputModes">> => maps:get(input_modes, Skill, ?DEFAULT_INPUT_MODES),
        <<"outputModes">> => maps:get(output_modes, Skill, ?DEFAULT_OUTPUT_MODES)
    }.

%% @private 从 JSON map 解析 Skill
map_to_skill(Map) ->
    #{
        id => maps:get(<<"id">>, Map),
        name => maps:get(<<"name">>, Map),
        description => maps:get(<<"description">>, Map, <<>>),
        tags => maps:get(<<"tags">>, Map, []),
        examples => maps:get(<<"examples">>, Map, []),
        input_modes => maps:get(<<"inputModes">>, Map, ?DEFAULT_INPUT_MODES),
        output_modes => maps:get(<<"outputModes">>, Map, ?DEFAULT_OUTPUT_MODES)
    }.

%% @private 将 capabilities 转换为 JSON map
capabilities_to_map(Caps) ->
    #{
        <<"streaming">> => maps:get(streaming, Caps, false),
        <<"pushNotifications">> => maps:get(push_notifications, Caps, false),
        <<"stateTransitionHistory">> => maps:get(state_transition_history, Caps, true)
    }.

%% @private 从 JSON map 解析 capabilities
map_to_capabilities(Map) ->
    #{
        streaming => maps:get(<<"streaming">>, Map, false),
        push_notifications => maps:get(<<"pushNotifications">>, Map, false),
        state_transition_history => maps:get(<<"stateTransitionHistory">>, Map, true)
    }.

%% @private 检查必需字段
check_required_fields(_Card, []) ->
    ok;
check_required_fields(Card, [Field | Rest]) ->
    case maps:is_key(Field, Card) of
        true -> check_required_fields(Card, Rest);
        false -> {error, {missing_required_field, Field}}
    end.

%% @private 验证技能列表
validate_skills([]) ->
    ok;
validate_skills([Skill | Rest]) ->
    case validate_skill(Skill) of
        ok -> validate_skills(Rest);
        Error -> Error
    end.

%% @private 验证单个技能
validate_skill(Skill) ->
    RequiredFields = [id, name],
    check_required_fields(Skill, RequiredFields).
