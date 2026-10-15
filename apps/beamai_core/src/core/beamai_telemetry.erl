%%%-------------------------------------------------------------------
%%% @doc 轻量 telemetry dispatch
%%%
%%% BeamAI 内部关键路径通过本模块发布结构化事件；业务模块不直接依赖
%%% logger，也不假设某个监控系统存在。事件只是纯数据，消费方式由用户配置：
%%%
%%% ```
%%% Handler = fun(#{name := Name, data := Data}) -> ... end,
%%% ok = beamai_telemetry:add_handler(app_trace, Handler),
%%% ok = beamai_telemetry:dispatch([beamai, tool, started],
%%%                                #{tool => <<"search">>},
%%%                                #{run_id => RunId}).
%%% '''
%%%
%%% 设计约束：
%%% <ul>
%%%   <li>事件负载必须由发布方保守化：不得默认放 prompt、tool result 全文、
%%%       API key 等敏感内容。</li>
%%%   <li>handler 异常被隔离并转为一条 warning；业务调用不感知失败。</li>
%%%   <li>默认只开启 info 及以上级别，debug 级高频事件必须显式开启。</li>
%%%   <li>不引入新依赖；未来可把注入 handler 适配到 OpenTelemetry、
%%%       telemetry 或日志采集器。</li>
%%% </ul>
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_telemetry).

-export([dispatch/3, dispatch/4]).
-export([add_handler/2, remove_handler/1, set_config/1, default_logger/1]).

-export_type([level/0, event_name/0, event/0, handler/0, handler_id/0]).

%%====================================================================
%% Types
%%====================================================================

-type level() :: debug | info | notice | warning | error.
-type event_name() :: [atom(), ...].
-type handler_id() :: atom().
-type handler() :: fun((event()) -> term()).

-type event() :: #{
    name := event_name(),
    level := level(),
    timestamp := integer(),
    data := map(),
    metadata := map()
}.

%% `enabled_levels => all` 便于测试或本地诊断；生产建议显式列出需要的级别。
-type config() :: #{
    enabled => boolean(),
    enabled_levels => all | [level()],
    handlers => [#{id => handler_id(), handler => handler()}]
}.

%%====================================================================
%% API
%%====================================================================

%% @doc 以 info 级别发布事件
-spec dispatch(event_name(), map(), map()) -> ok.
dispatch(Name, Data, Metadata) ->
    dispatch(info, Name, Data, Metadata).

%% @doc 发布指定级别事件
%%
%% 该函数没有返回错误分支：配置无效、handler 返回错误值或 handler crash
%% 都只记录 warning，调用方继续保持原业务路径。
-spec dispatch(level(), event_name(), map(), map()) -> ok.
dispatch(Level, Name, Data, Metadata) ->
    case enabled(Level) of
        true ->
            Event = #{
                name => Name,
                level => Level,
                timestamp => os:system_time(nanosecond),
                data => safe_map(Data),
                metadata => safe_map(Metadata)
            },
            lists:foreach(fun(Entry) -> run_handler(Entry, Event) end, handlers());
        false ->
            ok
    end.

%% @doc 注入 handler；重复 ID 覆盖旧 handler
-spec add_handler(handler_id(), handler()) -> ok.
add_handler(Id, Handler) when is_atom(Id), is_function(Handler, 1) ->
    Config = config(),
    Old = maps:get(handlers, Config, []),
    Entry = #{id => Id, handler => Handler},
    NewHandlers = [Entry | [OldEntry || #{id := OldId} = OldEntry <- Old,
                                         OldId =/= Id]],
    set_config(Config#{handlers => NewHandlers}).

%% @doc 移除 handler；ID 不存在时也是成功
-spec remove_handler(handler_id()) -> ok.
remove_handler(Id) ->
    Config = config(),
    Old = maps:get(handlers, Config, []),
    set_config(Config#{handlers => [OldEntry || #{id := OldId} = OldEntry <- Old,
                                                 OldId =/= Id]}).

%% @doc 覆写 telemetry 配置
-spec set_config(config()) -> ok.
set_config(Config) when is_map(Config) ->
    application:set_env(beamai_core, telemetry, Config).

%% @doc 内置安全日志 handler
%%
%% 只记录事件名、级别、ID 与少量通用元数据，不格式化 `data`。这样即使
%% 发布点误传大对象或敏感负载，也不会进入默认日志。
-spec default_logger(event()) -> ok.
default_logger(#{name := Name, level := Level} = Event) ->
    Ids = correlation_ids(Event),
    logger:info("beamai telemetry event=~ts level=~p~ts",
                [event_name_text(Name), Level, format_ids(Ids)]).

%%====================================================================
%% Internal functions
%%====================================================================

-spec config() -> config().
config() ->
    case application:get_env(beamai_core, telemetry) of
        {ok, Config} when is_map(Config) -> Config;
        _ -> #{}
    end.

-spec enabled(level()) -> boolean().
enabled(Level) ->
    Config = config(),
    maps:get(enabled, Config, true)
    andalso level_enabled(maps:get(enabled_levels, Config,
                                   [info, notice, warning, error]), Level).

-spec level_enabled(all | [level()], level()) -> boolean().
level_enabled(all, _Level) -> true;
level_enabled(Levels, Level) when is_list(Levels) -> lists:member(Level, Levels).

-spec handlers() -> [#{id => handler_id(), handler => handler()}].
handlers() ->
    maps:get(handlers, config(), []).

-spec run_handler(#{id => handler_id(), handler => handler()}, event()) -> ok.
run_handler(#{id := Id, handler := Handler}, Event)
  when is_atom(Id), is_function(Handler, 1) ->
    try Handler(Event)
    catch Class:Reason:Stack ->
        logger:warning(
          "beamai telemetry handler crashed id=~p class=~p reason=~p "
          "event=~ts stack_depth=~p",
          [Id, Class, Reason, event_name_text(maps:get(name, Event)),
           length(Stack)])
    end;
run_handler(Entry, _Event) ->
    logger:warning("beamai telemetry ignored invalid handler=~0p", [Entry]).

-spec safe_map(term()) -> map().
safe_map(Map) when is_map(Map) -> Map;
safe_map(_) -> #{}.

-spec correlation_ids(event()) -> [{atom(), term()}].
correlation_ids(#{metadata := Metadata}) ->
    [Entry || Key <- [run_id, request_id, conversation_id, message_id, batch_id],
              {_, _} = Entry <- [maps:find(Key, Metadata)]].

-spec event_name_text(event_name()) -> unicode:chardata().
event_name_text(Name) ->
    iolist_to_binary(lists:join(".", [atom_to_list(Part) || Part <- Name])).

-spec format_ids([{atom(), term()}]) -> unicode:chardata().
format_ids(Ids) ->
    [[$ , atom_to_list(Key), $=, io_lib:format("~0p", [Value])]
     || {Key, Value} <- Ids].
