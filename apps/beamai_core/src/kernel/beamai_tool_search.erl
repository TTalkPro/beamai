%%%-------------------------------------------------------------------
%%% @doc 工具检索：大工具集按需揭示（对标 Spring AI ToolSearchToolCallingAdvisor）
%%%
%%% 工具一多，把全部 schema 每轮都塞进提示词就成了纯浪费——模型一次用得上的
%%% 通常就那么两三个。本模块把「注册」与「广播」拆开：
%%%
%%%   - **注册**（kernel 的 tools）：决定**能不能执行**——全量注册，一个不少。
%%%   - **广播**（chat opts 的 tools）：决定**模型看不看得见**——由本模块的
%%%     around_chat filter 每轮现算，首轮只给检索工具一个。
%%%
%%% 模型想干活就得先调 `tool_search` 描述需求，检索工具返回若干工具名；下一轮
%%% filter 从历史里认领这些名字，把对应工具加进广播列表，模型这才看得见并调用。
%%% Spring 实测 28 个工具时省 34~64% token。
%%%
%%% 用法（工具集在建 kernel 前就已知，故索引一次性建好闭包进组件，
%%% 不需要 Spring 那套按 fingerprint 重建索引的机制）：
%%% ```
%%% Tools = [...],                       %% 全量工具
%%% {SearchTool, Filter} = beamai_tool_search:new(Tools, #{}),
%%% K0 = beamai_kernel:new(#{}, [Filter]),
%%% K = beamai_kernel:add_tools(K0, [SearchTool | Tools]).   %% 全量注册
%%% ```
%%%
%%% **未索引的工具原样透传**：filter 只裁剪自己索引过的那些，广播列表里其余的
%%% （典型如 agent 运行时追加的中断工具）一概不碰——否则它们会被静默吃掉。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_tool_search).

-export([new/1, new/2]).
-export([default_system_suffix/0]).

-define(DEFAULT_TOOL_NAME, <<"tool_search">>).
-define(DEFAULT_MAX_RESULTS, 5).

%% Opts：
%% - `index_module`  —— 索引后端（缺省 beamai_tool_index_keyword）
%% - `index_opts`    —— 传给后端 build/2 的选项（缺省 #{}）
%% - `max_results`   —— 单次检索最多返回几个工具名（缺省 5）
%% - `accumulate`    —— true 取历次检索的并集，false 只认最近一次（缺省 true）
%% - `tool_name`     —— 检索工具的名字（缺省 <<"tool_search">>）
-type opts() :: #{
    index_module => module(),
    index_opts => map(),
    max_results => pos_integer(),
    accumulate => boolean(),
    tool_name => binary()
}.

-export_type([opts/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc 建检索组件：返回 {检索工具, filter}，两者共享同一个索引
-spec new([beamai_tool:tool_spec()]) ->
    {beamai_tool:tool_spec(), beamai_filter:filter()}.
new(Tools) ->
    new(Tools, #{}).

-spec new([beamai_tool:tool_spec()], opts()) ->
    {beamai_tool:tool_spec(), beamai_filter:filter()}.
new(Tools, Opts) when is_list(Tools), is_map(Opts) ->
    Module = maps:get(index_module, Opts, beamai_tool_index_keyword),
    MaxResults = maps:get(max_results, Opts, ?DEFAULT_MAX_RESULTS),
    Accumulate = maps:get(accumulate, Opts, true),
    Name = maps:get(tool_name, Opts, ?DEFAULT_TOOL_NAME),
    Index = beamai_tool_index:new(Module, Tools, maps:get(index_opts, Opts, #{})),
    %% 索引覆盖的工具名——只有这些归 filter 管，其余透传
    Indexed = sets:from_list([beamai_tool:get_name(T) || T <- Tools], [{version, 2}]),
    {search_tool(Name, Index, MaxResults),
     search_filter(Name, Indexed, Accumulate)}.

%% @doc 缺省的系统提示补充文本（对照 Spring 的 systemMessageSuffix）
%%
%% **不自动注入**：kernel 的系统提示在最内层注入，filter 再加一条 system 消息
%% 就成了双 system 消息——各 provider 对此处理不一（Anthropic 的 system 是独立
%% 字段），不值得为省一次拼接冒这个险。需要时自行拼进 agent 的 system_prompt：
%% ```
%% Prompt = <<MyPrompt/binary, "\n\n", (beamai_tool_search:default_system_suffix())/binary>>
%% ```
%% 多数情况下检索工具自身的 description 已足够驱动模型，这条是不灵时的加强手段。
-spec default_system_suffix() -> binary().
default_system_suffix() ->
    <<"你能用的工具不会一次全部列出。当前工具清单里没有你需要的能力时，"
      "先调用 tool_search 用自然语言描述你想做的事，它会返回相关工具名，"
      "这些工具在随后的回合里就可直接调用。找不到合适工具时如实说明，不要臆造。"/utf8>>.

%%====================================================================
%% 内部 - 检索工具
%%====================================================================

%% @private 检索工具 spec（description 是模型肯不肯用它的关键，写足）
search_tool(Name, Index, MaxResults) ->
    #{
        name => Name,
        description => <<"按自然语言描述检索可用工具。为控制上下文长度，工具并非"
                         "一次全部列出——需要某种能力却在当前清单里找不到时，用本"
                         "工具描述你要做的事（如「查某地天气」「发邮件」），它返回"
                         "匹配的工具名，这些工具随后即可直接调用。"/utf8>>,
        parameters => #{
            <<"query">> => #{
                type => string,
                description => <<"要做的事的自然语言描述，例如「查询城市天气」"/utf8>>,
                required => true
            }
        },
        handler => fun(Args) ->
            {ok, search_names(Index, query_of(Args), MaxResults)}
        end
    }.

%% @private 空查询一律返回空，不落到索引
%%
%% 各后端对空查询本就不一致（keyword 无词元可打分 → 空；regex 的空模式是合法
%% 正则、**匹配一切** → 全量返回），而模型漏给参数是常态。在此收口保证「没说要
%% 什么就什么都不给」——否则换个后端就会把全量工具泄回提示词，检索的意义尽失。
search_names(_Index, <<>>, _MaxResults) ->
    [];
search_names(Index, Query, MaxResults) ->
    beamai_tool_index:search(Index, Query, MaxResults).

%% @private 取 query 参数（兼容 binary / atom 键；缺失即空串）
query_of(Args) ->
    case maps:get(<<"query">>, Args, maps:get(query, Args, <<>>)) of
        Q when is_binary(Q) -> Q;
        _ -> <<>>
    end.

%%====================================================================
%% 内部 - 裁剪 filter
%%====================================================================

%% @private 每轮 LLM 调用前重算广播列表（对照 Spring 的 prepareIteration）
search_filter(SearchName, Indexed, Accumulate) ->
    beamai_filter:new(<<"tool_search">>, #{
        around_chat => fun(#{messages := Msgs, opts := Opts} = Req, _FCtx, Next) ->
            Selected = selected_names(Msgs, SearchName, Accumulate),
            Next(Req#{opts => narrow(Opts, Selected, Indexed, SearchName)})
        end
    }).

%% @private 裁剪广播列表；opts 无 tools 字段（没工具可用）则原样不动
narrow(Opts, Selected, Indexed, SearchName) ->
    case maps:find(tools, Opts) of
        error ->
            Opts;
        {ok, Advertised} ->
            Opts#{tools => [T || T <- Advertised,
                                 keep(spec_name(T), Selected, Indexed, SearchName)]}
    end.

%% @private 某工具本轮是否广播
keep(Name, Selected, Indexed, SearchName) ->
    %% 检索工具自己永远在（否则模型没法再检索，一轮不中就彻底卡死）
    Name =:= SearchName
        %% 不归我管的（中断工具等运行时追加项）透传，不能被静默吃掉
        orelse not sets:is_element(Name, Indexed)
        %% 历次检索点过名的
        orelse lists:member(Name, Selected).

%% @private 取广播项的工具名（opts.tools 为 beamai_tool:to_tool_spec/1 的中间格式）
spec_name(#{name := N}) -> N;
spec_name(#{<<"name">> := N}) -> N;
spec_name(_) -> undefined.

%% @private 历史里历次 tool_search 点过名的工具
selected_names(Msgs, SearchName, Accumulate) ->
    Ids = search_call_ids(Msgs, SearchName),
    Results = [decode_names(C)
               || #{role := tool, tool_call_id := Id, content := C} <- Msgs,
                  lists:member(Id, Ids)],
    case Accumulate of
        true -> lists:usort(lists:append(Results));
        false -> last_or_empty(Results)
    end.

%% @private accumulate=false 时只认最近一次检索
last_or_empty([]) -> [];
last_or_empty(Results) -> lists:last(Results).

%% @private tool_search 那些调用的 id
%%
%% 工具结果消息只有 tool_call_id、不带工具名，故须回头从发起调用的 assistant
%% 回合认领 id，再拿 id 去匹配结果消息。
search_call_ids(Msgs, SearchName) ->
    lists:append(
      [[Id || TC <- TCs,
              {Id, Name, _Args} <- [beamai_tool:parse_tool_call(TC)],
              Name =:= SearchName]
       || #{tool_calls := TCs} <- Msgs, is_list(TCs)]).

%% @private 解检索结果（JSON 字符串数组）；解不动即当没检索过
decode_names(Content) when is_binary(Content) ->
    try jsx:decode(Content, [return_maps]) of
        Names when is_list(Names) -> [N || N <- Names, is_binary(N)];
        _ -> []
    catch
        _:_ -> []
    end;
decode_names(_) ->
    [].
