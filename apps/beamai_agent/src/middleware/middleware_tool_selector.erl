%%%-------------------------------------------------------------------
%%% @doc LLM Tool Selector Middleware - 智能工具筛选
%%%
%%% 使用 LLM 在模型调用前智能筛选相关工具，减少 token 使用。
%%%
%%% == 功能特性 ==
%%%
%%% - 使用轻量 LLM 预筛选相关工具
%%% - 减少主模型的工具描述 token
%%% - 支持配置最大工具数量
%%% - 支持必须包含的工具列表
%%% - 支持禁用筛选的条件
%%%
%%% == 配置示例 ==
%%%
%%% ```erlang
%%% {middleware_tool_selector, #{
%%%     %% 筛选后最大工具数量
%%%     max_tools => 5,
%%%
%%%     %% 必须包含的工具（不会被筛选掉）
%%%     always_include => [<<"search">>, <<"calculator">>],
%%%
%%%     %% 筛选 LLM 配置（可选，默认使用主 LLM）
%%%     selector_llm => #{
%%%         provider => openai,
%%%         model => <<"gpt-3.5-turbo">>
%%%     },
%%%
%%%     %% 触发筛选的最小工具数量
%%%     min_tools_to_filter => 8,
%%%
%%%     %% 筛选提示词模板
%%%     prompt_template => <<"...">>,
%%%
%%%     %% 调试模式
%%%     debug => false
%%% }}
%%% ```
%%%
%%% == 工作流程 ==
%%%
%%% 1. before_model: 检查工具数量
%%% 2. 如果超过阈值，调用筛选 LLM
%%% 3. 根据筛选结果更新 tools 列表
%%% 4. 主模型调用使用筛选后的工具
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_tool_selector).

-behaviour(beamai_middleware).

%% Middleware 回调
-export([init/1, before_model/2, after_model/2]).

%% 工具函数
-export([
    select_tools/3,
    get_selection_stats/1
]).

%%====================================================================
%% Middleware 回调
%%====================================================================

%% @doc 初始化 Middleware 状态
-spec init(map()) -> map().
init(Opts) ->
    #{
        max_tools => maps:get(max_tools, Opts, 5),
        always_include => maps:get(always_include, Opts, []),
        selector_llm => maps:get(selector_llm, Opts, undefined),
        min_tools_to_filter => maps:get(min_tools_to_filter, Opts, 8),
        prompt_template => maps:get(prompt_template, Opts, default_prompt_template()),
        cache_enabled => maps:get(cache_enabled, Opts, true),
        debug => maps:get(debug, Opts, false)
    }.

%% @doc 模型调用前筛选工具
-spec before_model(map(), map()) -> beamai_middleware:middleware_result().
before_model(State, MwState) ->
    #{min_tools_to_filter := MinTools,
      max_tools := MaxTools,
      always_include := AlwaysInclude,
      debug := Debug} = MwState,

    Tools = graph:get(State, tools, []),
    ToolCount = length(Tools),

    case ToolCount > MinTools of
        false ->
            %% 工具数量不够，跳过筛选
            ok;
        true ->
            %% 获取用户消息
            Messages = graph:get(State, messages, []),
            UserMessage = get_last_user_message(Messages),

            case UserMessage of
                undefined ->
                    ok;
                _ ->
                    %% 执行筛选
                    case select_tools(Tools, UserMessage, MwState) of
                        {ok, SelectedTools} ->
                            %% 合并必须包含的工具
                            FinalTools = merge_always_include(SelectedTools, Tools,
                                                               AlwaysInclude, MaxTools),

                            case Debug of
                                true ->
                                    OriginalNames = [get_tool_name(T) || T <- Tools],
                                    SelectedNames = [get_tool_name(T) || T <- FinalTools],
                                    logger:info("[ToolSelector] 从 ~p 个工具筛选出 ~p 个: ~p -> ~p",
                                               [ToolCount, length(FinalTools),
                                                OriginalNames, SelectedNames]);
                                false ->
                                    ok
                            end,

                            %% 保存原始工具列表
                            {update, #{
                                mw_original_tools => Tools,
                                tools => FinalTools,
                                mw_tool_selection_count =>
                                    graph:get(State, mw_tool_selection_count, 0) + 1
                            }};
                        {error, Reason} ->
                            case Debug of
                                true ->
                                    logger:warning("[ToolSelector] 筛选失败: ~p", [Reason]);
                                false ->
                                    ok
                            end,
                            ok
                    end
            end
    end.

%% @doc 模型调用后恢复原始工具
-spec after_model(map(), map()) -> beamai_middleware:middleware_result().
after_model(State, _MwState) ->
    case graph:get(State, mw_original_tools, undefined) of
        undefined ->
            ok;
        OriginalTools ->
            %% 恢复原始工具列表
            {update, #{
                tools => OriginalTools,
                mw_original_tools => undefined
            }}
    end.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 使用 LLM 筛选工具
-spec select_tools([map()], binary(), map()) -> {ok, [map()]} | {error, term()}.
select_tools(Tools, UserMessage, MwState) ->
    #{max_tools := MaxTools,
      selector_llm := SelectorLLM,
      prompt_template := PromptTemplate,
      cache_enabled := CacheEnabled} = MwState,

    %% 检查缓存
    CacheKey = {UserMessage, [get_tool_name(T) || T <- Tools]},
    case CacheEnabled andalso get_cache(CacheKey) of
        {ok, CachedTools} ->
            {ok, CachedTools};
        _ ->
            %% 构建工具描述
            ToolDescriptions = build_tool_descriptions(Tools),

            %% 构建提示词
            Prompt = build_selection_prompt(PromptTemplate, UserMessage,
                                            ToolDescriptions, MaxTools),

            %% 调用 LLM
            LLMConfig = case SelectorLLM of
                undefined -> #{};
                Config -> Config
            end,

            case call_selector_llm(Prompt, LLMConfig) of
                {ok, Response} ->
                    %% 解析响应
                    SelectedNames = parse_selection_response(Response),
                    SelectedTools = filter_tools_by_names(Tools, SelectedNames),

                    %% 缓存结果
                    case CacheEnabled of
                        true -> put_cache(CacheKey, SelectedTools);
                        false -> ok
                    end,

                    {ok, SelectedTools};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

%% @doc 获取筛选统计
-spec get_selection_stats(map()) -> map().
get_selection_stats(State) ->
    #{
        selection_count => graph:get(State, mw_tool_selection_count, 0),
        cache_hits => get(mw_tool_selector_cache_hits)
    }.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 默认提示词模板
-spec default_prompt_template() -> binary().
default_prompt_template() ->
    <<"You are a tool selector. Given a user message and a list of available tools, "
      "select the most relevant tools for the task.\n\n"
      "User message: {{message}}\n\n"
      "Available tools:\n{{tools}}\n\n"
      "Select up to {{max_tools}} most relevant tools. "
      "Return ONLY the tool names, one per line, nothing else.\n\n"
      "Selected tools:">>.

%% @private 获取最后的用户消息
-spec get_last_user_message([map()]) -> binary() | undefined.
get_last_user_message([]) ->
    undefined;
get_last_user_message(Messages) ->
    UserMessages = lists:filter(fun(M) ->
        Role = maps:get(<<"role">>, M, maps:get(role, M, undefined)),
        Role =:= <<"user">> orelse Role =:= user
    end, Messages),

    case UserMessages of
        [] ->
            undefined;
        _ ->
            LastMsg = lists:last(UserMessages),
            maps:get(<<"content">>, LastMsg, maps:get(content, LastMsg, undefined))
    end.

%% @private 获取工具名称
-spec get_tool_name(map()) -> binary().
get_tool_name(#{name := Name}) -> Name;
get_tool_name(#{<<"name">> := Name}) -> Name;
get_tool_name(_) -> <<"unknown">>.

%% @private 获取工具描述
-spec get_tool_description(map()) -> binary().
get_tool_description(#{description := Desc}) -> Desc;
get_tool_description(#{<<"description">> := Desc}) -> Desc;
get_tool_description(_) -> <<>>.

%% @private 构建工具描述
-spec build_tool_descriptions([map()]) -> binary().
build_tool_descriptions(Tools) ->
    Descriptions = lists:map(fun(Tool) ->
        Name = get_tool_name(Tool),
        Desc = get_tool_description(Tool),
        <<"- ", Name/binary, ": ", Desc/binary>>
    end, Tools),
    iolist_to_binary(lists:join(<<"\n">>, Descriptions)).

%% @private 构建筛选提示词
-spec build_selection_prompt(binary(), binary(), binary(), pos_integer()) -> binary().
build_selection_prompt(Template, Message, ToolDescriptions, MaxTools) ->
    Prompt1 = binary:replace(Template, <<"{{message}}">>, Message),
    Prompt2 = binary:replace(Prompt1, <<"{{tools}}">>, ToolDescriptions),
    Prompt3 = binary:replace(Prompt2, <<"{{max_tools}}">>,
                             integer_to_binary(MaxTools)),
    Prompt3.

%% @private 调用筛选 LLM
-spec call_selector_llm(binary(), map()) -> {ok, binary()} | {error, term()}.
call_selector_llm(Prompt, LLMConfig) ->
    %% 使用简单的聊天调用
    Messages = [#{role => user, content => Prompt}],
    Request = #{messages => Messages},

    case catch llm_client:chat(LLMConfig, Request) of
        {ok, Response} ->
            Content = extract_llm_content(Response),
            {ok, Content};
        {error, Reason} ->
            {error, Reason};
        {'EXIT', Reason} ->
            {error, Reason}
    end.

%% @private 提取 LLM 响应内容
-spec extract_llm_content(map()) -> binary().
extract_llm_content(#{<<"content">> := Content}) when is_binary(Content) ->
    Content;
extract_llm_content(#{content := Content}) when is_binary(Content) ->
    Content;
extract_llm_content(#{<<"content">> := ContentList}) when is_list(ContentList) ->
    Texts = lists:filtermap(fun
        (#{<<"type">> := <<"text">>, <<"text">> := Text}) -> {true, Text};
        (_) -> false
    end, ContentList),
    iolist_to_binary(Texts);
extract_llm_content(_) ->
    <<>>.

%% @private 解析筛选响应
-spec parse_selection_response(binary()) -> [binary()].
parse_selection_response(Response) ->
    %% 按行分割并清理
    Lines = binary:split(Response, [<<"\n">>, <<"\r">>], [global, trim_all]),
    lists:filtermap(fun(Line) ->
        %% 去除序号和空白
        Cleaned = re:replace(Line, "^\\s*[-\\d.)*]+\\s*", "", [{return, binary}]),
        Trimmed = string:trim(Cleaned),
        case byte_size(Trimmed) > 0 of
            true -> {true, Trimmed};
            false -> false
        end
    end, Lines).

%% @private 按名称筛选工具
-spec filter_tools_by_names([map()], [binary()]) -> [map()].
filter_tools_by_names(Tools, Names) ->
    %% 名称转小写用于比较
    LowerNames = [string:lowercase(binary_to_list(N)) || N <- Names],

    lists:filter(fun(Tool) ->
        Name = get_tool_name(Tool),
        LowerName = string:lowercase(binary_to_list(Name)),
        lists:member(LowerName, LowerNames)
    end, Tools).

%% @private 合并必须包含的工具
-spec merge_always_include([map()], [map()], [binary()], pos_integer()) -> [map()].
merge_always_include(SelectedTools, AllTools, AlwaysInclude, MaxTools) ->
    %% 获取必须包含的工具
    AlwaysTools = lists:filter(fun(Tool) ->
        Name = get_tool_name(Tool),
        lists:member(Name, AlwaysInclude)
    end, AllTools),

    %% 合并（去重）
    SelectedNames = [get_tool_name(T) || T <- SelectedTools],
    ExtraAlways = lists:filter(fun(Tool) ->
        Name = get_tool_name(Tool),
        not lists:member(Name, SelectedNames)
    end, AlwaysTools),

    Combined = AlwaysTools ++ lists:filter(fun(Tool) ->
        Name = get_tool_name(Tool),
        not lists:member(Name, AlwaysInclude)
    end, SelectedTools),

    %% 限制数量
    lists:sublist(Combined, MaxTools).

%% @private 获取缓存
-spec get_cache(term()) -> {ok, term()} | undefined.
get_cache(Key) ->
    case get({mw_tool_selector_cache, Key}) of
        undefined -> undefined;
        Value ->
            put(mw_tool_selector_cache_hits,
                get(mw_tool_selector_cache_hits) + 1),
            {ok, Value}
    end.

%% @private 设置缓存
-spec put_cache(term(), term()) -> ok.
put_cache(Key, Value) ->
    put({mw_tool_selector_cache, Key}, Value),
    ok.
