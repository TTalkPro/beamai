%%%-------------------------------------------------------------------
%%% @doc LLM Tool Emulator Middleware - 工具模拟器
%%%
%%% 使用 LLM 模拟工具执行，用于开发测试阶段。
%%%
%%% == 功能特性 ==
%%%
%%% - 使用 LLM 模拟真实工具的行为
%%% - 支持选择性模拟（部分工具真实执行）
%%% - 可配置模拟 LLM
%%% - 支持模拟延迟
%%% - 开发测试利器
%%%
%%% == 配置示例 ==
%%%
%%% ```erlang
%%% {middleware_tool_emulator, #{
%%%     %% 启用模拟的工具（空列表表示全部模拟）
%%%     emulate_tools => [<<"api_call">>, <<"database_query">>],
%%%
%%%     %% 不模拟的工具（真实执行）
%%%     exclude_tools => [<<"calculator">>],
%%%
%%%     %% 模拟 LLM 配置
%%%     emulator_llm => #{
%%%         provider => openai,
%%%         model => <<"gpt-3.5-turbo">>
%%%     },
%%%
%%%     %% 模拟延迟（毫秒，模拟真实工具延迟）
%%%     emulate_delay => 500,
%%%
%%%     %% 模拟提示词模板
%%%     prompt_template => <<"...">>,
%%%
%%%     %% 调试模式
%%%     debug => false
%%% }}
%%% ```
%%%
%%% == 使用场景 ==
%%%
%%% - 开发阶段测试 Agent 行为
%%% - 模拟昂贵或慢速的外部 API
%%% - 测试错误处理逻辑
%%% - 演示和原型验证
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(middleware_tool_emulator).

-behaviour(beamai_middleware).

%% Middleware 回调
-export([init/1, before_tools/2]).

%% 工具函数
-export([
    emulate_tool/4,
    should_emulate/2
]).

%%====================================================================
%% Middleware 回调
%%====================================================================

%% @doc 初始化 Middleware 状态
-spec init(map()) -> map().
init(Opts) ->
    #{
        emulate_tools => maps:get(emulate_tools, Opts, []),  %% 空表示全部
        exclude_tools => maps:get(exclude_tools, Opts, []),
        emulator_llm => maps:get(emulator_llm, Opts, undefined),
        emulate_delay => maps:get(emulate_delay, Opts, 0),
        prompt_template => maps:get(prompt_template, Opts, default_prompt_template()),
        error_rate => maps:get(error_rate, Opts, 0.0),  %% 模拟错误率
        debug => maps:get(debug, Opts, false)
    }.

%% @doc 工具执行前拦截并模拟
-spec before_tools(map(), map()) -> beamai_middleware:middleware_result().
before_tools(State, MwState) ->
    #{debug := Debug} = MwState,

    ToolCalls = graph:get(State, tool_calls, []),

    case ToolCalls of
        [] ->
            ok;
        _ ->
            %% 检查并替换需要模拟的工具
            {NewToolCalls, EmulatedResults} = process_tool_calls(ToolCalls, State, MwState),

            case EmulatedResults of
                [] ->
                    ok;
                _ ->
                    case Debug of
                        true ->
                            EmulatedNames = [maps:get(tool_name, R, unknown) || R <- EmulatedResults],
                            logger:info("[ToolEmulator] 模拟了 ~p 个工具: ~p",
                                       [length(EmulatedResults), EmulatedNames]);
                        false ->
                            ok
                    end,

                    %% 更新工具调用和预填结果
                    {update, #{
                        tool_calls => NewToolCalls,
                        mw_emulated_results => EmulatedResults,
                        mw_emulation_count =>
                            graph:get(State, mw_emulation_count, 0) + length(EmulatedResults)
                    }}
            end
    end.

%%====================================================================
%% 工具函数
%%====================================================================

%% @doc 模拟工具执行
-spec emulate_tool(binary(), map(), map(), map()) -> {ok, binary()} | {error, term()}.
emulate_tool(ToolName, ToolInput, ToolDef, MwState) ->
    #{emulator_llm := EmulatorLLM,
      emulate_delay := Delay,
      prompt_template := PromptTemplate,
      error_rate := ErrorRate} = MwState,

    %% 模拟延迟
    case Delay > 0 of
        true -> timer:sleep(Delay);
        false -> ok
    end,

    %% 模拟错误
    case rand:uniform() < ErrorRate of
        true ->
            {error, {emulated_error, <<"Simulated tool failure">>}};
        false ->
            %% 使用 LLM 生成模拟结果
            generate_emulated_result(ToolName, ToolInput, ToolDef, PromptTemplate, EmulatorLLM)
    end.

%% @doc 检查工具是否应该被模拟
-spec should_emulate(binary(), map()) -> boolean().
should_emulate(ToolName, MwState) ->
    #{emulate_tools := EmulateTools, exclude_tools := ExcludeTools} = MwState,

    %% 首先检查排除列表
    case lists:member(ToolName, ExcludeTools) of
        true ->
            false;
        false ->
            %% 检查模拟列表（空表示全部模拟）
            case EmulateTools of
                [] -> true;
                _ -> lists:member(ToolName, EmulateTools)
            end
    end.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 默认模拟提示词模板
-spec default_prompt_template() -> binary().
default_prompt_template() ->
    <<"You are simulating the execution of a tool for testing purposes.\n\n"
      "Tool: {{tool_name}}\n"
      "Description: {{tool_description}}\n"
      "Input: {{tool_input}}\n\n"
      "Generate a realistic response that this tool might return. "
      "The response should be consistent with the tool's purpose and the given input.\n\n"
      "Important:\n"
      "- Return ONLY the tool output, no explanation\n"
      "- Make the output realistic and useful\n"
      "- If the tool would return JSON, return valid JSON\n"
      "- If the tool would return plain text, return plain text\n\n"
      "Simulated output:">>.

%% @private 处理工具调用
-spec process_tool_calls([map()], map(), map()) -> {[map()], [map()]}.
process_tool_calls(ToolCalls, State, MwState) ->
    Tools = graph:get(State, tools, []),

    {ProcessedCalls, EmulatedResults} = lists:foldl(fun(ToolCall, {Calls, Results}) ->
        ToolName = extract_tool_name(ToolCall),

        case should_emulate(ToolName, MwState) of
            false ->
                %% 不模拟，保持原样
                {[ToolCall | Calls], Results};
            true ->
                %% 模拟执行
                ToolInput = extract_tool_input(ToolCall),
                ToolDef = find_tool_def(ToolName, Tools),

                case emulate_tool(ToolName, ToolInput, ToolDef, MwState) of
                    {ok, EmulatedOutput} ->
                        %% 创建模拟结果
                        Result = #{
                            tool_call_id => extract_tool_call_id(ToolCall),
                            tool_name => ToolName,
                            result => EmulatedOutput,
                            emulated => true
                        },
                        %% 标记工具调用已模拟
                        MarkedCall = ToolCall#{mw_emulated => true},
                        {[MarkedCall | Calls], [Result | Results]};
                    {error, Reason} ->
                        %% 模拟失败，作为错误结果
                        Result = #{
                            tool_call_id => extract_tool_call_id(ToolCall),
                            tool_name => ToolName,
                            result => {error, Reason},
                            emulated => true
                        },
                        MarkedCall = ToolCall#{mw_emulated => true},
                        {[MarkedCall | Calls], [Result | Results]}
                end
        end
    end, {[], []}, ToolCalls),

    {lists:reverse(ProcessedCalls), lists:reverse(EmulatedResults)}.

%% @private 生成模拟结果
-spec generate_emulated_result(binary(), map(), map() | undefined, binary(), map() | undefined) ->
    {ok, binary()} | {error, term()}.
generate_emulated_result(ToolName, ToolInput, ToolDef, PromptTemplate, EmulatorLLM) ->
    %% 构建提示词
    ToolDescription = case ToolDef of
        undefined -> <<"Unknown tool">>;
        #{description := Desc} -> Desc;
        #{<<"description">> := Desc} -> Desc;
        _ -> <<"No description">>
    end,

    InputJson = try
        jsx:encode(ToolInput)
    catch
        _:_ -> <<"(unable to serialize input)">>
    end,

    Prompt1 = binary:replace(PromptTemplate, <<"{{tool_name}}">>, ToolName),
    Prompt2 = binary:replace(Prompt1, <<"{{tool_description}}">>, ToolDescription),
    Prompt3 = binary:replace(Prompt2, <<"{{tool_input}}">>, InputJson),

    %% 调用 LLM
    LLMConfig = case EmulatorLLM of
        undefined -> #{};
        Config -> Config
    end,

    Messages = [#{role => user, content => Prompt3}],
    Request = #{messages => Messages},

    case catch llm_client:chat(LLMConfig, Request) of
        {ok, Response} ->
            Content = extract_llm_content(Response),
            {ok, Content};
        {error, Reason} ->
            %% LLM 调用失败，返回默认模拟结果
            DefaultResult = generate_default_result(ToolName, ToolInput),
            {ok, DefaultResult};
        {'EXIT', _Reason} ->
            DefaultResult = generate_default_result(ToolName, ToolInput),
            {ok, DefaultResult}
    end.

%% @private 生成默认模拟结果
-spec generate_default_result(binary(), map()) -> binary().
generate_default_result(ToolName, ToolInput) ->
    InputStr = try
        jsx:encode(ToolInput)
    catch
        _:_ -> <<"unknown">>
    end,
    <<"[Emulated result for ", ToolName/binary, " with input: ", InputStr/binary, "]">>.

%% @private 提取工具名称
-spec extract_tool_name(map()) -> binary().
extract_tool_name(#{<<"function">> := #{<<"name">> := Name}}) -> Name;
extract_tool_name(#{function := #{name := Name}}) -> Name;
extract_tool_name(#{<<"name">> := Name}) -> Name;
extract_tool_name(#{name := Name}) -> Name;
extract_tool_name(_) -> <<"unknown">>.

%% @private 提取工具输入
-spec extract_tool_input(map()) -> map().
extract_tool_input(#{<<"function">> := #{<<"arguments">> := Args}}) when is_binary(Args) ->
    try jsx:decode(Args, [return_maps]) catch _:_ -> #{} end;
extract_tool_input(#{<<"function">> := #{<<"arguments">> := Args}}) when is_map(Args) ->
    Args;
extract_tool_input(#{function := #{arguments := Args}}) when is_binary(Args) ->
    try jsx:decode(Args, [return_maps]) catch _:_ -> #{} end;
extract_tool_input(#{function := #{arguments := Args}}) when is_map(Args) ->
    Args;
extract_tool_input(#{<<"input">> := Input}) -> Input;
extract_tool_input(#{input := Input}) -> Input;
extract_tool_input(_) -> #{}.

%% @private 提取工具调用 ID
-spec extract_tool_call_id(map()) -> binary().
extract_tool_call_id(#{<<"id">> := Id}) -> Id;
extract_tool_call_id(#{id := Id}) -> Id;
extract_tool_call_id(_) -> <<"emulated_", (integer_to_binary(erlang:unique_integer([positive])))/binary>>.

%% @private 查找工具定义
-spec find_tool_def(binary(), [map()]) -> map() | undefined.
find_tool_def(ToolName, Tools) ->
    case lists:filter(fun(T) ->
        Name = maps:get(name, T, maps:get(<<"name">>, T, undefined)),
        Name =:= ToolName
    end, Tools) of
        [ToolDef | _] -> ToolDef;
        [] -> undefined
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
