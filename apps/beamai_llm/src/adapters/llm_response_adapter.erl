%%%-------------------------------------------------------------------
%%% @doc LLM 响应适配器模块
%%%
%%% 提供统一的响应解析功能，将不同 LLM Provider 的响应格式
%%% 转换为标准化的内部格式。
%%%
%%% == 支持的响应格式 ==
%%%
%%% - OpenAI 格式：GPT 系列、DeepSeek、智谱 GLM、Ollama（兼容模式）
%%% - Anthropic 格式：Claude 系列
%%%
%%% == 标准化响应格式 ==
%%%
%%% ```erlang
%%% #{
%%%     id => binary(),           %% 请求 ID
%%%     model => binary(),        %% 使用的模型名称
%%%     content => binary(),      %% 响应文本内容
%%%     tool_calls => [map()],    %% 工具调用列表（可选）
%%%     finish_reason => binary(),%% 结束原因（stop/tool_calls/length）
%%%     usage => #{               %% Token 使用统计
%%%         prompt_tokens => integer(),
%%%         completion_tokens => integer(),
%%%         total_tokens => integer()
%%%     }
%%% }
%%% ```
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 解析 OpenAI 格式响应
%%% {ok, Response} = llm_response_adapter:parse_openai(RawResponse),
%%% Content = maps:get(content, Response),
%%% ToolCalls = maps:get(tool_calls, Response),
%%%
%%% %% 解析 Anthropic 格式响应
%%% {ok, Response} = llm_response_adapter:parse_anthropic(RawResponse).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_response_adapter).

%% API
-export([parse_openai/1, parse_anthropic/1]).
-export([parse_tool_calls_openai/1, parse_tool_calls_anthropic/1]).
-export([parse_usage_openai/1, parse_usage_anthropic/1]).

%% 类型
-type chat_response() :: #{
    id := binary(),
    model := binary(),
    content := binary() | null,
    tool_calls => [map()],
    finish_reason := binary(),
    usage := map()
}.

-export_type([chat_response/0]).

%%====================================================================
%% OpenAI 响应解析
%%====================================================================

%% @doc 解析 OpenAI API 响应
-spec parse_openai(map()) -> {ok, chat_response()} | {error, term()}.
parse_openai(#{<<"choices">> := [Choice | _]} = Resp) ->
    Message = maps:get(<<"message">>, Choice, #{}),
    {ok, #{
        id => maps:get(<<"id">>, Resp, <<>>),
        model => maps:get(<<"model">>, Resp, <<>>),
        content => maps:get(<<"content">>, Message, null),
        tool_calls => parse_tool_calls_openai(Message),
        finish_reason => maps:get(<<"finish_reason">>, Choice, <<>>),
        usage => parse_usage_openai(maps:get(<<"usage">>, Resp, #{}))
    }};
parse_openai(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
parse_openai(_) ->
    {error, invalid_response}.

%% @doc 解析 OpenAI 格式的工具调用
-spec parse_tool_calls_openai(map()) -> [map()].
parse_tool_calls_openai(#{<<"tool_calls">> := Calls}) when is_list(Calls) ->
    [parse_single_tool_call_openai(C) || C <- Calls];
parse_tool_calls_openai(_) ->
    [].

parse_single_tool_call_openai(#{<<"id">> := Id, <<"function">> := Func}) ->
    #{
        id => Id,
        name => maps:get(<<"name">>, Func, <<>>),
        arguments => maps:get(<<"arguments">>, Func, <<>>)
    };
parse_single_tool_call_openai(_) ->
    #{id => <<>>, name => <<>>, arguments => <<>>}.

%% @doc 解析 OpenAI 格式的使用统计
-spec parse_usage_openai(map()) -> map().
parse_usage_openai(Usage) ->
    #{
        prompt_tokens => maps:get(<<"prompt_tokens">>, Usage, 0),
        completion_tokens => maps:get(<<"completion_tokens">>, Usage, 0),
        total_tokens => maps:get(<<"total_tokens">>, Usage, 0)
    }.

%%====================================================================
%% Anthropic 响应解析
%%====================================================================

%% @doc 解析 Anthropic API 响应
-spec parse_anthropic(map()) -> {ok, chat_response()} | {error, term()}.
parse_anthropic(#{<<"content">> := ContentBlocks} = Resp) ->
    {Content, ToolCalls} = extract_content_and_tools(ContentBlocks),
    {ok, #{
        id => maps:get(<<"id">>, Resp, <<>>),
        model => maps:get(<<"model">>, Resp, <<>>),
        content => Content,
        tool_calls => ToolCalls,
        finish_reason => maps:get(<<"stop_reason">>, Resp, <<>>),
        usage => parse_usage_anthropic(maps:get(<<"usage">>, Resp, #{}))
    }};
parse_anthropic(#{<<"error">> := Error}) ->
    {error, {api_error, Error}};
parse_anthropic(_) ->
    {error, invalid_response}.

%% @doc 解析 Anthropic 格式的工具调用
-spec parse_tool_calls_anthropic([map()]) -> [map()].
parse_tool_calls_anthropic(ContentBlocks) ->
    [parse_tool_use(Block) || Block <- ContentBlocks,
     maps:get(<<"type">>, Block, <<>>) =:= <<"tool_use">>].

parse_tool_use(#{<<"id">> := Id, <<"name">> := Name, <<"input">> := Input}) ->
    #{
        id => Id,
        name => Name,
        arguments => jsx:encode(Input)
    };
parse_tool_use(_) ->
    #{id => <<>>, name => <<>>, arguments => <<>>}.

%% @doc 解析 Anthropic 格式的使用统计
-spec parse_usage_anthropic(map()) -> map().
parse_usage_anthropic(Usage) ->
    InputTokens = maps:get(<<"input_tokens">>, Usage, 0),
    OutputTokens = maps:get(<<"output_tokens">>, Usage, 0),
    #{
        prompt_tokens => InputTokens,
        completion_tokens => OutputTokens,
        total_tokens => InputTokens + OutputTokens
    }.

%%====================================================================
%% 内部函数
%%====================================================================

%% @doc 提取内容和工具调用
extract_content_and_tools(Blocks) ->
    extract_content_and_tools(Blocks, <<>>, []).

extract_content_and_tools([], Content, Tools) ->
    {Content, lists:reverse(Tools)};
extract_content_and_tools([#{<<"type">> := <<"text">>, <<"text">> := T} | Rest], Content, Tools) ->
    extract_content_and_tools(Rest, <<Content/binary, T/binary>>, Tools);
extract_content_and_tools([#{<<"type">> := <<"tool_use">>} = Block | Rest], Content, Tools) ->
    Tool = #{
        id => maps:get(<<"id">>, Block, <<>>),
        name => maps:get(<<"name">>, Block, <<>>),
        arguments => jsx:encode(maps:get(<<"input">>, Block, #{}))
    },
    extract_content_and_tools(Rest, Content, [Tool | Tools]);
extract_content_and_tools([_ | Rest], Content, Tools) ->
    extract_content_and_tools(Rest, Content, Tools).
