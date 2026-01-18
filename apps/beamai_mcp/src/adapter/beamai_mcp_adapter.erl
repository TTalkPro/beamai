%%%-------------------------------------------------------------------
%%% @doc MCP Tool 到 beamai_agent Tool 转换器
%%%
%%% 将 MCP 工具定义转换为 beamai_agent 兼容格式。
%%%
%%% == 主要功能 ==
%%%
%%% - 转换单个或批量 MCP 工具
%%% - 适配 MCP handler 为 beamai_agent handler
%%% - 提取和转换 MCP 内容格式
%%%
%%% == 使用示例 ==
%%%
%%% ```erlang
%%% %% 转换 MCP 工具
%%% McpTool = beamai_mcp_types:make_tool(...),
%%% AgentTool = beamai_mcp_adapter:mcp_tool_to_beamai_tool(McpTool),
%%%
%%% %% 使用转换后的工具
%%% {ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
%%%     tools => [AgentTool],
%%%     ...
%%% }).
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_adapter).

-include("beamai_mcp.hrl").

%%====================================================================
%% API 导出
%%====================================================================

-export([
    mcp_tool_to_beamai_tool/1,
    mcp_tools_to_beamai_tools/1,
    adapt_handler/1,
    adapt_handler/2
]).

%% 内容提取
-export([
    extract_text_from_contents/1,
    extract_content/1
]).

%% 错误格式化
-export([
    format_error/1
]).

%%====================================================================
%% API 函数
%%====================================================================

%% @doc 转换单个 MCP tool 为 beamai_agent tool
%%
%% MCP tool 使用 record 结构，beamai_agent 使用 map。
%% 主要转换：
%% - name 保持不变
%% - description 保持不变
%% - input_schema → parameters
%% - handler 需要适配返回格式
%%
%% @param McpTool MCP 工具 record
%% @returns beamai_agent 工具 map
-spec mcp_tool_to_beamai_tool(#mcp_tool{}) -> map().
mcp_tool_to_beamai_tool(#mcp_tool{
    name = Name,
    description = Description,
    input_schema = InputSchema,
    handler = Handler
}) ->
    #{
        name => Name,
        description => Description,
        parameters => InputSchema,
        handler => adapt_handler(Handler)
    }.

%% @doc 批量转换 MCP tools
%%
%% @param McpTools MCP 工具列表
%% @returns beamai_agent 工具列表
-spec mcp_tools_to_beamai_tools([#mcp_tool{}]) -> [map()].
mcp_tools_to_beamai_tools(McpTools) ->
    [mcp_tool_to_beamai_tool(T) || T <- McpTools].

%% @doc 适配 MCP handler 为 beamai_agent handler (arity/1)
%%
%% MCP handler 返回格式：
%% - 成功: {ok, [ContentObject]}
%% - 失败: {error, Reason}
%%
%% beamai_agent handler 期望：
%% - 返回字符串或 map
%% - 错误可以抛出异常或返回错误字符串
%%
%% @param McpHandler MCP 风格的处理函数
%% @returns beamai_agent 风格的处理函数
-spec adapt_handler(function()) -> function().
adapt_handler(McpHandler) ->
    fun(Args) ->
        case safe_call_mcp_handler(McpHandler, Args) of
            {ok, Contents} ->
                %% 提取文本内容
                extract_text_from_contents(Contents);
            {error, Reason} ->
                %% 转换为字符串错误
                format_error(Reason)
        end
    end.

%% @doc 适配 MCP handler 为 beamai_agent handler (arity/2，带 Context)
%%
%% 某些 beamai_agent handler 接受两个参数：Args 和 Context。
%% 这个版本忽略 Context，只传递 Args 给 MCP handler。
%%
%% @param McpHandler MCP 风格的处理函数
%% @returns beamai_agent 风格的处理函数（arity/2）
-spec adapt_handler(function(), ignore_context) -> function().
adapt_handler(McpHandler, ignore_context) ->
    fun(Args, _Context) ->
        case safe_call_mcp_handler(McpHandler, Args) of
            {ok, Contents} ->
                extract_text_from_contents(Contents);
            {error, Reason} ->
                format_error(Reason)
        end
    end.

%% @doc 从 MCP 内容对象列表中提取文本
%%
%% MCP 内容格式：
%% ```
%% [
%%   #{<<"type">> => <<"text">>, <<"text">> => <<"content">>},
%%   #{<<"type">> => <<"image">>, <<"data">> => <<"base64">>, ...}
%% ]
%% ```
%%
%% 当前实现只提取 text 类型的内容。
%%
%% @param Contents MCP 内容对象列表
%% @returns 提取的文本内容（二进制）
-spec extract_text_from_contents([map()]) -> binary().
extract_text_from_contents([]) ->
    <<"No content returned">>;
extract_text_from_contents(Contents) when is_list(Contents) ->
    TextParts = lists:filtermap(fun(Content) ->
        case maps:get(<<"type">>, Content, undefined) of
            <<"text">> ->
                {true, maps:get(<<"text">>, Content, <<>>)};
            _ ->
                %% 忽略其他类型（image, resource）
                false
        end
    end, Contents),
    case TextParts of
        [] -> <<"No text content">>;
        Parts -> iolist_to_binary(lists:join(<<"\n">>, Parts))
    end;
extract_text_from_contents(_) ->
    <<"Invalid content format">>.

%% @doc 从 MCP 工具调用结果中提取内容
%%
%% MCP 工具返回格式：
%% ```
%% #{
%%   <<"content">> => [ContentObject],
%%   <<"isError">> => false
%% }
%% ```
%%
%% @param Result MCP 工具调用结果
%% @returns 提取的文本内容
-spec extract_content(map()) -> binary().
extract_content(#{<<"content">> := Contents}) when is_list(Contents) ->
    extract_text_from_contents(Contents);
extract_content(#{<<"isError">> := true, <<"content">> := Contents}) ->
    %% 错误响应
    <<"Error: ", (extract_text_from_contents(Contents))/binary>>;
extract_content(Result) when is_map(Result) ->
    %% 尝试转换为 JSON
    try
        jsx:encode(Result)
    catch
        _:_ -> <<"Success">>
    end;
extract_content(_) ->
    <<"Success">>.

%% @doc 格式化错误为可读字符串
%%
%% 支持多种错误格式：
%% - MCP JSON-RPC 错误 map
%% - 二进制字符串
%% - 其他 Erlang term
%%
%% @param Error 错误信息
%% @returns 格式化的错误字符串
-spec format_error(term()) -> binary().
format_error(#{<<"message">> := Msg, <<"code">> := Code}) when is_binary(Msg) ->
    CodeBin = integer_to_binary(Code),
    <<"Error [", CodeBin/binary, "]: ", Msg/binary>>;
format_error(#{<<"message">> := Msg}) when is_binary(Msg) ->
    <<"Error: ", Msg/binary>>;
format_error(Error) when is_binary(Error) ->
    <<"Error: ", Error/binary>>;
format_error(Error) when is_atom(Error) ->
    ErrorBin = atom_to_binary(Error, utf8),
    <<"Error: ", ErrorBin/binary>>;
format_error(Error) ->
    ErrorBin = iolist_to_binary(io_lib:format("~p", [Error])),
    <<"Error: ", ErrorBin/binary>>.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 安全调用 MCP handler
%%
%% 捕获所有异常并转换为 {error, Reason} 格式。
-spec safe_call_mcp_handler(function(), map()) ->
    {ok, [map()]} | {error, term()}.
safe_call_mcp_handler(Handler, Args) ->
    try
        case erlang:fun_info(Handler, arity) of
            {arity, 1} ->
                Handler(Args);
            {arity, 0} ->
                Handler();
            _ ->
                {error, invalid_handler_arity}
        end
    catch
        Class:Reason:_Stack ->
            {error, {Class, Reason}}
    end.
