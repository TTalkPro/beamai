%%%-------------------------------------------------------------------
%%% @doc MCP 应用模块
%%%
%%% 实现 OTP application 行为，管理 MCP 应用的启动和停止。
%%%
%%% == 功能概述 ==
%%%
%%% MCP (Model Context Protocol) 是 Anthropic 定义的 AI 模型集成协议。
%%% 本应用提供:
%%% - MCP Client: 连接外部 MCP 服务器
%%% - MCP Server: 作为 MCP 服务器对外提供服务
%%%
%%% == 支持的传输协议 ==
%%%
%%% Client 支持:
%%% - Stdio: 通过标准输入输出与本地进程通信
%%% - SSE: Server-Sent Events 流式传输
%%% - Streamable HTTP: HTTP POST + SSE 响应
%%%
%%% Server 支持:
%%% - SSE: 对外提供 SSE 流
%%% - Streamable HTTP: 接收 POST 请求，返回 SSE 响应
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_mcp_app).

-behaviour(application).

%%====================================================================
%% API 导出
%%====================================================================

-export([start/2, stop/1]).

%%====================================================================
%% Application 回调
%%====================================================================

%% @doc 启动 MCP 应用
%%
%% @param StartType 启动类型
%% @param StartArgs 启动参数
%% @returns {ok, Pid} | {error, Reason}
-spec start(application:start_type(), term()) ->
    {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    beamai_mcp_sup:start_link().

%% @doc 停止 MCP 应用
%%
%% @param State 应用状态
%% @returns ok
-spec stop(term()) -> ok.
stop(_State) ->
    ok.
