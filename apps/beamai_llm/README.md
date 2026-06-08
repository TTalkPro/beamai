# Agent LLM

[English](README_EN.md) | 中文

大语言模型（LLM）客户端层，支持多个 LLM 提供商。

## 支持的提供商

| 提供商 | 模块 | API 模式 | 说明 |
|--------|------|----------|------|
| OpenAI | `beamai_llm_provider_openai` | OpenAI | GPT-4, GPT-3.5-turbo 等 |
| Anthropic | `beamai_llm_provider_anthropic` | Anthropic | Claude 3, Claude 2 等 |
| DeepSeek | `beamai_llm_provider_deepseek` | OpenAI 兼容 | deepseek-chat, deepseek-reasoner |
| Ollama | `beamai_llm_provider_ollama` | OpenAI 兼容 | 本地模型部署 |
| 智谱 AI | `beamai_llm_provider_zhipu` | OpenAI 兼容 | GLM-4.7 等国产模型 |
| 阿里云百炼 | `beamai_llm_provider_bailian` | DashScope 原生 | 通义千问系列 (qwen-plus, qwen-max 等) |

## 模块概览

### 客户端

- **llm_client** - LLM 客户端主入口
- **beamai_llm_http_client** - HTTP 请求处理
- **beamai_llm_helper** - 辅助函数

### 提供商

- **beamai_llm_provider_behaviour** - 提供商行为定义
- **beamai_llm_provider_common** - 提供商公共函数（URL 构建、认证头、事件累加等）
- **beamai_llm_provider_openai** - OpenAI 实现
- **beamai_llm_provider_anthropic** - Anthropic 实现
- **beamai_llm_provider_deepseek** - DeepSeek 实现 (OpenAI 兼容 API)
- **beamai_llm_provider_ollama** - Ollama 实现
- **beamai_llm_provider_zhipu** - 智谱 AI 实现
- **beamai_llm_provider_bailian** - 阿里云百炼实现 (DashScope 原生 API)

### 适配器

- **beamai_llm_message_adapter** - 消息格式适配（含多模态：图片/音频/PDF）
- **beamai_llm_tool_adapter** - 工具格式适配
- **beamai_llm_response_parser** - Provider 响应解析（OpenAI/Anthropic/DashScope 等格式 → 统一 `beamai_llm_response` 结构）

### 错误处理

- **beamai_llm_error** - 统一错误结构。把各 Provider/HTTP 层杂乱的错误（`{http_error, ...}` / `{api_error, ...}` / `{request_failed, ...}` 等）归一化为带类型/状态码/是否可重试/建议退避的结构化 map

> **注意**: 核心响应数据结构 `beamai_llm_response` 位于 `beamai_core`，提供统一的类型定义和访问器。

> **流式一致性**: 所有 Provider（含 zhipu/bailian/ollama）的同步与流式调用均返回统一的 `beamai_llm_response` 结构，含分片工具调用累加、usage 统计、reasoning/thinking 内容。

## API 文档

### llm_client

```erlang
%% 发送聊天请求
llm_client:chat(Config, Messages) -> {ok, Response} | {error, Reason}.

%% 发送带工具的聊天请求
llm_client:with_tools(Config, Messages, Tools) -> {ok, Response} | {error, Reason}.

%% 简单聊天（单轮对话）
llm_client:simple_chat(Config, Prompt) -> {ok, Content} | {error, Reason}.

%% 流式聊天
llm_client:stream_chat(Config, Messages, Callback) -> {ok, Response} | {error, Reason}.
```

### 创建配置

LLM 配置必须使用 `llm_client:create/2` 创建：

```erlang
%% 创建 LLM 配置
LLM = llm_client:create(Provider, #{
    model => <<"gpt-4">>,                 %% 模型名称（必需）
    api_key => <<"sk-xxx">>,              %% API 密钥（必需，ollama 除外）
    base_url => <<"https://...">>,        %% 可选，自定义 API 地址
    temperature => 0.7,                   %% 可选，温度参数
    max_tokens => 4096                    %% 可选，最大 token 数
}).

%% Provider 类型：openai | anthropic | deepseek | ollama | zhipu | bailian
```

## 使用示例

### 基本聊天

```erlang
%% 创建 OpenAI 配置
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% 发送消息
Messages = [
    #{role => system, content => <<"You are a helpful assistant.">>},
    #{role => user, content => <<"Hello!">>}
],

{ok, Response} = llm_client:chat(LLM, Messages),
Content = maps:get(content, Response).
```

### 使用 DeepSeek

DeepSeek API 与 OpenAI API 兼容，支持 `deepseek-chat` 和 `deepseek-reasoner` 模型。

```erlang
%% 创建 DeepSeek 配置
LLM = llm_client:create(deepseek, #{
    model => <<"deepseek-chat">>,
    api_key => list_to_binary(os:getenv("DEEPSEEK_API_KEY"))
}),

%% 发送消息
Messages = [
    #{role => user, content => <<"你好！"/utf8>>}
],

{ok, Response} = llm_client:chat(LLM, Messages).
```

**支持的模型：**

| 模型 | 说明 | 推荐场景 |
|------|------|----------|
| `deepseek-chat` | 通用对话模型（默认） | 日常对话、代码生成 |
| `deepseek-reasoner` | 推理增强模型 | 复杂推理、数学问题 |

**特性：**
- 完整支持工具调用（Function Calling），流式分片工具调用自动累加
- 支持流式输出
- OpenAI 兼容 API，响应格式与 OpenAI 一致
- deepseek-reasoner 的思维链内容通过 `beamai_llm_response:reasoning_content/1` 访问（同步与流式均支持）
- 支持 `frequency_penalty` / `presence_penalty` / `stop` / `logprobs` / `top_logprobs` / `response_format` 配置参数
- deepseek-reasoner 自动剔除不支持的参数（temperature/top_p/penalty/logprobs），避免 400 错误
- 上下文硬盘缓存统计：`usage.details` 中的 `prompt_cache_hit_tokens` / `prompt_cache_miss_tokens`

**DeepSeek 特有机制（beta）：**

```erlang
%% Chat Prefix Completion：末尾 assistant 消息标 prefix => true，
%% 强制模型从该内容续写（自动路由到 /beta 端点）
Messages = [
    #{role => user, content => <<"写一段 Python 快排"/utf8>>},
    #{role => assistant, content => <<"```python\n">>, prefix => true}
],
{ok, Resp} = llm_client:chat(LLM, Messages),

%% FIM 填空补全：根据前缀 + 后缀补全中间内容（代码补全场景）
{ok, Resp2} = beamai_llm_provider_deepseek:fim(Config, #{
    prompt => <<"def fib(n):">>,
    suffix => <<"    return fib(n-1) + fib(n-2)">>
}),
Completion = beamai_llm_response:content(Resp2),

%% 流式 FIM
{ok, Resp3} = beamai_llm_provider_deepseek:stream_fim(Config, #{prompt => P}, Callback).
```

### 使用阿里云百炼 (DashScope 原生 API)

阿里云百炼 Provider 使用 DashScope 原生 API，支持：
- 文本生成：`/api/v1/services/aigc/text-generation/generation`
- 多模态生成：`/api/v1/services/aigc/multimodal-generation/generation`（自动根据模型选择）

```erlang
%% 创建百炼配置（通义千问）
LLM = llm_client:create(bailian, #{
    model => <<"qwen-plus">>,  %% 推荐：均衡性价比
    api_key => list_to_binary(os:getenv("BAILIAN_API_KEY"))
}),

%% 注意：中文字符串需要 /utf8 后缀
Messages = [
    #{role => user, content => <<"你好！"/utf8>>}
],

{ok, Response} = llm_client:chat(LLM, Messages).
```

**支持的模型：**

| 模型 | 说明 | 推荐场景 |
|------|------|----------|
| `qwen-max` | 旗舰模型，效果最好 | 复杂推理、专业任务 |
| `qwen-plus` | 均衡模型（推荐） | 通用场景 |
| `qwen-turbo` | 快速模型，成本最低 | 简单任务、高并发 |
| `qwen-vl-plus` | 视觉语言模型 | 图像理解（自动使用多模态端点） |

**特有功能：**

```erlang
%% 启用联网搜索
LLM = llm_client:create(bailian, #{
    model => <<"qwen-plus">>,
    api_key => ApiKey,
    enable_search => true  %% 启用联网搜索
}).
```

### 使用智谱 AI

智谱 AI 支持两种调用方式：

**方式一：原生 API（推荐）**

```erlang
LLM = llm_client:create(zhipu, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY"))
}),

Messages = [
    #{role => user, content => <<"你好！"/utf8>>}
],

{ok, Response} = llm_client:chat(LLM, Messages).
```

**方式二：Anthropic 兼容接口**

```erlang
%% 智谱 AI 提供 Anthropic 兼容接口
LLM = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

Messages = [
    #{role => user, content => <<"你好！"/utf8>>}
],

{ok, Response} = llm_client:chat(LLM, Messages).
```

### 工具调用

```erlang
%% 定义工具
Tools = [
    #{
        type => function,
        function => #{
            name => <<"get_weather">>,
            description => <<"查询城市天气"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    <<"city">> => #{
                        type => string,
                        description => <<"城市名称"/utf8>>
                    }
                },
                required => [<<"city">>]
            }
        }
    }
],

%% 发送带工具的请求
{ok, Response} = llm_client:with_tools(LLM, Messages, Tools),

%% 检查是否有工具调用
case maps:get(tool_calls, Response, []) of
    [] ->
        %% 普通文本响应
        Content = maps:get(content, Response);
    ToolCalls ->
        %% 处理工具调用
        lists:foreach(fun(#{name := Name, arguments := Args}) ->
            io:format("Tool: ~s, Args: ~s~n", [Name, Args])
        end, ToolCalls)
end.
```

### 流式响应

```erlang
%% 流式回调函数
Callback = fun(Event) ->
    case Event of
        #{<<"output">> := #{<<"choices">> := [#{<<"message">> := #{<<"content">> := Content}} | _]}} ->
            io:format("~ts", [Content]);
        _ ->
            ok
    end
end,

llm_client:stream_chat(LLM, Messages, Callback).
```

> 回调收到的是各 Provider 的**原始 SSE 事件**（用于实时 token 展示）；而 `stream_chat` 的最终返回值是与同步一致的统一 `beamai_llm_response`。

## 高级能力

### 多模态输入（图片 / 音频 / PDF）

消息的 `content` 除 binary 文本外，还可以是**内容部件列表**，由 `beamai_llm_message_adapter` 自动转换为各 Provider 的格式（OpenAI `image_url` / `input_audio`，Anthropic `image` / `document`）。纯文本 binary 完全向后兼容。

```erlang
Messages = [
    #{role => user, content => [
        #{type => text, text => <<"这张图里是什么？"/utf8>>},
        %% 图片：base64
        #{type => image, source => #{type => base64,
            media_type => <<"image/png">>, data => Base64Png}},
        %% 图片：url（也支持 #{type => url, url => <<"https://...">>}）
        #{type => image, source => #{type => url, url => <<"https://x/y.jpg">>}}
    ]}
],
{ok, Resp} = llm_client:chat(LLM, Messages).
```

- **图片**：OpenAI（gpt-4o 等）/ Anthropic / DeepSeek 均支持
- **音频**（`#{type => audio, data => Data, format => <<"wav">>}`）：OpenAI `input_audio`（Anthropic 不支持，自动忽略）
- **PDF 文档**（`#{type => document, source => ...}`）：Anthropic `document`；加 `citations => true` 可启用引用

### Anthropic Prompt 缓存（cache_control）

通过 `cache_control` 配置在 system / 工具定义 / 会话历史上注入 `cache_control: ephemeral` 断点，降低重复上下文的费用。

```erlang
LLM = llm_client:create(anthropic, #{
    model => <<"claude-sonnet-4-5-20250929">>,
    api_key => ApiKey,
    %% 策略：none | system_only | tools_only | system_and_tools | conversation
    cache_control => system_and_tools
    %% 或带 TTL：#{strategy => system_only, ttl => <<"1h">>}
}).
```

缓存命中统计在 `usage.details`（`cache_creation_input_tokens` / `cache_read_input_tokens`）。

### Anthropic 内置 Web Search

启用服务端内置搜索工具；Claude 自动搜网并在一次调用内生成回答。

```erlang
LLM = llm_client:create(anthropic, #{
    model => <<"claude-sonnet-4-5-20250929">>,
    api_key => ApiKey,
    %% true 或 #{max_uses => N, allowed_domains => [...], blocked_domains => [...]}
    web_search => #{max_uses => 3}
}),
{ok, Resp} = llm_client:chat(LLM, Messages),
%% 搜索结果（title/url/page_age 等）落到 metadata
Results = maps:get(web_search_results, beamai_llm_response:metadata(Resp), []).
```

### Anthropic 引用（Citations）

document 部件设 `citations => true` 后，响应中的引用会汇总到 `metadata.citations`。

```erlang
Citations = maps:get(citations, beamai_llm_response:metadata(Resp), []).
```

### 速率限制响应头

同步与流式调用都会把响应头中的速率限制信息（`anthropic-ratelimit-*` / `x-ratelimit-*` / `retry-after`）解析到 `metadata.rate_limit`。

```erlang
case maps:get(rate_limit, beamai_llm_response:metadata(Resp), undefined) of
    undefined -> ok;
    RL -> io:format("剩余请求数: ~p~n", [maps:get(<<"requests-remaining">>, RL, undefined)])
end.
```

### 自动重试与 Retry-After

`beamai_chat_completion:chat/3` 内置指数退避重试；遇到 429 / 5xx 时，若服务端返回 `Retry-After` 头则**按其建议退避**（上限 60s），否则按 `retry_delay * 重试次数` 退避。

```erlang
{ok, Resp} = beamai_chat_completion:chat(LLM, Messages, #{
    max_retries => 3,          %% 默认 3
    retry_delay => 1000,       %% 基础退避 ms，默认 1000
    on_retry => fun(State) ->  %% 可选，重试回调
        io:format("第 ~p 次重试，延迟 ~pms：~p~n",
                  [maps:get(attempt, State), maps:get(delay, State), maps:get(error, State)])
    end
}).
```

### 统一错误结构（beamai_llm_error）

把各 Provider 杂乱的错误归一化，便于上层一致判断类型 / 是否可重试 / 建议退避。各 Provider 仍返回原 legacy 错误元组，按需调用归一化：

```erlang
case llm_client:chat(LLM, Messages) of
    {ok, Resp} -> ...;
    {error, Reason} ->
        Err = beamai_llm_error:from_reason(Reason, anthropic),
        case beamai_llm_error:type(Err) of
            rate_limit -> %% 限流，可重试
                RetryMs = beamai_llm_error:retry_after_ms(Err);
            auth       -> %% 鉴权失败（API key 等），不可重试
                io:format("~ts~n", [beamai_llm_error:message(Err)]);
            _          -> ...
        end
end.
```

错误类型 `type`：`rate_limit | server_error | client_error | auth | timeout | network | invalid_response | api_error | unknown`。访问器：`type/1`、`status/1`、`message/1`、`provider/1`、`retryable/1`、`retry_after_ms/1`、`raw/1`。

## 环境变量

| 变量名 | 说明 |
|--------|------|
| `OPENAI_API_KEY` | OpenAI API 密钥 |
| `ANTHROPIC_API_KEY` | Anthropic API 密钥 |
| `DEEPSEEK_API_KEY` | DeepSeek API 密钥 |
| `ZHIPU_API_KEY` | 智谱 AI API 密钥 |
| `BAILIAN_API_KEY` | 阿里云百炼 API 密钥 (DashScope) |
| `OLLAMA_BASE_URL` | Ollama 服务地址（默认 http://localhost:11434） |

## Provider 技术细节

### DeepSeek (OpenAI 兼容 API)

DeepSeek API 完全兼容 OpenAI API 格式，使用相同的请求/响应结构。

**API 端点：**
- 默认地址：`https://api.deepseek.com`
- 聊天接口：`/chat/completions`

**请求格式：**
```json
{
  "model": "deepseek-chat",
  "messages": [...],
  "max_tokens": 4096,
  "temperature": 1.0,
  "tools": [...],
  "stream": false
}
```

**响应格式：**
```json
{
  "id": "xxx",
  "object": "chat.completion",
  "model": "deepseek-chat",
  "choices": [{
    "index": 0,
    "message": {
      "role": "assistant",
      "content": "...",
      "tool_calls": [...]
    },
    "finish_reason": "stop"
  }],
  "usage": {
    "prompt_tokens": 10,
    "completion_tokens": 20,
    "total_tokens": 30
  }
}
```

### 阿里云百炼 (DashScope 原生 API)

**请求格式：**
```json
{
  "model": "qwen-plus",
  "input": {
    "messages": [...]
  },
  "parameters": {
    "result_format": "message",
    "max_tokens": 4096,
    "temperature": 0.7
  }
}
```

**响应格式：**
```json
{
  "output": {
    "choices": [{
      "message": {
        "role": "assistant",
        "content": "...",
        "tool_calls": [...]
      },
      "finish_reason": "stop"
    }]
  },
  "usage": {
    "input_tokens": 26,
    "output_tokens": 66,
    "total_tokens": 92
  },
  "request_id": "xxx"
}
```

**流式输出：**
- 请求头：`X-DashScope-SSE: enable`
- 参数：`parameters.incremental_output: true`

## 架构说明

### Provider 公共模块 (beamai_llm_provider_common)

所有 Provider 共享的通用函数已抽取到 `beamai_llm_provider_common` 模块：

```erlang
%% URL 构建
beamai_llm_provider_common:build_url(Config, DefaultEndpoint, DefaultBaseUrl) -> URL.

%% Bearer 认证头
beamai_llm_provider_common:build_bearer_auth_headers(Config) -> Headers.

%% 可选参数处理
beamai_llm_provider_common:maybe_add_stream(Body, Request) -> NewBody.
beamai_llm_provider_common:maybe_add_tools(Body, Request) -> NewBody.
beamai_llm_provider_common:maybe_add_tool_choice(Body, Request) -> NewBody.
beamai_llm_provider_common:maybe_add_top_p(Body, Request) -> NewBody.
beamai_llm_provider_common:maybe_add_params(Body, Source, Specs) -> NewBody.

%% OpenAI 格式事件累加（流式响应）
%% 支持 content / reasoning_content / 分片 tool_calls / usage 累加
beamai_llm_provider_common:accumulate_openai_event(Event, Acc) -> NewAcc.
beamai_llm_provider_common:finalize_openai_stream(Acc, Provider) -> {ok, Response}.

%% Anthropic 格式事件累加（流式响应）
%% 支持 message_start / content_block_* / message_delta 事件流，
%% 包括 tool_use 块的 input_json_delta 拼接和 thinking 块累加
beamai_llm_provider_common:accumulate_anthropic_event(Event, Acc) -> NewAcc.
beamai_llm_provider_common:finalize_anthropic_stream(Acc) -> {ok, Response}.

%% 工具调用解析
beamai_llm_provider_common:parse_tool_calls(Message) -> [ToolCall].
beamai_llm_provider_common:parse_single_tool_call(Call) -> ToolCall.

%% 使用统计解析
beamai_llm_provider_common:parse_usage(Usage) -> #{prompt_tokens, completion_tokens, total_tokens}.

%% 响应头：速率限制与 Retry-After（用作 on_headers 处理器 / 重试退避）
beamai_llm_provider_common:rate_limit_metadata(Headers) -> #{rate_limit => map()} | #{}.
beamai_llm_provider_common:retry_after_ms(Headers) -> non_neg_integer() | undefined.
```

### LLM 响应结构 (beamai_llm_response)

> **注意**: `beamai_llm_response` 模块已移至 `beamai_core`，作为核心数据结构被 Kernel 层消费。

统一的 LLM 响应结构，抽象不同 Provider 的响应差异：

```erlang
%% 通过 Parser 函数解析响应（由 beamai_llm_http_client 内部使用）
beamai_llm_response_parser:parser_openai()      %% OpenAI 格式
beamai_llm_response_parser:parser_anthropic()   %% Anthropic 格式
beamai_llm_response_parser:parser_deepseek()    %% DeepSeek 格式（含 reasoning_content）
beamai_llm_response_parser:parser_dashscope()   %% 阿里云百炼 DashScope 格式
beamai_llm_response_parser:parser_ollama()      %% Ollama 格式
beamai_llm_response_parser:parser_zhipu()       %% 智谱特定格式（含 reasoning_content）

%% 统一访问接口
Content = beamai_llm_response:content(Response),
ToolCalls = beamai_llm_response:tool_calls(Response),
HasTools = beamai_llm_response:has_tool_calls(Response),
Usage = beamai_llm_response:usage(Response),

%% 标准化响应格式
#{
    id => binary(),              %% 请求 ID
    model => binary(),           %% 模型名称
    provider => atom(),          %% Provider 类型
    content => binary() | null,  %% 响应内容
    content_blocks => [map()],   %% 结构化内容块（text/tool_use/thinking 等）
    tool_calls => [map()],       %% 工具调用列表
    finish_reason => atom(),     %% 结束原因
    usage => #{..., details => #{...}},  %% Token 统计（details 含缓存命中等）
    metadata => #{...},          %% Provider 特有信息（见下）
    raw => map()                 %% 原始响应
}
```

**`metadata` 中可能出现的键（按能力）：**

| 键 | 来源 | 说明 |
|----|------|------|
| `reasoning_content` | deepseek-reasoner / GLM | 思维链内容（也可用 `beamai_llm_response:reasoning_content/1` 访问） |
| `rate_limit` | 同步/流式响应头 | `#{<<"requests-remaining">> => ..., ...}` |
| `citations` | Anthropic | 引用列表 |
| `web_search_results` | Anthropic Web Search | 搜索结果（title/url/page_age 等） |

## 依赖

- beamai_core
- jsx - JSON 编解码
- hackney - HTTP 客户端

## 许可证

Apache-2.0
