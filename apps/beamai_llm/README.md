# Agent LLM

[English](README_EN.md) | 中文

大语言模型（LLM）客户端层，支持多个 LLM 提供商。

## 支持的提供商

| 提供商 | 模块 | API 模式 | 说明 |
|--------|------|----------|------|
| OpenAI | `llm_provider_openai` | OpenAI | GPT-4, GPT-3.5-turbo 等 |
| Anthropic | `llm_provider_anthropic` | Anthropic | Claude 3, Claude 2 等 |
| DeepSeek | `llm_provider_deepseek` | OpenAI 兼容 | deepseek-chat, deepseek-reasoner |
| Ollama | `llm_provider_ollama` | OpenAI 兼容 | 本地模型部署 |
| 智谱 AI | `llm_provider_zhipu` | OpenAI 兼容 | GLM-4.7 等国产模型 |
| 阿里云百炼 | `llm_provider_bailian` | DashScope 原生 | 通义千问系列 (qwen-plus, qwen-max 等) |

## 模块概览

### 客户端

- **llm_client** - LLM 客户端主入口
- **llm_http_client** - HTTP 请求处理
- **llm_helper** - 辅助函数

### 提供商

- **llm_provider_behaviour** - 提供商行为定义
- **llm_provider_openai** - OpenAI 实现
- **llm_provider_anthropic** - Anthropic 实现
- **llm_provider_deepseek** - DeepSeek 实现 (OpenAI 兼容 API)
- **llm_provider_ollama** - Ollama 实现
- **llm_provider_zhipu** - 智谱 AI 实现
- **llm_provider_bailian** - 阿里云百炼实现 (DashScope 原生 API)

### 适配器

- **llm_message_adapter** - 消息格式适配
- **llm_tool_adapter** - 工具格式适配
- **llm_response_adapter** - 响应格式适配

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
- 完整支持工具调用（Function Calling）
- 支持流式输出
- OpenAI 兼容 API，响应格式与 OpenAI 一致

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

## 依赖

- beamai_core
- jsx - JSON 编解码
- hackney - HTTP 客户端

## 许可证

Apache-2.0
