# Agent LLM

大语言模型（LLM）客户端层，支持多个 LLM 提供商。

## 支持的提供商

| 提供商 | 模块 | 说明 |
|--------|------|------|
| OpenAI | `llm_provider_openai` | GPT-4, GPT-3.5-turbo 等 |
| Anthropic | `llm_provider_anthropic` | Claude 3, Claude 2 等 |
| Ollama | `llm_provider_ollama` | 本地模型部署 |
| 智谱 AI | `llm_provider_zhipu` | GLM-4 等国产模型 |

## 模块概览

### 客户端

- **llm_client** - LLM 客户端主入口
- **llm_http_client** - HTTP 请求处理
- **llm_helper** - 辅助函数

### 提供商

- **llm_provider_behaviour** - 提供商行为定义
- **llm_provider_openai** - OpenAI 实现
- **llm_provider_anthropic** - Anthropic 实现
- **llm_provider_ollama** - Ollama 实现
- **llm_provider_zhipu** - 智谱 AI 实现

### 适配器

- **llm_message_adapter** - 消息格式适配
- **llm_tool_adapter** - 工具格式适配
- **llm_response_adapter** - 响应格式适配

## API 文档

### llm_client

```erlang
%% 发送聊天请求
llm_client:chat(Messages, Config) -> {ok, Response} | {error, Reason}.

%% 发送带工具的聊天请求
llm_client:chat_with_tools(Messages, Tools, Config) -> {ok, Response} | {error, Reason}.

%% 流式聊天
llm_client:chat_stream(Messages, Config, Callback) -> {ok, Response} | {error, Reason}.
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

%% Provider 类型：openai | anthropic | ollama | zhipu
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

### 使用智谱 AI（Anthropic 兼容接口）

```erlang
%% 智谱 AI 提供 Anthropic 兼容接口
LLM = llm_client:create(anthropic, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    base_url => <<"https://open.bigmodel.cn/api/anthropic">>
}),

Messages = [
    #{role => user, content => <<"你好！">>}
],

{ok, Response} = llm_client:chat(LLM, Messages).
```

### 工具调用

```erlang
%% 定义工具
Tools = [
    #{
        name => <<"calculator">>,
        description => <<"Perform mathematical calculations">>,
        parameters => #{
            type => object,
            properties => #{
                <<\"expression\">> => #{type => string, description => <<"Math expression">>}
            },
            required => [<<"expression">>]
        }
    }
],

%% 发送带工具的请求
{ok, Response} = llm_client:chat_with_tools(Messages, Tools, Config),

%% 检查是否有工具调用
case maps:get(tool_calls, Response, []) of
    [] ->
        %% 普通文本响应
        Content = maps:get(content, Response);
    ToolCalls ->
        %% 处理工具调用
        lists:foreach(fun(Call) ->
            Name = maps:get(name, Call),
            Args = maps:get(arguments, Call),
            %% 执行工具...
        end, ToolCalls)
end.
```

### 流式响应

```erlang
%% 流式回调函数
Callback = fun
    ({chunk, Delta}) ->
        io:format("~s", [Delta]);
    ({done, FullResponse}) ->
        io:format("~n完成~n")
end,

llm_client:chat_stream(Messages, Config, Callback).
```

## 环境变量

| 变量名 | 说明 |
|--------|------|
| `OPENAI_API_KEY` | OpenAI API 密钥 |
| `ANTHROPIC_API_KEY` | Anthropic API 密钥 |
| `ZHIPU_API_KEY` | 智谱 AI API 密钥 |
| `OLLAMA_BASE_URL` | Ollama 服务地址（默认 http://localhost:11434） |

## 依赖

- beamai_core
- jsx - JSON 编解码
- hackney - HTTP 客户端

## 许可证

Apache-2.0
