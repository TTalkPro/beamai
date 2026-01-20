# Agent LLM

English | [中文](README.md)

Large Language Model (LLM) client layer with support for multiple LLM providers.

## Supported Providers

| Provider | Module | API Mode | Description |
|----------|--------|----------|-------------|
| OpenAI | `llm_provider_openai` | OpenAI | GPT-4, GPT-3.5-turbo, etc. |
| Anthropic | `llm_provider_anthropic` | Anthropic | Claude 3, Claude 2, etc. |
| DeepSeek | `llm_provider_deepseek` | OpenAI Compatible | deepseek-chat, deepseek-reasoner |
| Ollama | `llm_provider_ollama` | OpenAI Compatible | Local model deployment |
| Zhipu AI | `llm_provider_zhipu` | OpenAI Compatible | GLM-4.7 and other Chinese models |
| Alibaba Cloud Bailian | `llm_provider_bailian` | DashScope Native | Qwen series (qwen-plus, qwen-max, etc.) |

## Module Overview

### Clients

- **llm_client** - LLM client main entry point
- **llm_http_client** - HTTP request handling
- **llm_helper** - Helper functions

### Providers

- **llm_provider_behaviour** - Provider behavior definition
- **llm_provider_openai** - OpenAI implementation
- **llm_provider_anthropic** - Anthropic implementation
- **llm_provider_deepseek** - DeepSeek implementation (OpenAI compatible API)
- **llm_provider_ollama** - Ollama implementation
- **llm_provider_zhipu** - Zhipu AI implementation
- **llm_provider_bailian** - Alibaba Cloud Bailian implementation (DashScope native API)

### Adapters

- **llm_message_adapter** - Message format adaptation
- **llm_tool_adapter** - Tool format adaptation
- **llm_response_adapter** - Response format adaptation

## API Documentation

### llm_client

```erlang
%% Send chat request
llm_client:chat(Config, Messages) -> {ok, Response} | {error, Reason}.

%% Send chat request with tools
llm_client:with_tools(Config, Messages, Tools) -> {ok, Response} | {error, Reason}.

%% Simple chat (single-turn conversation)
llm_client:simple_chat(Config, Prompt) -> {ok, Content} | {error, Reason}.

%% Streaming chat
llm_client:stream_chat(Config, Messages, Callback) -> {ok, Response} | {error, Reason}.
```

### Creating Configuration

LLM configuration must be created using `llm_client:create/2`:

```erlang
%% Create LLM configuration
LLM = llm_client:create(Provider, #{
    model => <<"gpt-4">>,                 %% Model name (required)
    api_key => <<"sk-xxx">>,              %% API key (required, except for ollama)
    base_url => <<"https://...">>,        %% Optional, custom API endpoint
    temperature => 0.7,                   %% Optional, temperature parameter
    max_tokens => 4096                    %% Optional, maximum token count
}).

%% Provider types: openai | anthropic | deepseek | ollama | zhipu | bailian
```

## Usage Examples

### Basic Chat

```erlang
%% Create OpenAI configuration
LLM = llm_client:create(openai, #{
    model => <<"gpt-4">>,
    api_key => list_to_binary(os:getenv("OPENAI_API_KEY"))
}),

%% Send message
Messages = [
    #{role => system, content => <<"You are a helpful assistant.">>},
    #{role => user, content => <<"Hello!">>}
],

{ok, Response} = llm_client:chat(LLM, Messages),
Content = maps:get(content, Response).
```

### Using DeepSeek

DeepSeek API is compatible with OpenAI API, supporting `deepseek-chat` and `deepseek-reasoner` models.

```erlang
%% Create DeepSeek configuration
LLM = llm_client:create(deepseek, #{
    model => <<"deepseek-chat">>,
    api_key => list_to_binary(os:getenv("DEEPSEEK_API_KEY"))
}),

%% Send message
Messages = [
    #{role => user, content => <<"你好！"/utf8>>}
],

{ok, Response} = llm_client:chat(LLM, Messages).
```

**Supported Models:**

| Model | Description | Recommended Use Cases |
|-------|-------------|----------------------|
| `deepseek-chat` | General conversation model (default) | Daily conversations, code generation |
| `deepseek-reasoner` | Reasoning-enhanced model | Complex reasoning, math problems |

**Features:**
- Full support for tool calling (Function Calling)
- Streaming output support
- OpenAI compatible API, response format consistent with OpenAI

### Using Alibaba Cloud Bailian (DashScope Native API)

Alibaba Cloud Bailian Provider uses DashScope native API, supporting:
- Text generation: `/api/v1/services/aigc/text-generation/generation`
- Multimodal generation: `/api/v1/services/aigc/multimodal-generation/generation` (automatically selected based on model)

```erlang
%% Create Bailian configuration (Qwen)
LLM = llm_client:create(bailian, #{
    model => <<"qwen-plus">>,  %% Recommended: balanced cost-performance
    api_key => list_to_binary(os:getenv("BAILIAN_API_KEY"))
}),

%% Note: Chinese strings require /utf8 suffix
Messages = [
    #{role => user, content => <<"你好！"/utf8>>}
],

{ok, Response} = llm_client:chat(LLM, Messages).
```

**Supported Models:**

| Model | Description | Recommended Use Cases |
|-------|-------------|----------------------|
| `qwen-max` | Flagship model, best performance | Complex reasoning, professional tasks |
| `qwen-plus` | Balanced model (recommended) | General scenarios |
| `qwen-turbo` | Fast model, lowest cost | Simple tasks, high concurrency |
| `qwen-vl-plus` | Vision-language model | Image understanding (automatically uses multimodal endpoint) |

**Unique Features:**

```erlang
%% Enable web search
LLM = llm_client:create(bailian, #{
    model => <<"qwen-plus">>,
    api_key => ApiKey,
    enable_search => true  %% Enable web search
}).
```

### Using Zhipu AI

Zhipu AI supports two calling methods:

**Method 1: Native API (Recommended)**

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

**Method 2: Anthropic Compatible Interface**

```erlang
%% Zhipu AI provides Anthropic compatible interface
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

### Tool Calling

```erlang
%% Define tools
Tools = [
    #{
        type => function,
        function => #{
            name => <<"get_weather">>,
            description => <<"Query city weather"/utf8>>,
            parameters => #{
                type => object,
                properties => #{
                    <<"city">> => #{
                        type => string,
                        description => <<"City name"/utf8>>
                    }
                },
                required => [<<"city">>]
            }
        }
    }
],

%% Send request with tools
{ok, Response} = llm_client:with_tools(LLM, Messages, Tools),

%% Check for tool calls
case maps:get(tool_calls, Response, []) of
    [] ->
        %% Normal text response
        Content = maps:get(content, Response);
    ToolCalls ->
        %% Handle tool calls
        lists:foreach(fun(#{name := Name, arguments := Args}) ->
            io:format("Tool: ~s, Args: ~s~n", [Name, Args])
        end, ToolCalls)
end.
```

### Streaming Response

```erlang
%% Streaming callback function
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

## Environment Variables

| Variable Name | Description |
|---------------|-------------|
| `OPENAI_API_KEY` | OpenAI API key |
| `ANTHROPIC_API_KEY` | Anthropic API key |
| `DEEPSEEK_API_KEY` | DeepSeek API key |
| `ZHIPU_API_KEY` | Zhipu AI API key |
| `BAILIAN_API_KEY` | Alibaba Cloud Bailian API key (DashScope) |
| `OLLAMA_BASE_URL` | Ollama service address (default http://localhost:11434) |

## Provider Technical Details

### DeepSeek (OpenAI Compatible API)

DeepSeek API is fully compatible with OpenAI API format, using the same request/response structure.

**API Endpoint:**
- Default address: `https://api.deepseek.com`
- Chat interface: `/chat/completions`

**Request Format:**
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

**Response Format:**
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

### Alibaba Cloud Bailian (DashScope Native API)

**Request Format:**
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

**Response Format:**
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

**Streaming Output:**
- Request header: `X-DashScope-SSE: enable`
- Parameter: `parameters.incremental_output: true`

## Dependencies

- beamai_core
- jsx - JSON encoding/decoding
- hackney - HTTP client

## License

Apache-2.0
