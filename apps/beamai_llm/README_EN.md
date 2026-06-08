# Agent LLM

English | [中文](README.md)

Large Language Model (LLM) client layer with support for multiple LLM providers.

## Supported Providers

| Provider | Module | API Mode | Description |
|----------|--------|----------|-------------|
| OpenAI | `beamai_llm_provider_openai` | OpenAI | GPT-4, GPT-3.5-turbo, etc. |
| Anthropic | `beamai_llm_provider_anthropic` | Anthropic | Claude 3, Claude 2, etc. |
| DeepSeek | `beamai_llm_provider_deepseek` | OpenAI Compatible | deepseek-chat, deepseek-reasoner |
| Ollama | `beamai_llm_provider_ollama` | OpenAI Compatible | Local model deployment |
| Zhipu AI | `beamai_llm_provider_zhipu` | OpenAI Compatible | GLM-4.7 and other Chinese models |
| Alibaba Cloud Bailian | `beamai_llm_provider_bailian` | DashScope Native | Qwen series (qwen-plus, qwen-max, etc.) |

## Module Overview

### Clients

- **llm_client** - LLM client main entry point
- **beamai_llm_http_client** - HTTP request handling
- **beamai_llm_helper** - Helper functions

### Providers

- **beamai_llm_provider_behaviour** - Provider behavior definition
- **beamai_llm_provider_openai** - OpenAI implementation
- **beamai_llm_provider_anthropic** - Anthropic implementation
- **beamai_llm_provider_deepseek** - DeepSeek implementation (OpenAI compatible API)
- **beamai_llm_provider_ollama** - Ollama implementation
- **beamai_llm_provider_zhipu** - Zhipu AI implementation
- **beamai_llm_provider_bailian** - Alibaba Cloud Bailian implementation (DashScope native API)

### Adapters

- **beamai_llm_message_adapter** - Message format adaptation (incl. multimodal: image/audio/PDF)
- **beamai_llm_tool_adapter** - Tool format adaptation
- **beamai_llm_response_parser** - Provider response parsing (OpenAI/Anthropic/DashScope formats → unified `beamai_llm_response` structure)

### Error Handling

- **beamai_llm_error** - Unified error structure. Normalizes the assorted Provider/HTTP errors (`{http_error, ...}` / `{api_error, ...}` / `{request_failed, ...}`, etc.) into a structured map with type / status / retryability / suggested backoff.

> **Note:** The core response data structure `beamai_llm_response` is in `beamai_core`, providing unified type definitions and accessors.

> **Streaming consistency:** All providers (incl. zhipu/bailian/ollama) return the unified `beamai_llm_response` structure for both sync and streaming calls, with streamed tool-call accumulation, usage stats, and reasoning/thinking content.

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
- Full support for tool calling (Function Calling), with automatic accumulation of streamed tool-call fragments
- Streaming output support
- OpenAI compatible API, response format consistent with OpenAI
- Chain-of-thought from deepseek-reasoner is exposed via `beamai_llm_response:reasoning_content/1` (both sync and streaming)
- Supports `frequency_penalty` / `presence_penalty` / `stop` / `logprobs` / `top_logprobs` / `response_format` config options
- deepseek-reasoner automatically strips unsupported params (temperature/top_p/penalty/logprobs) to avoid 400 errors
- Context disk-cache stats exposed in `usage.details` (`prompt_cache_hit_tokens` / `prompt_cache_miss_tokens`)

**DeepSeek-specific mechanisms (beta):**

```erlang
%% Chat Prefix Completion: mark the trailing assistant message with prefix => true
%% to force the model to continue from it (auto-routed to the /beta endpoint)
Messages = [
    #{role => user, content => <<"Write a Python quicksort">>},
    #{role => assistant, content => <<"```python\n">>, prefix => true}
],
{ok, Resp} = llm_client:chat(LLM, Messages),

%% FIM (Fill-In-the-Middle) completion: complete code between a prefix and suffix
{ok, Resp2} = beamai_llm_provider_deepseek:fim(Config, #{
    prompt => <<"def fib(n):">>,
    suffix => <<"    return fib(n-1) + fib(n-2)">>
}),
Completion = beamai_llm_response:content(Resp2),

%% Streaming FIM
{ok, Resp3} = beamai_llm_provider_deepseek:stream_fim(Config, #{prompt => P}, Callback).
```

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

> The callback receives each provider's **raw SSE events** (for live token display); the value returned by `stream_chat` is the unified `beamai_llm_response`, consistent with sync calls.

## Advanced Capabilities

### Multimodal Input (Image / Audio / PDF)

A message's `content` may be a **list of content parts** in addition to a binary string. `beamai_llm_message_adapter` converts them to each provider's format (OpenAI `image_url` / `input_audio`, Anthropic `image` / `document`). Plain binary text is fully backward-compatible.

```erlang
Messages = [
    #{role => user, content => [
        #{type => text, text => <<"What is in this image?">>},
        %% Image: base64
        #{type => image, source => #{type => base64,
            media_type => <<"image/png">>, data => Base64Png}},
        %% Image: url (also supports #{type => url, url => <<"https://...">>})
        #{type => image, source => #{type => url, url => <<"https://x/y.jpg">>}}
    ]}
],
{ok, Resp} = llm_client:chat(LLM, Messages).
```

- **Image**: OpenAI (gpt-4o, etc.) / Anthropic / DeepSeek
- **Audio** (`#{type => audio, data => Data, format => <<"wav">>}`): OpenAI `input_audio` (ignored for Anthropic)
- **PDF document** (`#{type => document, source => ...}`): Anthropic `document`; add `citations => true` to enable citations

### Anthropic Prompt Caching (cache_control)

Inject `cache_control: ephemeral` breakpoints on the system prompt / tool definitions / conversation history to cut the cost of repeated context.

```erlang
LLM = llm_client:create(anthropic, #{
    model => <<"claude-sonnet-4-5-20250929">>,
    api_key => ApiKey,
    %% strategy: none | system_only | tools_only | system_and_tools | conversation
    cache_control => system_and_tools
    %% or with TTL: #{strategy => system_only, ttl => <<"1h">>}
}).
```

Cache hit stats are in `usage.details` (`cache_creation_input_tokens` / `cache_read_input_tokens`).

### Anthropic Built-in Web Search

Enable the server-side built-in search tool; Claude searches the web and produces the answer within a single call.

```erlang
LLM = llm_client:create(anthropic, #{
    model => <<"claude-sonnet-4-5-20250929">>,
    api_key => ApiKey,
    %% true or #{max_uses => N, allowed_domains => [...], blocked_domains => [...]}
    web_search => #{max_uses => 3}
}),
{ok, Resp} = llm_client:chat(LLM, Messages),
%% Search results (title/url/page_age, etc.) land in metadata
Results = maps:get(web_search_results, beamai_llm_response:metadata(Resp), []).
```

### Anthropic Citations

With `citations => true` on a document part, citations in the response are collected into `metadata.citations`.

```erlang
Citations = maps:get(citations, beamai_llm_response:metadata(Resp), []).
```

### Rate-limit Response Headers

Both sync and streaming calls parse rate-limit info from response headers (`anthropic-ratelimit-*` / `x-ratelimit-*` / `retry-after`) into `metadata.rate_limit`.

```erlang
case maps:get(rate_limit, beamai_llm_response:metadata(Resp), undefined) of
    undefined -> ok;
    RL -> io:format("remaining requests: ~p~n", [maps:get(<<"requests-remaining">>, RL, undefined)])
end.
```

### Automatic Retry & Retry-After

`beamai_chat_completion:chat/3` has built-in exponential-backoff retry. On 429 / 5xx, if the server returns a `Retry-After` header it **backs off accordingly** (capped at 60s); otherwise it uses `retry_delay * attempt`.

```erlang
{ok, Resp} = beamai_chat_completion:chat(LLM, Messages, #{
    max_retries => 3,          %% default 3
    retry_delay => 1000,       %% base backoff ms, default 1000
    on_retry => fun(State) ->  %% optional retry callback
        io:format("retry #~p, delay ~pms: ~p~n",
                  [maps:get(attempt, State), maps:get(delay, State), maps:get(error, State)])
    end
}).
```

### Unified Error Structure (beamai_llm_error)

Normalizes assorted provider errors so callers can uniformly inspect type / retryability / suggested backoff. Providers still return their legacy error tuples; normalize on demand:

```erlang
case llm_client:chat(LLM, Messages) of
    {ok, Resp} -> ...;
    {error, Reason} ->
        Err = beamai_llm_error:from_reason(Reason, anthropic),
        case beamai_llm_error:type(Err) of
            rate_limit -> %% rate limited, retryable
                RetryMs = beamai_llm_error:retry_after_ms(Err);
            auth       -> %% auth failure (API key, etc.), not retryable
                io:format("~ts~n", [beamai_llm_error:message(Err)]);
            _          -> ...
        end
end.
```

Error `type`: `rate_limit | server_error | client_error | auth | timeout | network | invalid_response | api_error | unknown`. Accessors: `type/1`, `status/1`, `message/1`, `provider/1`, `retryable/1`, `retry_after_ms/1`, `raw/1`.

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
