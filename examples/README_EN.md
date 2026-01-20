# BeamAI Examples

English | [中文](README.md)

This directory contains example code for using the BeamAI Framework.

## Example List

### Agent Examples

#### example_agent_interactive.erl
**Interactive Deep Agent** - Intelligent assistant for continuous conversations
- **Features**: Multi-turn conversations, Planning, Reflection, Subtask derivation
- **Tools**: Search, Calculator, Time query, Notes
- **Commands**: `quit`/`exit`, `trace`, `plan`
- **Use Cases**: Interactive applications, Dialogue systems, AI assistants

Detailed documentation: [INTERACTIVE_DEEP_AGENT_GUIDE.md](INTERACTIVE_DEEP_AGENT_GUIDE.md)

#### example_agent_simple.erl
Simple Agent basic examples:
- **calculator_example/0**: Calculator Agent (math tools)
- **weather_example/0**: Weather query Agent (simulated data)

#### example_agent_deep.erl
Deep Agent advanced feature examples:
- **research_example/0**: Research Agent (Planning + Reflection)
- **code_analyzer_example/0**: Code analysis Agent (subtask derivation)

#### example_agent_graph.erl
Deep Agent example based on Graph engine

### LLM Examples

#### example_llm_chat.erl
Chat example using Zhipu AI (GLM-4), demonstrating different conversation methods:
- **simple_chat/0**: Simplest single-turn conversation
- **chat_with_messages/0**: Using custom message list
- **chat_with_system_prompt/0**: Conversation with system prompt
- **multi_turn/0**: Multi-turn conversation example

#### example_llm_anthropic.erl
Example using Zhipu GLM-4.7 through Anthropic API compatible interface

#### example_llm_bailian.erl
Alibaba Cloud Bailian (DashScope native API) examples:
- **simple_chat/0**: Simplest single-turn conversation
- **chat_with_messages/0**: Using custom message list
- **chat_with_system_prompt/0**: Conversation with system prompt
- **multi_turn/0**: Multi-turn conversation example

#### example_bailian_native_test.erl
Bailian DashScope native API complete tests:
- **test_simple_chat/0**: Simple conversation test
- **test_chat_with_tools/0**: Tool calling test
- **test_stream_chat/0**: Streaming output test

#### example_output_parser.erl
Output Parser structured output examples:
- **json_parse_example/0**: JSON Schema parsing
- **retry_example/0**: Automatic retry mechanism

### Graph Computing Examples

#### example_pregel.erl
Pregel distributed computing algorithm examples

#### example_checkpoint.erl
Checkpoint state persistence examples

#### example_graph_parallel.erl
Parallel graph computing examples

### Integration Examples

#### example_a2a_server.erl / example_a2a_handler.erl
A2A (Agent-to-Agent) protocol server examples

#### example_mcp_tools.erl / example_mcp_proxy.erl
MCP (Model Context Protocol) tool integration examples

## Usage

### Quick Start

```bash
# 1. Set environment variables (choose based on the provider you use)
export ZHIPU_API_KEY=your_key_here      # Zhipu AI
export BAILIAN_API_KEY=your_key_here    # Alibaba Cloud Bailian (DashScope)

# 2. Compile the project
rebar3 compile

# 3. Start Shell
rebar3 shell

# 4. Run examples (in shell)
%% Interactive Deep Agent
example_agent_interactive:run().

%% Simple Agent
example_agent_simple:calculator_example().

%% Deep Agent
example_agent_deep:research_example().

%% Zhipu chat
example_llm_chat:simple_chat().

%% Bailian (Alibaba Cloud DashScope)
example_llm_bailian:simple_chat().

%% MCP tool integration
example_mcp_tools:run().
```

### Compile and Run a Single Example

```bash
# Compile example
erlc -I apps/beamai_llm/include -I apps/beamai_core/include \
     examples/beamai_examples/src/example_llm_chat.erl

# Run
erl -pa apps/beamai_llm/ebin -pa apps/beamai_core/ebin \
    -eval "example_llm_chat:simple_chat()" \
    -s init stop \
    -noshell
```

## Code Examples

### Simple Agent

```erlang
%% Create Agent with tools
%% Define calculator tool
CalcTool = #{
    name => <<"calculator">>,
    description => <<"Perform mathematical calculations">>,
    parameters => #{
        type => object,
        properties => #{
            <<"expression">> => #{
                type => string,
                description => <<"Mathematical expression, e.g. 2+3*4">>
            }
        },
        required => [<<"expression">>]
    },
    handler => fun(#{<<"expression">> := Expr}) ->
        {ok, calculate(Expr)}
    end
},

%% Use Registry to build tool list
Tools = beamai_tool_registry:from_config(#{tools => [CalcTool]}),

{ok, Agent} = beamai_agent:start_link(<<"my_agent">>, #{
    system_prompt => <<"You are a helpful assistant.">>,
    tools => Tools,
    llm => #{
        provider => anthropic,
        model => <<"glm-4.7">>,
        api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
        base_url => <<"https://open.bigmodel.cn/api/anthropic">>
    }
}).

%% Run
{ok, Result} = beamai_agent:run(Agent, <<"What is 2+3*4?">>).
```

### Multi Mode Coordinator

```erlang
%% Create research team
{ok, Team} = beamai_agent:start_multi(<<"research_team">>, #{
    agents => [
        #{
            name => <<"researcher">>,
            role => <<"Researcher">>,
            system_prompt => <<"You are a researcher, responsible for gathering materials.">>
        },
        #{
            name => <<"writer">>,
            role => <<"Writer">>,
            system_prompt => <<"You are a writer, responsible for writing articles.">>
        }
    ],
    llm => LLMConfig
}).

%% Run
{ok, Result} = beamai_agent:run(Team,
    <<"Research and write an introduction about Erlang.">>).
```

### Deep Agent

```erlang
%% Create Deep Agent with planning
{ok, Agent} = beamai_deepagent:start_link(<<"deep_agent">>, #{
    max_depth => 3,
    planning_enabled => true,
    reflection_enabled => true,
    tools => [...],
    llm => LLMConfig
}).

%% Run complex task
{ok, Result} = beamai_deepagent:run(Agent,
    <<"Analyze the codebase architecture and provide optimization suggestions.">>).

%% View execution plan
{ok, Plan} = beamai_deepagent:get_plan(Agent).
```

## Environment Requirements

- **Erlang/OTP**: 26+
- **Dependencies**: beamai_core, beamai_llm
- **Environment Variables**: `ZHIPU_API_KEY`, `BAILIAN_API_KEY` (or other providers you use)

## Related Documentation

- **[README.md](../README.md)** - Project homepage
- **[DEPENDENCIES.md](../doc/DEPENDENCIES.md)** - Dependencies
- **[doc/ARCHITECTURE.md](../doc/ARCHITECTURE.md)** - Architecture design

## Tips

1. **Choose the right example**:
   - Beginners: Start with `example_agent_simple`
   - Intermediate: Try `example_agent_deep` and `example_output_parser`
   - Practical: Run `example_agent_interactive`

2. **API Key configuration**:
   ```bash
   export ZHIPU_API_KEY=your_key        # Zhipu AI
   export BAILIAN_API_KEY=your_key      # Alibaba Cloud Bailian (DashScope)
   ```

3. **Debugging tips**:
   - Use `sys:trace(Agent, true)` to enable tracing
   - Use the callback system to listen for execution events
   - Check Scratchpad to understand execution steps

---

**Start exploring the features of BeamAI Framework!**
