# API Reference

English | [中文](API_REFERENCE.md)

This document covers the public API of the BeamAI core library: `beamai_core`, `beamai_llm`, and `beamai_agent`. Features migrated to [beamai_extra](https://github.com/TTalkPro/beamai_extra) (Process Framework orchestration, the storage and snapshot engine, the Context Reducer, Tools Library, RAG, A2A/MCP protocols, and the Deep Agent) are out of scope here.

The reference describes every public module, its exports, and its key types. Signatures stay in Erlang; descriptions are in English. Worked examples appear once per major module, not once per function.

## Table of Contents

- [beamai_core - Core Module](#beamai_core---core-module)
- [beamai_llm - LLM Client](#beamai_llm---llm-client)
- [beamai_agent - SimpleAgent (ReAct)](#beamai_agent---simpleagent-react)
- [Behaviour Interfaces](#behaviour-interfaces)
- [Error Handling](#error-handling)
- [More Documentation](#more-documentation)

---

## beamai_core - Core Module

### Top-level DSL: beamai

`beamai` is the facade module for all external calls. It wraps `beamai_kernel`, `beamai_tool`, `beamai_filter`, `beamai_context`, and the prompt renderer behind a small set of constructors and convenience calls.

```erlang
-spec kernel() -> beamai_kernel:kernel().
-spec kernel(beamai_kernel:kernel_settings()) -> beamai_kernel:kernel().
-spec kernel(beamai_kernel:kernel_settings(), [beamai_filter:filter()]) ->
    beamai_kernel:kernel().

-spec tool(binary(), beamai_tool:handler()) -> beamai_tool:tool_spec().
-spec tool(binary(), beamai_tool:handler(), map()) -> beamai_tool:tool_spec().

-spec add_tool(beamai_kernel:kernel(), beamai_tool:tool_spec()) ->
    beamai_kernel:kernel().
-spec add_tools(beamai_kernel:kernel(), [beamai_tool:tool_spec()]) ->
    beamai_kernel:kernel().
-spec add_tool_module(beamai_kernel:kernel(), module()) ->
    beamai_kernel:kernel().

-spec add_llm(beamai_kernel:kernel(), atom(), map()) ->
    beamai_kernel:kernel().
-spec add_llm(beamai_kernel:kernel(), beamai_chat_completion:config()) ->
    beamai_kernel:kernel().

-spec filter(binary(), beamai_filter:hooks()) -> beamai_filter:filter().
-spec filter(binary(), beamai_filter:hooks(), beamai_filter:fctx()) ->
    beamai_filter:filter().

-spec invoke_tool(beamai_kernel:kernel(), binary(),
                  beamai_tool:args(), beamai_context:t()) ->
    {ok, term(), beamai_context:t()} | {error, term()}.

-spec chat(beamai_kernel:kernel(), [map()]) ->
    {ok, map(), beamai_context:t()} | {error, term()}.
-spec chat(beamai_kernel:kernel(), [map()], beamai_kernel:chat_opts()) ->
    {ok, map(), beamai_context:t()} | {error, term()}.

-spec render(binary(), map()) -> {ok, binary()} | {error, term()}.

-spec tools(beamai_kernel:kernel()) -> [map()].
-spec tools(beamai_kernel:kernel(), openai | anthropic | atom()) -> [map()].

-spec tools_by_tag(beamai_kernel:kernel(), binary()) ->
    [beamai_tool:tool_spec()].

-spec context() -> beamai_context:t().
-spec context(map()) -> beamai_context:t().
```

| Function | Purpose |
|----------|---------|
| `kernel/0,1,2` | Build a kernel with optional settings and filter list |
| `tool/2,3` | Construct a tool spec from name + handler, with optional options |
| `add_tool/2`, `add_tools/2` | Register one tool or a batch |
| `add_tool_module/2` | Load tools from a module implementing `beamai_tool_behaviour` |
| `add_llm/2,3` | Register an LLM service (provider atom + opts, or prebuilt config) |
| `filter/2,3` | Build a filter from a hooks map, with optional private-context seed |
| `invoke_tool/4` | Call a registered tool through the tool filter chain |
| `chat/2,3` | Send a chat completion through the chat filter chain |
| `render/2` | Render a `{{var}}` prompt template against a context or map |
| `tools/1,2` | List tool schemas (default OpenAI format, or per-provider) |
| `tools_by_tag/2` | Filter registered tools by tag |
| `context/0,1` | Build an empty execution context, or one seeded with env vars |

**Example: Kernel + Tool + Filter in one block**

```erlang
AddTool = beamai:tool(<<"add">>,
    fun(#{a := A, b := B}) -> {ok, A + B} end,
    #{description => <<"Add two numbers">>,
      parameters => #{
          a => #{type => integer, required => true},
          b => #{type => integer, required => true}
      }}).

Logger = beamai:filter(<<"logger">>, #{
    around_tool => fun(#{tool := #{name := N}, args := Args} = Req, _FCtx, Next) ->
        io:format("tool ~ts(~p)~n", [N, Args]),
        Next(Req)
    end
}).

K0 = beamai:kernel(#{}, [Logger]),
K1 = beamai:add_tool(K0, AddTool),
K2 = beamai:add_llm(K1, beamai_chat_completion:create(zhipu, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY"))
})),

{ok, 16, _Ctx} = beamai:invoke_tool(K2, <<"add">>, #{a => 7, b => 9},
                                    beamai:context()).
```

---

### Kernel: beamai_kernel

`beamai_kernel` is the core container: it owns the tool registry, the LLM service config, and the filter list. Filters are passed **once** at construction time. The order in the list decides the onion layering (earlier equals more outer).

```erlang
-spec new() -> kernel().
-spec new(kernel_settings()) -> kernel().
-spec new(kernel_settings(), [beamai_filter:filter()]) -> kernel().

-spec add_tool(kernel(), beamai_tool:tool_spec()) -> kernel().
-spec add_tools(kernel(), [beamai_tool:tool_spec()]) -> kernel().
-spec add_tool_module(kernel(), module()) -> kernel().
-spec add_service(kernel(), beamai_chat_completion:config()) -> kernel().

-spec invoke_tool(kernel(), binary(),
                   beamai_tool:args(), beamai_context:t()) ->
    {ok, term(), beamai_context:writes()} | {error, term()}.

-spec invoke_chat(kernel(), [map()], chat_opts()) ->
    {ok, map(), beamai_context:t()} | {error, term()}.

-spec invoke_chat_stream(kernel(), [map()], chat_opts(),
                         fun((binary(), map()) -> ok)) ->
    {ok, map(), beamai_context:t()} | {error, term()}.

-spec get_tool(kernel(), binary()) ->
    {ok, beamai_tool:tool_spec()} | error.
-spec list_tools(kernel()) -> [beamai_tool:tool_spec()].
-spec get_tools_by_tag(kernel(), binary()) -> [beamai_tool:tool_spec()].
-spec get_tool_specs(kernel()) -> [map()].
-spec get_tool_schemas(kernel()) -> [map()].
-spec get_tool_schemas(kernel(), openai | anthropic | atom()) -> [map()].

-spec get_service(kernel()) ->
    {ok, beamai_chat_completion:config()} | error.

-spec state_slots(kernel()) -> beamai_context:state_slots().
-spec serial_tool(kernel(), binary()) -> boolean().
-spec return_direct_tool(kernel(), binary()) -> boolean().
```

#### Types

```erlang
-type kernel() :: #{
    '__kernel__' := true,
    tools := #{binary() => beamai_tool:tool_spec()},
    llm_config := beamai_chat_completion:config() | undefined,
    filters := [beamai_filter:filter()],
    settings := kernel_settings()
}.

-type kernel_settings() :: #{
    default_timeout => pos_integer(),
    atom() => term()
}.

-type chat_opts() :: #{
    tools => [map()],
    tool_choice => auto | none | required,
    context => beamai_context:t(),
    system_prompts => [map()],
    atom() => term()
}.
```

| Function | Purpose |
|----------|---------|
| `new/0,1,2` | Construct a kernel (default settings / custom settings / settings + filters) |
| `add_tool/2`, `add_tools/2` | Register one or many tools |
| `add_tool_module/2` | Register all tools exposed by a tool module |
| `add_service/2` | Attach an LLM service config |
| `invoke_tool/4` | Run one tool through the tool filter chain |
| `invoke_chat/3` | Run one LLM call through the chat filter chain (no tool loop) |
| `invoke_chat_stream/4` | Streaming variant; same chain plus the `token_transform` pipeline |
| `get_tool/2`, `list_tools/1`, `get_tools_by_tag/2` | Look up tools |
| `get_tool_specs/1`, `get_tool_schemas/1,2` | Export tool definitions for an LLM call |
| `get_service/1` | Inspect the LLM service config |
| `state_slots/1` | Kernel-level declarations of tool-call state slots |
| `serial_tool/2`, `return_direct_tool/2` | Query tool flags set during registration |

---

### Context: beamai_context

`beamai_context` is the shared execution context threaded through the kernel. It has three partitions:

- **`env`**: the environment and user-supplied variables (kernel reference, conversation id, vars).
- **`state`**: the structured state partition that tools write to via `writes()`.
- **`'__filter_states__'`**: per-filter private state, isolated by filter name.

```erlang
-spec new() -> t().
-spec new(map()) -> t().

-spec get(t(), atom() | binary()) -> term() | undefined.
-spec get(t(), atom() | binary(), term()) -> term().
-spec variables(t()) -> #{binary() => term()}.
-spec set(t(), atom() | binary(), term()) -> t().
-spec set_many(t(), map()) -> t().

-spec with_conversation_id(t(), binary()) -> t().
-spec conversation_id(t()) -> binary() | undefined.

-spec with_kernel(t(), term()) -> t().
-spec get_kernel(t()) -> term() | undefined.

-spec state_get(t(), atom() | binary()) -> term() | undefined.
-spec state_get(t(), atom() | binary(), term()) -> term().
-spec get_state(t()) -> #{binary() => term()}.
-spec with_state(t(), #{binary() => term()}) -> t().

-spec apply_writes(t(), [{pos_integer(), writes()}], state_slots()) ->
    {t(), [binary()]}.

-spec filter_state(t(), binary(), map()) -> map().
-spec set_filter_state(t(), binary(), map()) -> t().

-spec normalize_key(atom() | binary()) -> binary().
-spec keys(t()) -> [binary()].
-spec delete(t(), atom() | binary()) -> t().
-spec has_key(t(), atom() | binary()) -> boolean().
-spec update(t(), atom() | binary(), fun((term()) -> term())) -> t().
```

#### Types

```erlang
-type t() :: #{
    '__context__' := true,
    env := env(),
    state := #{binary() => term()},
    '__filter_states__' := #{binary() => map()}
}.

-type env() :: #{
    kernel := term() | undefined,
    conversation_id := binary() | undefined,
    vars := #{binary() => term()}
}.

-type writes() :: #{binary() | atom() => term()}.

-type state_slots() :: #{
    binary() => #{init => term(),
                  reduce => fun((term(), term()) -> term())}
}.

-type message() :: beamai_message:message().
-type tool_call() :: beamai_message:tool_call().
```

| Function | Purpose |
|----------|---------|
| `new/0,1` | Empty context, or one with seed env vars |
| `get/2,3`, `variables/1` | Read env vars (with optional default) |
| `set/3`, `set_many/2`, `update/3` | Write env vars |
| `with_conversation_id/2`, `conversation_id/1` | Bind / read the conversation identifier |
| `with_kernel/2`, `get_kernel/1` | Bind / read the kernel reference |
| `state_get/2,3`, `get_state/1`, `with_state/2` | Read or replace the state partition |
| `apply_writes/3` | Fold a batch of tool writes into the state partition in `tool_call` order, using slot reduce funs |
| `filter_state/3`, `set_filter_state/3` | Read / write a single filter's private state |
| `normalize_key/1`, `keys/1`, `delete/2`, `has_key/2` | Generic map helpers |

---

### Tool: beamai_tool

`beamai_tool` defines a tool's shape: name, handler, parameters, timeouts, retry policy, and flags such as `serial`, `sensitive`, `return_direct`.

```erlang
-spec new(binary(), handler()) -> tool_spec().
-spec new(binary(), handler(), map()) -> tool_spec().

-spec validate(tool_spec()) -> ok | {error, [term()]}.

-spec invoke(tool_spec(), args()) -> tool_result().
-spec invoke(tool_spec(), args(), beamai_context:t()) -> tool_result().

-spec to_tool_spec(tool_spec()) -> map().
-spec to_tool_schema(tool_spec()) -> map().
-spec to_tool_schema(tool_spec(), openai | anthropic | atom()) -> map().

-spec get_name(tool_spec()) -> binary().
-spec get_tag(tool_spec()) -> binary() | [binary()] | undefined.
-spec has_tag(tool_spec(), binary()) -> boolean().
-spec is_serial(tool_spec()) -> boolean().
-spec is_sensitive(tool_spec()) -> boolean().
-spec is_return_direct(tool_spec()) -> boolean().

-spec parse_tool_call(map()) -> {binary(), binary(), map()}.
-spec encode_result(term()) -> binary().

-spec from_module(module()) -> {ok, [tool_spec()]} | {error, term()}.
```

#### Types

```erlang
-type tool_spec() :: #{
    name := binary(),
    handler := handler(),
    description => binary(),
    parameters => parameters_schema(),
    tag => binary() | [binary()],
    timeout => pos_integer(),
    retry => boolean()
             | #{max_retries => non_neg_integer(),
                 initial_delay_ms => non_neg_integer()}
             | #{max => integer(), delay => integer()},
    serial => boolean(),
    sensitive => boolean(),
    return_direct => boolean(),
    filters => [filter_ref()],
    metadata => map()
}.

-type handler() ::
    fun((args()) -> tool_result())
    | fun((args(), beamai_context:t()) -> tool_result())
    | {module(), atom()}
    | {module(), atom(), [term()]}.

-type tool_result() ::
    {ok, term()}
    | {ok, term(), beamai_context:writes()}
    | {error, term()}.

-type args() :: map().

-type parameters_schema() :: #{atom() | binary() => param_spec()}.

-type param_spec() :: #{
    type := string | integer | float | boolean | array | object,
    description => binary(),
    required => boolean(),
    default => term(),
    enum => [term()],
    items => param_spec(),
    properties => parameters_schema()
}.
```

| Function | Purpose |
|----------|---------|
| `new/2,3` | Build a tool spec (minimum form, or with options) |
| `validate/1` | Check spec validity; returns the list of problems |
| `invoke/2,3` | Run the handler with an empty context, or a supplied one |
| `to_tool_spec/1` | Canonical internal tool spec map |
| `to_tool_schema/1,2` | Provider-shaped schema (OpenAI default, or named provider) |
| `get_name/1`, `get_tag/1`, `has_tag/2` | Introspection |
| `is_serial/1`, `is_sensitive/1`, `is_return_direct/1` | Flag queries |
| `parse_tool_call/1` | Unpack an LLM `tool_call` map into `{Id, Name, Args}` |
| `encode_result/1` | Render a tool result for inclusion in an LLM message |
| `from_module/1` | Load all tools exposed by a module implementing `beamai_tool_behaviour` |

---

### Filter System

The Filter system is the onion-style interception mechanism that wraps LLM calls, tool executions, and full turns. It is documented in detail in [FILTER_EN.md](FILTER_EN.md). This section lists the public API.

#### beamai_filter

A `filter` bundles up to four hooks plus a private-context seed.

```erlang
-spec new(binary(), hooks()) -> filter().
-spec new(binary(), hooks(), fctx()) -> filter().
-spec hook(filter(), hook_type()) ->
    around_fun() | token_transform() | undefined.
-spec init(filter()) -> fctx().
```

#### Types

```erlang
-type hook_type() :: around_chat | around_tool | around_turn | token_transform.

-type hooks() :: #{
    around_chat     => around_fun(),
    around_tool     => around_fun(),
    around_turn     => around_fun(),
    token_transform => token_transform()
}.

-type around_fun() ::
    fun((request(), fctx(), next()) ->
        response() | {response(), fctx()}).

-type token_transform() :: #{
    init  => term(),
    step  := fun((token_data(), State :: term()) ->
                 {[token_data()], NewState :: term()}),
    flush => fun((State :: term()) -> [token_data()])
}.

-type token_data() :: #{token := binary(), meta := map()}.

-type filter() :: #{
    '__filter__' := true,
    name  := binary(),
    hooks := hooks(),
    init  := fctx()
}.
```

The four hooks:

| Hook | Chain | Form |
|------|-------|------|
| `around_chat` | Chat (one LLM call) | `fun(Req, FCtx, Next) -> Resp \| {Resp, NewFCtx}` |
| `around_tool` | Tool (one tool execution) | same |
| `around_turn` | Turn (whole tool loop, Agent layer) | same |
| `token_transform` | Streaming (per-token intervention) | `#{init, step, flush}` |

`token_transform` is the streaming-only fourth hook: `step` is a 1-to-N function over one incoming `token_data()`; `flush` runs at end of normal stream and emits any buffered residue. Error paths do not flush. See [FILTER_EN.md](FILTER_EN.md#the-4th-hook-token_transform-token-stream-transform) for the full contract.

| Function | Purpose |
|----------|---------|
| `new/2,3` | Build a filter (hooks only, or hooks + initial private context) |
| `hook/2` | Read a single hook by type |
| `init/1` | Read the private-context seed |

#### beamai_filter_chain

Composes and runs the onion chain for one phase.

```erlang
-spec run([beamai_filter:filter()], phase(), terminal(), request()) ->
    {ok, response()} | {error, term()}.

-spec compose([beamai_filter:filter()], phase(), terminal()) ->
    fun((request()) -> response()).
```

| Function | Purpose |
|----------|---------|
| `run/4` | Run the chain once with a terminal and a request |
| `compose/3` | Compose the chain into a single closure for embedding |

#### Built-in Filter Library: beamai_filters

`beamai_filters` is the library of ready-made filter constructors. Put them in `new/2`'s filters list at kernel build time.

```erlang
-spec logging_filter() -> beamai_filter:filter().
-spec timeout_filter(pos_integer()) -> beamai_filter:filter().
-spec approval_filter(fun((binary(), map()) -> boolean())) ->
    beamai_filter:filter().
-spec safeguard_filter([binary()]) -> beamai_filter:filter().
-spec safeguard_filter([binary()], map()) -> beamai_filter:filter().
-spec validation_turn_filter(fun((map()) -> ok | {invalid, term()}),
                             non_neg_integer()) ->
    beamai_filter:filter().
-spec schema_validation_turn_filter(beamai_json_schema:schema(),
                                    non_neg_integer()) ->
    beamai_filter:filter().
-spec schema_validation_turn_filter(beamai_json_schema:schema(),
                                    non_neg_integer(), map()) ->
    beamai_filter:filter().
-spec token_redact_filter(iodata(), binary()) -> beamai_filter:filter().
-spec hold_release_filter(fun((binary()) -> ok | {block, binary()})) ->
    beamai_filter:filter().
```

| Constructor | Chain | Purpose |
|-------------|-------|---------|
| `logging_filter/0` | chat / tool / turn | One pair of debug logs per chain; put first to see all layers |
| `timeout_filter/1` | tool | Wall-clock timeout (ms) for a single tool execution |
| `approval_filter/1` | tool | Rejects sensitive tools when the predicate returns `false` (non-interactive) |
| `safeguard_filter/1,2` | chat | Substring pre-filter on a sensitive word list; short-circuits the LLM call |
| `validation_turn_filter/2` | turn | Re-enter the loop when the final answer fails a predicate; up to N retries |
| `schema_validation_turn_filter/2,3` | turn | JSON Schema specialization of the above |
| `token_redact_filter/2` | token_transform | Stateless per-token regex replacement |
| `hold_release_filter/1` | token_transform | Buffer the full stream, gate release on end-of-stream predicate |

**Example: structured-output self-correction**

```erlang
Schema = #{
    type => object,
    properties => #{
        <<"name">> => #{type => string},
        <<"age">>  => #{type => integer, minimum => 0}
    },
    required => [<<"name">>, <<"age">>]
},

K = beamai:kernel(#{}, [
    beamai_filters:schema_validation_turn_filter(Schema, 2)
]).
%% The turn chain re-enters the tool loop when JSON validation fails,
%% feeding the schema errors back to the model.
```

---

### Memory Filter: beamai_memory_filter

The Memory Filter is a single `around_chat` filter that loads and stores per-conversation history. Place it **first** in the kernel's filter list so it runs outermost (history expansion before any other rewrite, persistence after).

```erlang
-spec memory_filter(beamai_chat_memory:handle()) -> beamai_filter:filter().
```

It binds to a `beamai_chat_memory` backend (ETS, DETS, or a custom behaviour implementation) and manages history entirely through `conversation_id`. Without this filter the kernel is stateless and single-shot; with it, multi-turn conversation works across invokes. See [MEMORY_EN.md](MEMORY_EN.md) for the full design.

---

### Conversation Storage Backends

Both backends implement the `beamai_chat_memory` behaviour. The handle convention is `{Module, Ref}`.

#### beamai_chat_memory_ets

Default in-memory implementation. The owning gen_server holds the ETS table.

```erlang
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
-spec stop(atom()) -> ok.
-spec handle(atom()) -> beamai_chat_memory:handle().

-spec mem_get(atom(), binary()) -> [beamai_message:message()].
-spec mem_add(atom(), binary(), [beamai_message:message()]) -> ok.
-spec mem_clear(atom(), binary()) -> ok.
```

#### beamai_chat_memory_dets

Persistent implementation backed by a DETS file. Each write is followed by `dets:sync`, so a process or node restart with the same file recovers every conversation's history.

```erlang
-spec start_link(atom(),
                 #{file := file:name_all(), _ => _}) ->
    {ok, pid()} | {error, term()}.

-spec stop(atom()) -> ok.
-spec handle(atom()) -> beamai_chat_memory:handle().

-spec mem_get(atom(), binary()) -> [beamai_message:message()].
-spec mem_add(atom(), binary(), [beamai_message:message()]) -> ok.
-spec mem_clear(atom(), binary()) -> ok.
```

For a count-based sliding window that keeps the full history underneath and trims only before sending, see `beamai_memory_provider_default:new/2` described in [MEMORY_EN.md](MEMORY_EN.md#sliding-window).

---

### Token Stream Assembler: beamai_token_stream

`beamai_token_stream:wrap/2` composes a list of `token_transform` filters with a sink callback into a single `{TokenCallback, Flush}` pair. `invoke_chat_stream` stores this pair in its process dictionary and drives it: each token flows through every step in registration order, then is handed to the sink; on normal end, `flush` cascades inside-out.

```erlang
-spec wrap([beamai_filter:token_transform()], sink()) ->
    {fun((binary(), map()) -> ok), fun(() -> ok)}.
```

The returned `TokenCallback` accepts `{Token, Meta}` and is what the streaming LLM driver invokes per chunk. `Flush` is called once on normal end of stream.

---

### Tool Index and Search

#### beamai_tool_index (Behaviour)

Indexes a tool list for query-time retrieval. The default backend is keyword; vector backends can plug in here.

```erlang
-spec new(module(), [beamai_tool:tool_spec()], map()) -> handle().
-spec search(handle(), binary(), pos_integer()) -> [binary()].
-spec fingerprint([beamai_tool:tool_spec()]) -> binary().
```

```erlang
-opaque handle() :: {module(), State :: term()}.

-callback build(Tools :: [beamai_tool:tool_spec()],
                Opts  :: map()) -> State :: term().
-callback search(Query      :: binary(),
                 MaxResults :: pos_integer(),
                 State      :: term()) -> [binary()].
```

| Function | Purpose |
|----------|---------|
| `new/3` | Build an index from a backend module, tool list, and options |
| `search/3` | Return up to `MaxResults` matching tool names, best first |
| `fingerprint/1` | SHA-256 fingerprint of a tool list, for cache invalidation |

#### beamai_tool_search

`beamai_tool_search:new/1,2` builds the on-demand reveal pattern: it returns a single `tool_search` tool plus a filter that hides all unindexed tools from the advertised list on round 1 and reveals the searched ones on later rounds.

```erlang
-spec new([beamai_tool:tool_spec()]) ->
    {beamai_tool:tool_spec(), beamai_filter:filter()}.

-spec new([beamai_tool:tool_spec()], opts()) ->
    {beamai_tool:tool_spec(), beamai_filter:filter()}.

-spec default_system_suffix() -> binary().
```

```erlang
{STool, SFilter} = beamai_tool_search:new(Tools, #{
    index_module => beamai_tool_index_keyword,
    max_results  => 5,
    accumulate   => true,
    tool_name    => <<"tool_search">>
}),
K0 = beamai_kernel:new(#{}, [SFilter]),
K1 = beamai_kernel:add_tools(K0, [STool | Tools]).
```

---

### JSON Schema: beamai_json_schema

Zero-dependency validator for a practical subset of DRAFT 2020-12. Atom and binary keys are both accepted. `$ref`, `$defs`, `patternProperties`, `if-then-else`, and `format` are not supported.

```erlang
-spec validate(schema(), term()) -> ok | {error, [error()]}.
-spec validate(schema(), term(), opts()) -> ok | {error, [error()]}.
-spec error_message([error()]) -> binary().
```

```erlang
-type schema() :: map() | boolean().
-type error()  :: #{path := binary(),
                    keyword := atom(),
                    message := binary()}.
-type opts()   :: #{max_errors => pos_integer()}.
```

| Function | Purpose |
|----------|---------|
| `validate/2,3` | Validate an instance; collects up to `max_errors` (default all) |
| `error_message/1` | Render an error list as a single human-readable string |

---

### Prompt Template: beamai_prompt

`{{variable}}` substitution.

```erlang
-spec new(binary()) -> prompt_template().
-spec render(prompt_template(), map() | beamai_context:t()) ->
    {ok, binary()} | {error, term()}.
-spec get_variables(prompt_template()) -> [binary()].
```

```erlang
-type prompt_template() :: #{
    template := binary(),
    input_variables := [binary()]
}.
```

| Function | Purpose |
|----------|---------|
| `new/1` | Compile a template from a binary with `{{var}}` placeholders |
| `render/2` | Render against a map or against a context's env vars |
| `get_variables/1` | List the variable names referenced by the template |

---

### Result Monad: beamai_result

A small `Result<T>` monad for `{ok, _} | {error, _}` flows.

```erlang
-spec ok(T) -> {ok, T}.
-spec error(E) -> {error, E}.

-spec is_ok(result(term())) -> boolean().
-spec is_error(result(term())) -> boolean().

-spec map(result(A), fun((A) -> B)) -> result(B).
-spec flat_map(result(A), fun((A) -> result(B))) -> result(B).
-spec bind(result(A), fun((A) -> result(B))) -> result(B).

-spec pipe(A, [fun((A) -> result(B)) | fun((A) -> B)]) -> result(B).
-spec pipe_while(A, [{fun((A) -> boolean()),
                       fun((A) -> result(A))}]) -> result(A).

-spec unwrap(result(T)) -> T.
-spec unwrap_or(result(T), T) -> T.
-spec unwrap_error(result(term(), E)) -> E.

-spec from_boolean(boolean(), E) -> result(ok, E).
-spec from_maybe(T | undefined, E) -> result(T, E).

-spec collect([result(T)]) -> result([T]).
-spec partition([result(T, E)]) -> {[T], [E]}.

-spec tap(result(T), fun((T) -> term())) -> result(T).
-spec tap_error(result(T, E), fun((E) -> term())) -> result(T, E).
```

```erlang
-type result(T)    :: {ok, T} | {error, term()}.
-type result(T, E) :: {ok, T} | {error, E}.
```

---

### Tool Error Classification: beamai_tool_error

Classifies a tool's error reason into one of three buckets. Useful for deciding whether to retry, short-circuit, or bubble up.

```erlang
-spec classify(term()) -> class().
-spec message(term()) -> binary().
```

```erlang
-type class() :: semantic | transient | environment.
```

---

### LLM Response: beamai_llm_response

The unified response shape across all providers. Construct from a raw map with `new/1`, then read fields via the accessor functions.

```erlang
-spec new(map()) -> response().

-spec id(response()) -> binary().
-spec model(response()) -> binary().
-spec provider(response()) -> provider().
-spec content(response()) -> binary() | null.
-spec content_blocks(response()) -> [content_block()].
-spec thinking(response()) -> binary() | null.
-spec reasoning_content(response()) -> binary() | null.
-spec tool_calls(response()) -> [tool_call()].
-spec has_tool_calls(response()) -> boolean().
-spec finish_reason(response()) -> finish_reason().
-spec is_complete(response()) -> boolean().
-spec needs_tool_call(response()) -> boolean().

-spec usage(response()) -> usage().
-spec input_tokens(response()) -> non_neg_integer().
-spec output_tokens(response()) -> non_neg_integer().
-spec total_tokens(response()) -> non_neg_integer().

-spec raw(response()) -> map().
-spec raw_get(response(), [binary()] | binary()) -> term() | undefined.
-spec raw_get(response(), [binary()] | binary(), term()) -> term().
-spec metadata(response()) -> map().
-spec set_metadata(response(), term(), term()) -> response().
-spec to_map(response()) -> map().
```

#### Types

```erlang
-type response() :: #{
    '__struct__' := ?MODULE,
    id := binary(),
    model := binary(),
    provider := provider(),
    content := binary() | null,
    content_blocks := [content_block()],
    tool_calls := [tool_call()],
    finish_reason := finish_reason(),
    usage := usage(),
    raw := map(),
    metadata := map()
}.

-type provider() :: openai | anthropic | deepseek | zhipu
                  | ollama | dashscope | unknown.

-type content_block() ::
    #{type := text,         text := binary()}
    | #{type := thinking,   thinking := binary(), signature := binary()}
    | #{type := redacted_thinking, data := binary()}
    | #{type := tool_use,   id := binary(), name := binary(), input := map()}.

-type tool_call() :: #{
    id := binary(),
    name := binary(),
    arguments := map(),
    raw_arguments := binary()
}.

-type finish_reason() ::
    complete | tool_use | length_limit | content_filtered
    | stop_sequence | pause_turn | refusal | error | unknown.

-type usage() :: #{
    input_tokens := non_neg_integer(),
    output_tokens := non_neg_integer(),
    total_tokens := non_neg_integer(),
    details => map()
}.
```

---

### Message Construction: beamai_message

Constructors and accessors for the unified message shape (`role`, `content`, `tool_calls`, `content_blocks`, `tool_call_id`, `name`).

```erlang
-spec system(term()) -> message().
-spec user(term()) -> message().
-spec assistant(term()) -> message().
-spec tool_calls([tool_call()]) -> message().
-spec tool_result(binary(), binary(), term()) -> message().
-spec with_content_blocks(message(),
                          [beamai_llm_response:content_block()]) -> message().
-spec from_response(term()) -> message() | undefined.

-spec role(message()) -> atom().
-spec content(message()) -> binary() | null.
-spec get_tool_calls(message()) -> [tool_call()].
-spec tool_call_id(message()) -> binary() | undefined.
-spec name(message()) -> binary() | undefined.
-spec content_blocks(message()) ->
    [beamai_llm_response:content_block()].

-spec is_message(term()) -> boolean().
-spec is_role(message(), atom()) -> boolean().
-spec messages([{atom(), term()}]) -> [message()].
```

#### Types

```erlang
-type message() :: #{
    role := user | assistant | system | tool,
    content := binary() | null,
    tool_calls => [tool_call()],
    content_blocks => [beamai_llm_response:content_block()],
    tool_call_id => binary(),
    name => binary()
}.

-type tool_call() :: #{
    id := binary(),
    type := function,
    function := #{
        name := binary(),
        arguments := binary() | map()
    }
}.
```

---

### HTTP Client: beamai_http

Wraps the `beamai_http_behaviour` backend. `beamai_http_gun` is the only built-in implementation and the default; the backend is selected via the application environment and can be swapped at runtime (mainly so tests can substitute a fake).

```erlang
-spec get(url()) -> response().
-spec get(url(), params()) -> response().
-spec get(url(), params(), options()) -> response().

-spec post(url(), binary(), body()) -> response().
-spec post(url(), binary(), body(), options()) -> response().
-spec post_json(url(), map() | list()) -> response().
-spec post_json(url(), map() | list(), options()) -> response().

-spec put(url(), binary(), body()) -> response().
-spec put(url(), binary(), body(), options()) -> response().

-spec delete(url()) -> response().
-spec delete(url(), params()) -> response().
-spec delete(url(), params(), options()) -> response().

-spec request(atom(), url(), headers(), body(), options()) -> response().
-spec request_meta(atom(), url(), headers(), body(), options()) ->
    {ok, body(), beamai_http_behaviour:meta()} | {error, term()}.

-spec stream_request(atom(), url(), headers(), body(), options()) ->
    stream_response().
-spec stream_request(atom(), url(), headers(), body(), options(),
                     chunk_handler()) -> stream_response().

-spec url_encode(term()) -> binary().
-spec build_url(url(), params()) -> binary().

-spec ensure_started() -> ok.
-spec get_backend() -> module().
-spec set_backend(module()) -> ok.
```

Gun backend characteristics:

- **HTTP/2** — opt-in per pool via `protocols`, see [HTTP_EN.md](HTTP_EN.md)
- **Connection pool** — built-in purpose-shaped pools (`beamai_http_pool` instances)
- **TLS** — system CA certificates (OTP 25+)

The Gun backend runs three purpose-shaped pools (`http_pool_short` for short
requests, `http_pool_stream` for SSE streaming, `http_pool_longpoll` for async
polling), configured per pool via `http_pools` — set only the pools and keys
you want to override. See [HTTP_EN.md](HTTP_EN.md):

```erlang
%% Configure the backend via sys.config (optional)
{beamai_core, [
    {http_backend, beamai_http_gun},
    {http_pools, #{
        http_pool_stream   => #{max_connections_per_host => 20,
                                idle_timeout => 120000},
        http_pool_longpoll => #{idle_timeout => 300000}
    }}
]}.
```

The legacy `http_pool` key still works (applied uniformly to all three pools,
with a deprecation warning at startup; the legacy key name `connection_timeout`
is normalized to `connect_timeout`).

---

### Utility Modules

#### beamai_utils

Generic helpers shared by the Agent layer.

```erlang
-spec timestamp() -> timestamp().
-spec timestamp_seconds() -> pos_integer().
-spec safe_get(map(), term(), term()) -> term().
-spec safe_merge(map(), map()) -> map().
-spec paginate(list(), non_neg_integer(), pos_integer()) -> list().
-spec filter_by_time([map()], timestamp(), timestamp()) -> [map()].
-spec validate_binary(term()) -> boolean().
-spec validate_map(term()) -> boolean().
-spec binary_join(binary(), [binary()]) -> binary().
-spec to_binary(term()) -> binary().
-spec ensure_binary(term()) -> binary().
-spec encode_body(binary() | map() | list() | term()) -> binary().
-spec decode_json_response(binary() | term()) -> map() | binary() | term().
-spec parse_json(binary() | map()) -> map().
-spec safe_execute(fun(() -> T)) -> {ok, T} | {error, {atom(), term()}}.
-spec safe_execute(fun((A) -> T), A) -> {ok, T} | {error, {atom(), term()}}.
-spec format_error(term()) -> binary().
-spec format_error(binary(), term()) -> binary().
```

#### beamai_sse

Server-Sent Events parser/encoder used by every streaming provider.

```erlang
-spec parse(binary()) -> {binary(), [map()]}.
-spec encode_event(binary(), binary() | term()) -> binary().
-spec encode_event(binary(), binary() | term(), binary()) -> binary().
```

#### beamai_jsonrpc

JSON-RPC 2.0 message encode/decode.

```erlang
-spec encode_request(term(), binary(), map()) -> binary().
-spec encode_notification(binary(), map()) -> binary().
-spec encode_response(term(), term()) -> binary().
-spec encode_error(term(), integer(), binary()) -> binary().
-spec encode_error(term(), integer(), binary(), term()) -> binary().

-spec decode(binary()) -> {ok, map() | {batch, [map()]}} | {error, term()}.
-spec decode_request(binary()) ->
    {ok, {term(), binary(), map()}} | {error, term()}.

-spec is_request(map()) -> boolean().
-spec is_notification(map()) -> boolean().
-spec is_response(map()) -> boolean().
-spec is_error(map()) -> boolean().
-spec is_batch(term()) -> boolean().

-spec parse_error(term()) -> map().
-spec invalid_request(term()) -> map().
-spec method_not_found(term(), binary()) -> map().
-spec invalid_params(term(), binary()) -> map().
-spec internal_error(term()) -> map().
-spec custom_error(term(), integer(), binary(), term()) -> map().
```

#### beamai_id

Generates prefixed, time-ordered identifiers.

```erlang
-spec gen_id(prefix()) -> id().
-spec parse_id(id()) -> {ok, parsed_id()} | {error, term()}.
```

```erlang
-type id() :: binary().
-type prefix() :: binary().
-type parsed_id() :: #{
    prefix := binary(),
    timestamp := non_neg_integer(),
    random := binary()
}.
```

---

## beamai_llm - LLM Client

The `beamai_llm` app is the unified LLM client. It exposes one synchronous entry point (`beamai_chat_completion:chat/2,3`), one streaming entry point (`stream_chat/3,4`), a structured error type, and adapters for converting between the unified internal shapes and each provider's wire format.

### Chat Completion: beamai_chat_completion

The most-used public API. Construct a `config()` with `create/2`, then call `chat/2,3` or `stream_chat/3,4`.

```erlang
-spec create(provider(), map()) -> config().
-spec chat(config(), [map()]) -> {ok, map()} | {error, term()}.
-spec chat(config(), [map()], map()) -> {ok, map()} | {error, term()}.
-spec stream_chat(config(), [map()], fun((term()) -> ok)) ->
    {ok, map()} | {error, term()}.
-spec stream_chat(config(), [map()], fun((term()) -> ok), map()) ->
    {ok, map()} | {error, term()}.
```

#### Types

```erlang
-type provider() :: openai | anthropic | ollama | zhipu
                  | dashscope | deepseek | mock
                  | {custom, module()}.

-type config() :: #{
    provider := provider(),
    module := module(),
    '__llm_config__' := true,
    atom() => term()
}.
```

**Example: create + chat**

```erlang
LLM = beamai_chat_completion:create(zhipu, #{
    model => <<"glm-4.7">>,
    api_key => list_to_binary(os:getenv("ZHIPU_API_KEY")),
    temperature => 0.7
}),

{ok, Resp} = beamai_chat_completion:chat(LLM, [
    #{role => user, content => <<"Hello">>}
]),

io:format("~ts~n", [beamai_llm_response:content(Resp)]).
```

---

### Error Structure: beamai_llm_error

A unified error type returned across all providers. Use `from_reason/1,2` to wrap a raw reason (typically a thrown term or HTTP response body), then read the structured fields.

```erlang
-spec from_reason(term()) -> llm_error().
-spec from_reason(term(), atom() | undefined) -> llm_error().
-spec is_error(term()) -> boolean().

-spec type(llm_error()) -> error_type().
-spec status(llm_error()) -> non_neg_integer() | undefined.
-spec message(llm_error()) -> binary().
-spec provider(llm_error()) -> atom() | undefined.
-spec retryable(llm_error()) -> boolean().
-spec retry_after_ms(llm_error()) -> non_neg_integer() | undefined.
-spec raw(llm_error()) -> term().
```

#### Types

```erlang
-type error_type() :: rate_limit | server_error | client_error | auth
                    | timeout | network | invalid_response
                    | api_error | unknown.

-type llm_error() :: #{
    '__llm_error__' := true,
    type := error_type(),
    status := non_neg_integer() | undefined,
    message := binary(),
    provider := atom() | undefined,
    retryable := boolean(),
    retry_after_ms := non_neg_integer() | undefined,
    raw := term()
}.
```

`retryable/1` and `retry_after_ms/1` are particularly useful for backoff loops: respect the suggested wait when present, otherwise fall back to your own policy for `rate_limit` and `server_error`.

---

### HTTP Client: beamai_llm_http_client

The shared HTTP client every provider uses. Most callers never touch this module directly; it appears here for completeness and for building custom provider modules.

```erlang
-spec request(binary(), [{binary(), binary()}], map(), request_opts()) ->
    {ok, map()} | {error, term()}.

-spec request(binary(), [{binary(), binary()}], map(), request_opts(),
              response_parser()) -> {ok, map()} | {error, term()}.

-spec stream_request(binary(), [{binary(), binary()}], map(),
                     request_opts(), stream_callback()) ->
    {ok, map()} | {error, term()}.

-spec stream_request(binary(), [{binary(), binary()}], map(),
                     request_opts(), stream_callback(),
                     event_accumulator()) -> {ok, map()} | {error, term()}.

-spec parse_sse(binary()) -> {binary(), [map() | done | skip]}.
-spec parse_sse_lines([binary()], [map() | done | skip]) ->
    {binary(), [map() | done | skip]}.

-spec init_stream_acc() -> map().
-spec finalize_stream(map()) -> {ok, map()}.
```

---

### LLM Helper: beamai_llm_helper

Small façade used by upper layers (such as expert-style multi-step flows) to invoke an LLM and return a synthesized answer.

```erlang
-spec call_expert(expert_config(), binary(), llm_config()) ->
    binary() | {error, term()}.
-spec synthesize(binary(), binary(), binary(), llm_config()) ->
    binary() | {error, term()}.
```

---

### Output Parser

`beamai_output_parser` is the unified entry. The parser modules underneath cover JSON, XML, CSV, and the format instructions used to teach the model what shape to emit.

#### beamai_output_parser

```erlang
-spec new(format(), map()) -> parser().
-spec json() -> parser().
-spec json(map()) -> parser().
-spec xml() -> parser().
-spec csv() -> parser().

-spec parse(parser(), binary()) -> parse_result().
-spec parse(parser(), binary(), map()) -> parse_result().
-spec parse_with_retry(parser(), binary(), non_neg_integer()) ->
    parse_result().
-spec parse_with_retry(parser(), binary(), non_neg_integer(), map()) ->
    parse_result().

-spec get_instructions(format()) -> binary().
-spec get_instructions(format(), map()) -> binary().

-spec is_retryable_error(parse_error()) -> boolean().
```

#### Types

```erlang
-type format() :: json | xml | csv | raw.
-type parser() :: #{type := format(), options := map()}.
-type parse_result() :: {ok, term()} | {error, parse_error()}.

-type parse_error() ::
    {invalid_json, binary()}
    | {invalid_xml, binary()}
    | {invalid_csv, binary()}
    | {extract_failed, binary()}
    | {max_retries_exceeded, [parse_error()]}.
```

**Example: JSON parser with schema and retry**

```erlang
Parser = beamai_output_parser:json(#{
    schema => #{
        type => object,
        properties => #{
            <<"title">> => #{type => string},
            <<"count">> => #{type => integer}
        },
        required => [<<"title">>, <<"count">>]
    }
}),

Instructions = beamai_output_parser:get_instructions(json, #{
    schema => Parser
}),

{ok, Parsed} = beamai_output_parser:parse_with_retry(Parser, LLMText, 3).
```

#### beamai_parser_json

The JSON-specific extractor and repairer.

```erlang
-spec parse(binary(), parse_options()) -> parse_result().
-spec extract_json(binary()) -> {ok, binary()} | {error, term()}.
-spec extract_json_codeblock(binary()) -> {ok, binary()} | {error, term()}.
-spec repair_json(binary()) -> binary().
-spec find_json_boundaries(binary()) -> {ok, binary()} | {error, term()}.
```

#### beamai_parser_instructions

Renders the format-prompt text that gets spliced into a system message.

```erlang
-spec json() -> binary().
-spec json(map()) -> binary().
-spec xml() -> binary().
-spec csv() -> binary().
-spec json_schema_to_instruction(map()) -> binary().
```

#### beamai_parser_retry

Retry primitives shared by the parsers.

```erlang
-spec parse(beamai_output_parser:parser(), binary(),
            non_neg_integer(), map()) -> parse_result().

-spec parse_with_backoff(beamai_output_parser:parser(), binary(),
                         non_neg_integer(), atom(), retry_options()) ->
    parse_result().
```

---

### Provider Adapters

The adapters convert between the unified internal shapes and each provider's wire format. They are not meant to be called by user code directly; they sit underneath `beamai_chat_completion`.

#### beamai_llm_response_parser

Returns parser functions for each provider, plus `from_*` constructors that build a unified `beamai_llm_response:response()` from a raw provider payload.

```erlang
-spec parser_openai() ->
    fun((map()) ->
        {ok, beamai_llm_response:response()} | {error, term()}).
-spec parser_anthropic() ->
    fun((map()) ->
        {ok, beamai_llm_response:response()} | {error, term()}).
-spec parser(beamai_llm_response:provider()) ->
    fun((map()) ->
        {ok, beamai_llm_response:response()} | {error, term()}).
-spec parser_ollama() ->
    fun((map()) ->
        {ok, beamai_llm_response:response()} | {error, term()}).
-spec parser_dashscope() ->
    fun((map()) ->
        {ok, beamai_llm_response:response()} | {error, term()}).
-spec parser_zhipu() ->
    fun((map()) ->
        {ok, beamai_llm_response:response()} | {error, term()}).
-spec parser_deepseek() ->
    fun((map()) ->
        {ok, beamai_llm_response:response()} | {error, term()}).
-spec parser_deepseek_fim() ->
    fun((map()) ->
        {ok, beamai_llm_response:response()} | {error, term()}).

-spec from_provider(map(), beamai_llm_response:provider()) ->
    {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_openai(map()) ->
    {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_anthropic(map()) ->
    {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_zhipu(map()) ->
    {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_deepseek(map()) ->
    {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_deepseek_fim(map()) ->
    {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_ollama(map()) ->
    {ok, beamai_llm_response:response()} | {error, term()}.
-spec from_dashscope(map()) ->
    {ok, beamai_llm_response:response()} | {error, term()}.
```

#### beamai_llm_message_adapter

Converts a list of unified `message()` maps to and from a provider-specific wire shape.

```erlang
-spec to_provider([message()], atom()) -> [map()].
-spec from_provider([map()], atom()) -> [message()].
-spec extract_system_prompt([message()]) ->
    {term() | undefined, [message()]}.

-spec to_openai([message()]) -> [map()].
-spec from_openai([map()]) -> [message()].
-spec to_anthropic([message()]) -> [map()].
-spec from_anthropic([map()]) -> [message()].
```

```erlang
-type role_atom() :: user | assistant | system | tool.
-type role()      :: role_atom() | binary().
-type message()   :: #{
    role := role(),
    content := content(),
    name => binary(),
    tool_call_id => binary(),
    tool_calls => [map()],
    prefix => boolean()
}.
-type content()      :: binary() | null | [content_part()].
-type content_part() :: #{type := atom(), _ => _}.
-type media_source() ::
    #{type := base64, media_type := binary(), data := binary()}
    | #{type := url, url := binary()}.
```

#### beamai_llm_tool_adapter

Same shape as the message adapter, but for tool definitions.

```erlang
-spec to_provider([tool() | tool_spec()], atom()) -> [map()].
-spec from_provider([map()], atom()) -> [tool()].
-spec to_openai([tool() | tool_spec()]) -> [map()].
-spec from_openai([map()]) -> [tool()].
-spec to_anthropic([tool() | tool_spec()]) -> [map()].
-spec from_anthropic([map()]) -> [tool()].
```

```erlang
-type tool() :: #{
    name := binary(),
    description := binary(),
    parameters := map()
}.

-type tool_spec() :: #{
    type := function,
    function := #{
        name := binary(),
        description := binary(),
        parameters := map()
    }
}.
```

#### beamai_llm_provider_common

Helpers used by every provider module: URL building, default timeouts, bearer-auth headers, stream-event accumulators for both OpenAI and Anthropic formats, tool-call parsing, usage parsing, and rate-limit header parsing.

```erlang
%% URL building
-spec build_url(map(), binary(), binary()) -> binary().

%% Timeouts
-spec default_timeout(atom()) -> pos_integer().
-spec request_timeout(map(), atom()) -> timeout().

%% Request headers
-spec build_bearer_auth_headers(map()) -> [{binary(), binary()}].

%% Request body helpers
-spec maybe_add_stream(map(), map()) -> map().
-spec maybe_add_tools(map(), map()) -> map().
-spec maybe_add_top_p(map(), map()) -> map().
-spec maybe_add_tool_choice(map(), map()) -> map().
-spec maybe_add_params(map(), map(), [{atom(), binary()}]) -> map().

%% Stream event accumulators
-spec accumulate_openai_event(map(), map()) -> map().
-spec finalize_openai_stream(map(), atom()) ->
    {ok, map()} | {error, term()}.
-spec accumulate_anthropic_event(map(), map()) -> map().
-spec finalize_anthropic_stream(map()) -> {ok, map()}.
-spec accumulate_completions_event(map(), map()) -> map().
-spec finalize_completions_stream(map(),
        fun((map()) -> {ok, map()} | {error, term()})) ->
    {ok, map()} | {error, term()}.

%% Response parsing
-spec parse_tool_calls(map()) -> [map()].
-spec parse_single_tool_call(map()) -> map().
-spec parse_usage(map()) -> map().

%% Headers
-spec rate_limit_metadata([{binary(), binary()}]) -> map().
-spec retry_after_ms([{binary(), binary()}]) ->
    non_neg_integer() | undefined.
```

---

### Supported Providers

Pass the provider atom to `beamai_chat_completion:create/2`. Each adapter module implements `beamai_chat_behaviour` and ships with the same sync and streaming surface.

| Provider | Module | API Mode | Multimodal | Streaming | Notes |
|----------|--------|----------|------------|-----------|-------|
| `anthropic` | `beamai_llm_provider_anthropic` | Anthropic | yes | yes | Caching, Web Search, citations |
| `openai` | `beamai_llm_provider_openai` | OpenAI | yes | yes | Reference OpenAI client |
| `deepseek` | `beamai_llm_provider_deepseek` | OpenAI-compatible | no | yes | DeepSeek chat + FIM variants |
| `zhipu` | `beamai_llm_provider_zhipu` | OpenAI-compatible | yes | yes | Zhipu AI GLM series (4.6+ exposes `reasoning_content`) |
| `dashscope` | `beamai_llm_provider_dashscope` | DashScope native | yes | yes | Alibaba Cloud Qwen |
| `ollama` | `beamai_llm_provider_ollama` | OpenAI-compatible | varies | yes | Local models |

A `mock` provider also exists (`beamai_llm_provider_mock`) for tests. It implements `beamai_chat_behaviour` and returns canned responses, useful for unit tests of agent and tool-loop code without hitting a real API.

---

## beamai_agent - SimpleAgent (ReAct)

The `beamai_agent` app provides a stateful multi-turn Agent built on top of `beamai_kernel`. It implements a ReAct-style loop: each user turn calls the LLM, executes any returned tool calls, and feeds the results back until the model emits a final answer. State lives outside the kernel: conversation history is owned by a Memory Provider, pause snapshots by a Pause Store, branch lineage by a Branch Store. The Agent handle is an opaque `agent_state()` value returned by `new/1`.

### Agent: beamai_agent

The primary public API. Every Agent interaction goes through `run/2,3`, `stream/2,3`, or `resume/2,3`.

```erlang
-spec new(map()) ->
    {ok, beamai_agent_state:agent_state()} | {error, term()}.

-spec run(beamai_agent_state:agent_state(), binary()) ->
    {ok, run_result(), beamai_agent_state:agent_state()}
    | {interrupt, interrupt_info(), beamai_agent_state:agent_state()}
    | {error, term()}.

-spec run(beamai_agent_state:agent_state(), binary(), map()) ->
    {ok, run_result(), beamai_agent_state:agent_state()}
    | {interrupt, interrupt_info(), beamai_agent_state:agent_state()}
    | {error, term()}.

-spec stream(beamai_agent_state:agent_state(), binary()) ->
    {ok, run_result(), beamai_agent_state:agent_state()}
    | {interrupt, interrupt_info(), beamai_agent_state:agent_state()}
    | {error, term()}.

-spec stream(beamai_agent_state:agent_state(), binary(), map()) ->
    {ok, run_result(), beamai_agent_state:agent_state()}
    | {interrupt, interrupt_info(), beamai_agent_state:agent_state()}
    | {error, term()}.

-spec resume(beamai_agent_state:agent_state(), term()) ->
    {ok, run_result(), beamai_agent_state:agent_state()}
    | {interrupt, interrupt_info(), beamai_agent_state:agent_state()}
    | {error, term()}.

-spec resume(beamai_agent_state:agent_state(), term(), map()) ->
    {ok, run_result(), beamai_agent_state:agent_state()}
    | {interrupt, interrupt_info(), beamai_agent_state:agent_state()}
    | {error, term()}.

-spec is_interrupted(beamai_agent_state:agent_state()) -> boolean().
-spec get_interrupt_info(beamai_agent_state:agent_state()) ->
    interrupt_info() | undefined.

-spec messages(beamai_agent_state:agent_state()) -> [map()].
-spec last_response(beamai_agent_state:agent_state()) ->
    binary() | undefined.
-spec turn_count(beamai_agent_state:agent_state()) -> non_neg_integer().

-spec kernel(beamai_agent_state:agent_state()) ->
    beamai_kernel:kernel().
-spec id(beamai_agent_state:agent_state()) -> binary().
-spec name(beamai_agent_state:agent_state()) -> binary().

-spec set_system_prompt(beamai_agent_state:agent_state(), binary()) ->
    beamai_agent_state:agent_state().
-spec add_message(beamai_agent_state:agent_state(), map()) ->
    beamai_agent_state:agent_state().
-spec clear_messages(beamai_agent_state:agent_state()) ->
    beamai_agent_state:agent_state().
-spec update_metadata(beamai_agent_state:agent_state(), map()) ->
    beamai_agent_state:agent_state().
```

#### Types

```erlang
-type run_result() :: #{
    content := binary(),
    tool_calls_made => [map()],
    finish_reason := beamai_llm_response:finish_reason(),
    usage := map(),
    iterations := non_neg_integer()
}.

-type interrupt_info() :: #{
    reason := term(),
    interrupt_type := tool_request | tool_result
                     | callback | env_retry,
    interrupted_tool_call => map(),
    completed_results => [map()],
    created_at := integer()
}.
```

**Example 1: basic run loop**

```erlang
{ok, A0} = beamai_agent:new(#{
    llm => beamai_chat_completion:create(zhipu, #{
        model => <<"glm-4.7">>,
        api_key => list_to_binary(os:getenv("ZHIPU_API_KEY"))
    }),
    tools => [beamai:tool(<<"add">>,
        fun(#{a := A, b := B}) -> {ok, A + B} end,
        #{description => <<"Add two numbers">>,
          parameters => #{
              a => #{type => integer, required => true},
              b => #{type => integer, required => true}
          }})]
}),

case beamai_agent:run(A0, <<"What is 7 plus 9?">>) of
    {ok, #{content := Answer}, A1} ->
        io:format("agent says: ~ts~n", [Answer]),
        %% A1 is the new state; pass it to the next call
        {ok, _, _A2} = beamai_agent:run(A1, <<"Now double that">>);
    {error, Reason} ->
        io:format("error: ~p~n", [Reason])
end.
```

**Example 2: interrupt + resume**

```erlang
{ok, A0} = beamai_agent:new(#{...}),

case beamai_agent:run(A0, <<"Refactor the auth module">>) of
    {interrupt, #{reason := <<"approval needed">>, tool_call := TC}, A1} ->
        %% Ask the user, then resume with the decision payload
        Decision = ask_user(TC),
        case beamai_agent:resume(A1, #{tool_call_id => maps:get(id, TC),
                                       decision => Decision}) of
            {ok, #{content := Answer}, A2} -> {ok, Answer, A2};
            {error, Reason}                -> {error, Reason}
        end;
    {ok, #{content := Answer}, _A1} ->
        {ok, Answer, A0}
end.
```

When `pause_store` is configured on the agent, a second invocation (after process or node restart) can pick the same conversation back up by calling `beamai_agent_pause:load/1` and rehydrating the state. See [Pause Persistence](#pause-persistence-beamai_agent_pause).

---

### State: beamai_agent_state

Constructs and inspects the opaque `agent_state()` handle returned by `beamai_agent:new/1`.

```erlang
-spec create(map()) ->
    {ok, agent_state()} | {error, term()}.
-spec build_kernel(map()) -> map().
-spec memory(agent_state()) ->
    beamai_memory_provider:provider() | undefined.
-spec conversation_id(agent_state()) -> binary().
```

```erlang
-type agent_state() :: #{
    '__agent__' := true,
    id := binary(),
    name := binary(),
    kernel := beamai_kernel:kernel(),
    memory := beamai_memory_provider:provider() | undefined,
    conversation_id := binary(),
    system_prompt := binary() | undefined,
    max_tool_iterations := pos_integer(),
    parallel_tools := boolean(),
    callbacks := beamai_agent_callbacks:callbacks(),
    turn_count := non_neg_integer(),
    metadata := map(),
    created_at := integer(),
    interrupt_state := undefined | interrupt_state(),
    run_id := binary() | undefined,
    interrupt_tools := [map()],
    on_env_error := proceed | pause,
    pause_store := beamai_pause_store:handle() | undefined
}.

-type interrupt_state() :: #{
    status := interrupted,
    reason := term(),
    messages := [map()],
    completed_tool_results := [map()],
    interrupted_tool_call := map() | undefined,
    iteration := non_neg_integer(),
    tool_calls_made := [map()],
    saved_state := map(),
    interrupt_type := tool_request | tool_result
                     | callback | env_retry,
    created_at := integer(),
    phase => approval | env_retry,
    batch_messages => [map()],
    failed_calls => [map()]
}.
```

---

### Tool Loop: beamai_agent_tool_loop

The internal ReAct loop. Exported for advanced customisation (custom loop runners, instrumented test harnesses).

```erlang
-spec run(loop_opts(), [map()]) ->
    {ok, map(), [map()], pos_integer(), [map()]}
    | {interrupt, atom(), map()}
    | {error, term()}.

-spec build_env_interrupt_context(
    non_neg_integer(), [map()], [map()], [map()], [map()],
    map(), [map()]) -> map().
```

```erlang
-type loop_opts() :: #{
    kernel := beamai_kernel:kernel(),
    messages := [map()],
    new_messages := [map()],
    load_history := boolean(),
    chat_opts := map(),
    callbacks := map(),
    max_iterations := pos_integer(),
    max_tool_iterations := pos_integer(),
    parallel_tools := boolean(),
    interrupt_tools := [map()],
    on_env_error => proceed | pause,
    memory := beamai_memory_provider:provider() | undefined,
    conversation_id := binary(),
    meta := map(),
    stream_token_handler => undefined | fun((binary()) -> ok)
}.
```

---

### Callbacks: beamai_agent_callbacks

The Agent fires ten optional callbacks at well-defined points. Pass any subset in the agent config under the `callbacks` key.

```erlang
-spec invoke(atom(), [term()], callbacks()) -> ok.
-spec build_metadata(map()) -> map().
```

```erlang
-type callbacks() :: #{
    on_turn_start  => fun((map()) -> ok),
    on_turn_end    => fun((map()) -> ok),
    on_turn_error  => fun((term(), map()) -> ok),
    on_llm_call    => fun(([map()], map()) -> ok),
    on_llm_result  => fun((map(), map()) -> ok),
    on_tool_call   => fun((binary(), map()) -> ok | {interrupt, term()}),
    on_tool_result => fun((binary(), binary()) -> ok),
    on_token       => fun((binary(), map()) -> ok),
    on_interrupt   => fun((map(), map()) -> ok),
    on_resume      => fun((map(), map()) -> ok)
}.
```

`on_tool_call` is the only callback allowed to return `{interrupt, term()}`; this is the standard way to inject a synchronous human-in-the-loop approval between tool invocation and execution.

---

### Interrupt Mechanism: beamai_agent_interrupt

Helpers for inspecting and resuming an interrupted run. Most callers use `beamai_agent:resume/2,3`; the functions below are exported for custom runners.

```erlang
-spec find_interrupt_tool([map()], map()) ->
    {yes, map(), [map()]} | no.

-spec is_interrupt_tool(map(), [map()]) -> boolean().

-spec handle_interrupt(atom(), term(), map(), map()) ->
    {map(), map()}.

-spec build_resume_messages(map(), term()) -> [map()].

-spec resume_action(map(), term(), map()) ->
    {execute, map()} | {result, map()}.

-spec replace_results_by_id([map()], [map()]) -> [map()].

-spec validate_resume_input(map(), term()) ->
    ok | {error, term()}.

-spec get_interrupt_tool_specs(map()) -> [map()].
```

---

### Pause Persistence: beamai_agent_pause

Optional integration with `beamai_pause_store` to survive process and node restarts. Configure the agent's `pause_store` and the loop auto-saves on interrupt and auto-loads when a previous snapshot exists.

```erlang
-spec save(map()) -> ok.
-spec load(map()) -> {ok, map()} | none.
-spec clear(map()) -> ok.
```

Without a configured `pause_store`, all three calls are no-ops or return `none`.

---

### Sub-Agent Delegation: beamai_agent_delegate

Tools that let a parent Agent spawn child Agents, fan out tasks in parallel, and manage them as first-class objects.

```erlang
-spec tool(config()) -> map().
-spec fanout_tool(config()) -> map().
-spec management_tools(config()) -> [map()].

-spec run_many([{map(), binary()}], timeout()) ->
    [beamai_subagent_manager:outcome() | {error, sub_agent_timeout}].

-spec run_many([{map(), binary()}], timeout(),
               fun((map()) -> binary())) ->
    [beamai_subagent_manager:outcome() | {error, sub_agent_timeout}].
```

```erlang
-type config() :: #{
    name := binary(),
    description => binary(),
    subagent := fun((map(), beamai_context:t()) -> map()),
    seed => fun((map(), beamai_context:t()) -> binary()),
    result => fun((map()) -> binary()),
    timeout => timeout()
}.
```

| Function | Purpose |
|----------|---------|
| `tool/1` | One synchronous delegation tool: spawn, await, drop |
| `fanout_tool/1` | Single call accepts a `tasks` list and runs them concurrently |
| `management_tools/1` | Returns a set of management tools (spawn / list / result / kill / restart) |
| `run_many/2,3` | Programmatic concurrent fan-out with timeout and an optional result-extraction function |

---

### Sub-Agent Manager: beamai_subagent_manager

Async registry for child Agents. Backed by a singleton process started by `start_link/0,1`.

```erlang
-spec start_link() -> {ok, pid()} | {error, term()}.
-spec start_link(map()) -> {ok, pid()} | {error, term()}.
-spec ensure_started() -> pid().

-spec spawn(spec()) -> {ok, id()}.
-spec await(id(), timeout()) ->
    outcome() | {error, timeout} | {error, not_found}.
-spec result(id()) -> {ok, outcome()} | {error, not_ready | not_found}.

-spec list() -> [info()].
-spec list(term()) -> [info()].

-spec kill(id()) -> ok | {error, not_found}.
-spec restart(id()) -> {ok, id()} | {error, not_found}.
-spec drop(id()) -> ok.
```

```erlang
-type id()      :: binary().
-type status()  :: running | done | failed | killed.
-type outcome() :: {ok, binary()} | {error, term()}.

-type spec() :: #{
    subagent := map(),
    prompt := binary(),
    result => fun((map()) -> binary()),
    owner => term()
}.

-type info() :: #{
    id := id(),
    status := status(),
    owner := term(),
    started_at := integer(),
    finished_at := integer() | undefined
}.
```

---

### Timeline and Branching: beamai_timeline

Fork-as-new-conversation for multi-branch conversation exploration. Each branch lives in its own `conversation_id` and lineage is recorded through `beamai_branch_store`.

```erlang
-spec fork(deps(), binary()) ->
    {ok, binary()} | {error, fork_error()}.

-spec fork(deps(), binary(), map()) ->
    {ok, binary()} | {error, fork_error()}.

-spec rollback(deps(), binary(), non_neg_integer()) -> ok.

-spec lineage(deps(), binary()) ->
    {ok, beamai_branch_store:branch_record()} | none.

-spec ancestry(deps(), binary()) -> [ancestry_record()].

-spec prune(deps(), binary()) ->
    ok | {error, {has_children, [binary()]}}.
```

```erlang
-type deps() :: #{
    memory := beamai_chat_memory:handle(),
    branch := beamai_branch_store:handle(),
    pause_store => beamai_pause_store:handle()
}.

-type ancestry_record() :: #{
    id := binary(),
    parent := binary() | undefined,
    fork_point := non_neg_integer() | all,
    created_at := integer()
}.

-type fork_error() ::
    empty_source
    | {invalid_at, term()}
    | {target_exists, binary()}.
```

| Function | Purpose |
|----------|---------|
| `fork/2` | Full-copy branch (clone entire history into a new conversation) |
| `fork/3` | Prefix-copy branch with a lineage record |
| `rollback/3` | Destructive truncate to the first N messages |
| `lineage/2` | Read the branch record for a conversation |
| `ancestry/2` | Walk `parent` links backwards, including self at the head |
| `prune/2` | Delete a branch (history + pause snapshot + lineage); refuses when children exist |

---

### Branch Lineage Store

#### beamai_branch_store (Behaviour)

Pluggable store for branch lineage.

```erlang
-spec record(handle(), binary(), branch_record()) -> ok.
-spec get(handle(), binary()) -> {ok, branch_record()} | none.
-spec children(handle(), binary()) -> [binary()].
-spec delete(handle(), binary()) -> ok.
```

```erlang
-type handle() :: {module(), term()}.

-type branch_record() :: #{
    parent := binary() | undefined,
    fork_point := non_neg_integer() | all,
    created_at := integer()
}.

-callback record(Ref :: term(), ConvId :: binary(),
                 Record :: branch_record()) -> ok.
-callback get(Ref :: term(), ConvId :: binary()) ->
    {ok, branch_record()} | none.
-callback children(Ref :: term(), ConvId :: binary()) -> [binary()].
-callback delete(Ref :: term(), ConvId :: binary()) -> ok.
```

#### beamai_branch_store_ets

ETS-backed default implementation.

```erlang
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
-spec stop(atom()) -> ok.
-spec handle(atom()) -> beamai_branch_store:handle().

-spec record(atom(), binary(),
             beamai_branch_store:branch_record()) -> ok.
-spec get(atom(), binary()) ->
    {ok, beamai_branch_store:branch_record()} | none.
-spec children(atom(), binary()) -> [binary()].
-spec delete(atom(), binary()) -> ok.
```

---

### Pause Snapshot Store

#### beamai_pause_store (Behaviour)

Pluggable store for cross-process HITL snapshots.

```erlang
-spec pause_save(handle(), binary(), snapshot()) -> ok.
-spec pause_load(handle(), binary()) -> {ok, snapshot()} | none.
-spec pause_clear(handle(), binary()) -> ok.
```

```erlang
-type handle() :: {module(), term()}.
-type snapshot() :: map().

-callback pause_save(Ref :: term(), ConvId :: binary(),
                     Snapshot :: snapshot()) -> ok.
-callback pause_load(Ref :: term(), ConvId :: binary()) ->
    {ok, snapshot()} | none.
-callback pause_clear(Ref :: term(), ConvId :: binary()) -> ok.
```

#### beamai_pause_store_ets

ETS-backed default implementation.

```erlang
-spec start_link(atom()) -> {ok, pid()} | {error, term()}.
-spec start_link(atom(), map()) -> {ok, pid()} | {error, term()}.
-spec stop(atom()) -> ok.
-spec handle(atom()) -> beamai_pause_store:handle().

-spec pause_save(atom(), binary(),
                beamai_pause_store:snapshot()) -> ok.
-spec pause_load(atom(), binary()) ->
    {ok, beamai_pause_store:snapshot()} | none.
-spec pause_clear(atom(), binary()) -> ok.
```

---

### Agent Utilities: beamai_agent_utils

Helpers shared across the Agent layer. These are exported for advanced use (custom runners, instrumentation); typical user code does not call them directly.

```erlang
-spec extract_content(map()) -> binary().
-spec build_chat_opts(beamai_kernel:kernel(), map()) -> map().

-spec execute_tools(beamai_kernel:kernel(), [map()]) ->
    {[map()], [map()], beamai_context:t()}.

-spec execute_tools(beamai_kernel:kernel(), [map()],
                    beamai_context:t()) ->
    {[map()], [map()], beamai_context:t()}.

-spec execute_tools(beamai_kernel:kernel(), [map()],
                    beamai_context:t(), boolean()) ->
    {[map()], [map()], beamai_context:t()}.

-spec execute_tools(beamai_kernel:kernel(), [map()],
                    beamai_context:t(), boolean(),
                    fun((map()) -> ok)) ->
    {[map()], [map()], beamai_context:t()}.

-spec run_one_tool(beamai_kernel:kernel(), map(),
                   beamai_context:t()) ->
    {map(), map(), beamai_context:writes()}.

-spec tool_error(term()) -> #{error := map()}.
```

`execute_tools/5` is the unified entry: pass `Parallel=true` to fan out tool calls concurrently, and a callback to receive each tool result in real time as it lands.

---

## Behaviour Interfaces

These are the formal callback contracts for pluggable backends. Implement them to add a custom provider, memory backend, conversation store, tool module, or HTTP backend.

### beamai_chat_behaviour

Every LLM provider module implements this. The `stream_chat` callback is optional.

```erlang
-callback create(Provider :: provider(), Opts :: map()) -> config().

-callback chat(Config :: config(), Messages :: [map()]) ->
    {ok, map()} | {error, term()}.

-callback chat(Config :: config(), Messages :: [map()], Opts :: map()) ->
    {ok, map()} | {error, term()}.

-callback stream_chat(Config :: config(), Messages :: [map()],
                     Callback :: fun()) ->
    {ok, map()} | {error, term()}.
```

```erlang
-type provider() :: openai | anthropic | ollama | zhipu
                  | dashscope | deepseek | mock
                  | {custom, module()}.

-type config() :: #{
    provider := provider(),
    module := module(),
    '__llm_config__' := true,
    atom() => term()
}.
```

### beamai_memory_provider

The Agent's cross-turn memory interface. A default implementation lives in `beamai_memory_provider_default`.

```erlang
-spec history(provider(), binary()) -> [message()].
-spec append(provider(), binary(), [message()]) -> ok.
-spec prepare(provider(), binary(), [message()]) -> [message()].
-spec clear(provider(), binary()) -> ok.
-spec default(beamai_chat_memory:handle()) -> provider().
```

```erlang
-callback history(Ref :: term(), ConvId :: binary()) -> [message()].
-callback append(Ref :: term(), ConvId :: binary(),
                 Msgs :: [message()]) -> ok.
-callback prepare(Ref :: term(), ConvId :: binary(),
                  Messages :: [message()]) -> [message()].
-callback clear(Ref :: term(), ConvId :: binary()) -> ok.
```

```erlang
-type provider() :: {module(), term()}.
-type message() :: beamai_message:message().
```

### beamai_chat_memory

The raw per-conversation message store, dispatched by `beamai_memory_provider`. ETS and DETS implementations ship in core.

```erlang
-spec mem_get(handle(), binary()) -> [message()].
-spec mem_add(handle(), binary(), [message()]) -> ok.
-spec mem_clear(handle(), binary()) -> ok.
```

```erlang
-callback mem_get(Ref :: term(), ConvId :: binary()) -> [message()].
-callback mem_add(Ref :: term(), ConvId :: binary(),
                  Msgs :: [message()]) -> ok.
-callback mem_clear(Ref :: term(), ConvId :: binary()) -> ok.
```

```erlang
-type handle() :: {module(), term()}.
-type message() :: beamai_message:message().
```

### beamai_tool_behaviour

Modules that ship tool collections implement this. The Kernel calls `tool_info/0` and `filters/0` if defined; both are optional.

```erlang
-callback tool_info() -> #{
    description => binary(),
    tags => [binary()],
    metadata => map()
}.

-callback tools() -> [beamai_tool:tool_spec()].

-callback filters() -> [beamai_filter:filter()].
```

### beamai_http_behaviour

Every HTTP backend implements this — the built-in `beamai_http_gun`, plus test doubles such as `beamai_llm_fake_backend`.

```erlang
-callback request(Method :: method(),
                  Url :: url(),
                  Headers :: headers(),
                  Body :: body(),
                  Opts :: opts()) -> response().

-callback stream_request(Method :: method(),
                          Url :: url(),
                          Headers :: headers(),
                          Body :: body(),
                          Opts :: opts(),
                          Handler :: chunk_handler()) ->
    stream_response().

-callback ensure_started() -> ok.
-callback close(Ref :: term()) -> ok.
-callback request_meta(Method :: method(),
                       Url :: url(),
                       Headers :: headers(),
                       Body :: body(),
                       Opts :: opts()) -> response_meta().
```

```erlang
-type method() :: get | post | put | delete | head | options | patch.
-type url() :: binary() | string().
-type headers() :: [{binary(), binary()}].
-type body() :: binary() | iodata().

-type opts() :: #{
    timeout => pos_integer(),
    connect_timeout => pos_integer(),
    headers => headers(),
    pool => atom(),
    init_acc => term(),
    forward_headers => boolean()
}.

-type response() :: {ok, term()} | {error, term()}.
-type meta() :: #{status => non_neg_integer() | undefined,
                  headers => headers()}.
-type response_meta() :: {ok, term(), meta()} | {error, term()}.
-type chunk_handler() ::
    fun((binary(), term()) -> {continue, term()} | {done, term()}).
-type stream_response() :: {ok, term()} | {error, term()}.
```

---

## Error Handling

BeamAI APIs follow a consistent convention: `{ok, Result}` on success, `{error, Reason}` on failure. When the LLM client returns `{error, Reason}`, you can wrap it with `beamai_llm_error:from_reason/1,2` to recover a structured `llm_error()` with `type`, `status`, `retryable`, and `retry_after_ms` fields. This is the recommended way to drive retry/backoff loops.

Common error reasons you may see:

| Reason | Where it comes from | Notes |
|--------|--------------------|----|
| `missing_api_key` | `beamai_chat_completion:create/2` | Provider config has no `api_key` and no env fallback |
| `{http_error, StatusCode, Body}` | LLM providers | Raw HTTP failure; wrap with `beamai_llm_error:from_reason/2` for the structured form |
| `{api_error, Details}` | LLM providers | Provider returned a structured error body |
| `{beamai_llm_error, _}` | `beamai_chat_completion:chat/2,3` etc. | Already structured; inspect fields directly |
| `timeout` | Tools under `beamai_filters:timeout_filter/1` | Classified `transient` by `beamai_tool_error:classify/1` |
| `not_found` | `beamai_subagent_manager` | `await/2` or `kill/1` on an unknown id |
| `storage_not_enabled` | `beamai_agent_pause` | `load/1` called when no `pause_store` is configured |
| `{has_children, _}` | `beamai_timeline:prune/2` | Refuses to delete a branch with children |

For the full error taxonomy returned by LLM calls, see [Error Structure: beamai_llm_error](#error-structure-beamai_llm_error).

---

## More Documentation

- [README.md](../README.md) - Project overview (Chinese)
- [README_EN.md](../README_EN.md) - Project overview (English)
- [FILTER.md](FILTER.md) / [FILTER_EN.md](FILTER_EN.md) - Filter onion system, including the `token_transform` fourth hook
- [MEMORY.md](MEMORY.md) / [MEMORY_EN.md](MEMORY_EN.md) - Conversation memory (Memory Filter)
- [OUTPUT_PARSER.md](OUTPUT_PARSER.md) - Output Parser guide
- [DEPENDENCIES.md](DEPENDENCIES.md) / [DEPENDENCIES_EN.md](DEPENDENCIES_EN.md) - Dependency details
- Module READMEs:
  - [beamai_core](../apps/beamai_core/README.md) / [README_EN.md](../apps/beamai_core/README_EN.md)
  - [beamai_llm](../apps/beamai_llm/README.md) / [README_EN.md](../apps/beamai_llm/README_EN.md)
  - [beamai_agent](../apps/beamai_agent/README.md) (Chinese only)