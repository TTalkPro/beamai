# Refactoring Plan: Split `beamai_http_pool` into Purpose-Shaped Pool Instances

**Status:** Implemented — Phases 1–4 landed 2026-07-17 (see docs/HTTP.md for the operator-facing docs)

> **Historical record — superseded in part.** This plan was written while the
> Hackney backend still existed; its Non-Goal 3 ("No drop of Hackney backend")
> and every Hackney-related consideration below reflect that era. `beamai_http_hackney`
> has since been deleted and Gun is the only built-in backend. The backend gate in
> `beamai_llm_http_client:maybe_inject_pool/3` still stands, but now guards against
> non-Gun backends generally (e.g. the test fake) rather than Hackney specifically.
> Left unedited as a record of the decisions made at the time — see docs/HTTP.md
> for current behaviour.
**Target branch:** `feature/http-pool-sharding`
**Owner:** TBD
**Estimated effort:** 4 phases × ~1 day = 4 dev-days (plus 1 day for tests/docs)
**Backwards compat:** Old `http_pool` config key kept working with deprecation warning

---

## 1. Goals & Non-Goals

### Goals

1. **Eliminate config rigidity.** `beamai_http_pool` today hardcodes two `gun:open` options that are not configurable from `sys.config`:
   - `protocols => [http]` (line 313 of `beamai_http_pool.erl` — workaround for Gun 2.1 ALPN single-element match crash)
   - `connect_timeout => maps:get(connection_timeout, Config)` (line 308 — derived from a key name that differs from Gun's option name)

   Both must become first-class pool config knobs so operators can tune TLS handshake time and ALPN per traffic class without forking the module.

2. **Fix global HTTP/2 disable.** Because `protocols` is hardcoded to `[http]` in the single shared pool, *all* LLM traffic — including OpenAI/Anthropic which support HTTP/2 — is forced to HTTP/1.1. Pool-level config will let HTTP/2-capable providers opt back in.

3. **Stop connection-budget contention.** Today `max_connections_per_host=10` is shared by 6 LLM providers across three traffic shapes:
   - **Short chat** — single JSON request/response, ~1–5 s held.
   - **SSE stream** — one connection held for 30 s+ while emitting deltas; a busy stream blocks 9 other short requests to the same host.
   - **Long-poll async** — Zhipu's `do_get_request/3` polls `async-result/<task_id>` until the task transitions out of `PROCESSING`; can pin a connection for minutes.

   Splitting the pool lets each traffic shape carry its own budget without starving the others.

4. **Keep default behavior identical per traffic class** for users that don't override anything: every pool ships with today's single-pool defaults (10/30 s/60 s/`[http]`). The one intended difference is that traffic classes no longer share (and starve) one budget — see the aggregate note in Section 2.1.

### Non-Goals

1. **No per-provider pools.** Each provider keeps a single purpose-shaped pool it shares across all its requests. We are *not* introducing `http_pool_openai`, `http_pool_anthropic`, etc.
2. **No rewrite of any provider.** Provider modules (`beamai_llm_provider_*`) get at most a one-line pool-name override at the call site that needs it (Zhipu async). No new request bodies, parsers, or error paths.
3. **No drop of Hackney backend.** `beamai_http_hackney` keeps using its own `hackney_pool`; this refactor only touches the Gun path. Hackney continues to honor the existing `pool => atom()` opt in its request opts.
4. **No HTTP/2 re-enable by default.** `protocols => [http]` stays the default everywhere — the Gun 2.1 ALPN `badmatch` regression is still a known hazard. Operators opt in per pool.
5. **No change to `beamai_http_behaviour`** callbacks. Behaviour shape stays; pool-name is plumbed through the existing `pool => atom()` opt (already defined in the behaviour type at `beamai_http_behaviour.erl:41` but currently unused by Gun).
6. **No changes to `beamai_core_app`** (`start_link` is unchanged) or `beamai_core_sup` start_link API.

---

## 2. Target Architecture

### 2.1 Three purpose-shaped pool instances

Registered names (atoms):

| Pool name               | Use case                                | Default `max_connections_per_host` | Default `connect_timeout` | Default `idle_timeout` | Default `protocols` |
|-------------------------|-----------------------------------------|------------------------------------|---------------------------|------------------------|---------------------|
| `http_pool_short`       | Synchronous chat / tool-call requests   | 10                                 | 30 000 ms                 | 60 000 ms              | `[http]`            |
| `http_pool_stream`      | SSE streaming chat completions          | 10                                 | 30 000 ms                 | 60 000 ms              | `[http]`            |
| `http_pool_longpoll`    | Async task status polling (Zhipu)       | 10                                 | 30 000 ms                 | 60 000 ms              | `[http]`            |

Rationale:
- **All three pools ship with identical defaults, equal to today's single-pool defaults.** This keeps Goal 4 honest: a no-config user sees exactly today's per-traffic-class behavior — each class gets the same 10-conn/60s-idle budget it effectively competes for now, except classes no longer starve each other. Purpose-tuned values (bigger stream budget, longer longpoll idle) are *recommendations* documented in Appendix B and applied by operators via `http_pools` (Phase 3), not baked-in defaults.
- Note the aggregate: with three pools, a single host can now hold up to 3×10 connections across traffic classes (vs. 10 total today). This is the intended effect of eliminating cross-class contention (Goal 3), not an accident — but it is *not* bit-identical at the host level, and the docs must say so.
- All three default to `[http]` (HTTP/1.1) to preserve the current workaround; HTTP/2 is opt-in per pool.

### 2.2 Supervisor tree

```
beamai_core_sup (one_for_one, intensity=5, period=10)
├── http_pool_short     (gen_server, registered as http_pool_short)
├── http_pool_stream    (gen_server, registered as http_pool_stream)
└── http_pool_longpoll  (gen_server, registered as http_pool_longpoll)
```

Each pool is an *independent* `gen_server`. A crash in one pool (e.g., OOM while holding 20 SSE streams) does *not* take down the other two — that's why we keep `one_for_one`.

ASCII provider routing:

```
                         ┌────────────────────────┐
   chat(LLM, Msgs)  ───▶ │  beamai_http_gun       │
   stream_chat(...) ───▶ │   ├─ request_async/5   │
   async_poll(...)  ───▶ │   │   └─ get_connection │──┐
                         └────────────┬───────────┘  │
                                      │              │ pool name in opts
                                      ▼              │
                          ┌────────────────────┐    │
                          │ beamai_http (core) │◀───┘  (passes pool opt through)
                          └────────────────────┘
                                      │
        ┌─────────────────────┬───────┴────────┬──────────────────────┐
        ▼                     ▼                ▼                      ▼
  http_pool_short       http_pool_stream   http_pool_longpoll     (no pool
  (registered:          (registered:        (registered:           passed →
   http_pool_short)      http_pool_stream)   http_pool_longpoll)   defaults to
                                                                 http_pool_short)
```

### 2.3 Process registry update

`beamai_core.app.src` `{registered, [...]}` list goes from:

```erlang
{registered, [beamai_core_sup, beamai_http_pool]}
```

to:

```erlang
{registered, [beamai_core_sup,
              http_pool_short,
              http_pool_stream,
              http_pool_longpoll]}
```

The old atom `beamai_http_pool` is removed from the registered list (the process is gone, not renamed). See Section 5 for the deprecation path.

---

## 3. Configuration Model

### 3.1 New application env key: `http_pools`

```erlang
%% sys.config — new style (preferred)
{beamai_core, [
    {http_backend, beamai_http_gun},
    {http_pools, #{
        http_pool_short => #{
            max_connections_per_host => 10,
            connect_timeout => 30000,
            idle_timeout => 60000,
            protocols => [http]
        },
        http_pool_stream => #{
            max_connections_per_host => 20,
            connect_timeout => 30000,
            idle_timeout => 120000,
            protocols => [http]
        },
        http_pool_longpoll => #{
            max_connections_per_host => 5,
            connect_timeout => 30000,
            idle_timeout => 300000,
            protocols => [http]
        }
    }}
]}.
```

(The example above shows an *operator-tuned* config; the built-in defaults are uniform — see Section 3.2.)

Per-pool config shape:

```erlang
-type pool_config() :: #{
    name => atom(),                      %% pool registered name, required when set via env
    max_connections_per_host => pos_integer(),
    connect_timeout => pos_integer(),    %% ms — passed to gun:open's connect_timeout
    idle_timeout => pos_integer(),       %% ms — passed to do_cleanup_idle
    protocols => [atom()]                %% each element: http | http2 — passed to gun:open's protocols
}.
```

Key naming notes (important migration detail):

| Old `http_pool` key | New `http_pools` per-pool key | Reason                                                                                      |
|---------------------|-------------------------------|---------------------------------------------------------------------------------------------|
| `max_connections_per_host` | `max_connections_per_host` | unchanged                                                                                  |
| `connection_timeout`       | `connect_timeout`           | renamed to match `gun:open`'s option name and the documented field name in Section 1        |
| `idle_timeout`             | `idle_timeout`              | unchanged                                                                                  |
| *(absent)*                 | `protocols`                 | NEW — defaults to `[http]`; previously hardcoded in `create_new_connection/5`              |

The rename `connection_timeout` → `connect_timeout` is a **breaking change** to the old key, but it only affects the `http_pool` legacy config (Section 5). New `http_pools` users always use the Gun-aligned name.

### 3.2 Default pool configs (when user doesn't override)

If `http_pools` is missing entirely, the three pools start with these defaults baked into `beamai_core_sup:default_pool_config/1`. **All three are identical and equal to today's single-pool defaults** — purpose-tuning is an operator decision (Appendix B), not a default:

```erlang
default_pool_config(_PoolName) ->
    #{max_connections_per_host => 10,
      connect_timeout => 30000,
      idle_timeout => 60000,
      protocols => [http]}.
```

If `http_pools` *is* set but a specific pool is missing, that pool uses its default (mix-and-match is allowed). Unknown pool names raise at supervisor init — see Section 4.2.

### 3.3 Migration of the old `http_pool` key

If the old `http_pool` env is still present (legacy sys.config), it must keep working. Migration logic in `beamai_core_sup`:

1. Read legacy `application:get_env(beamai_core, http_pool, undefined)`.
2. If present, emit a deprecation warning (Section 5) **once at supervisor init**.
3. Map its keys onto the three new pools, falling back to per-pool defaults:
   ```erlang
   legacy_to_pools(Legacy) ->
       Merged = maps:merge(default_pool_config(http_pool_short), Legacy),
       %% Legacy `connection_timeout` (old name) wins over `connect_timeout` (new name)
       Merged1 = case maps:find(connection_timeout, Legacy) of
           {ok, V} -> Merged#{connect_timeout => V};
           error -> Merged
       end,
       #{
           http_pool_short   => Merged1,
           http_pool_stream  => Merged1,
           http_pool_longpoll => Merged1
       }.
   ```
   This makes every pool receive the same merged config, preserving the single-pool semantics for users who only set `http_pool`.
4. If the user sets *both* `http_pool` and `http_pools`, the new `http_pools` wins (the legacy value is ignored after the warning).

---

## 4. Code Changes (file-by-file)

### 4.1 `apps/beamai_core/src/http/beamai_http_pool.erl`

This module is the workhorse. Almost every function changes.

#### 4.1.1 `-module`/exports (lines 35–48)

Drop the singleton API; replace with name-parameterized API:

```erlang
-module(beamai_http_pool).

-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]).
-export([get_connection/2, get_connection/3]).           %% (PoolName, Url) and (PoolName, Url, Opts)
-export([return_connection/2]).                            %% (PoolName, ConnPid)
-export([connection_failed/2]).                            %% (PoolName, ConnPid)
-export([close_all/1]).                                    %% (PoolName)
-export([stats/1]).                                        %% (PoolName)

%% gen_server callbacks (unchanged)
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
```

Remove: `start_link/0`, `get_connection/1,2`, `return_connection/1`, `connection_failed/1`, `close_all/0`, `stats/0` — all singletons.

#### 4.1.2 Type updates (lines 54–69)

```erlang
-record(state, {
    name :: atom(),                                       %% NEW
    pools = #{} :: #{binary() => [pid()]},
    conn_info = #{} :: #{pid() => map()},
    in_use = #{} :: #{pid() => true},
    config :: pool_config()
}).

-type pool_name() :: http_pool_short | http_pool_stream | http_pool_longpoll.

-type pool_config() :: #{
    name => atom(),                                       %% optional, set by init from app env
    max_connections_per_host => pos_integer(),
    connect_timeout => pos_integer(),
    idle_timeout => pos_integer(),
    protocols => [http | http2]
}.
```

#### 4.1.3 New `start_link/2` (replaces both `start_link/0,1`)

```erlang
-spec start_link(pool_name(), pool_config()) -> {ok, pid()} | {error, term()}.
start_link(PoolName, Config) ->
    gen_server:start_link({local, PoolName}, ?MODULE, [PoolName, Config], []).
```

The arg list is `[PoolName, Config]` so `init/1` (kept single-arg for gen_server) can destructure:

```erlang
init([PoolName, Config]) ->
    FullConfig = merge_config(PoolName, Config),
    erlang:send_after(?CLEANUP_INTERVAL, self(), cleanup_idle),
    {ok, #state{name = PoolName, config = FullConfig}}.
```

#### 4.1.4 New `get_connection/2,3` (lines 95–103 currently)

```erlang
-spec get_connection(pool_name(), binary() | string()) -> {ok, pid()} | {error, term()}.
get_connection(PoolName, Url) ->
    get_connection(PoolName, Url, #{}).

-spec get_connection(pool_name(), binary() | string(), map()) -> {ok, pid()} | {error, term()}.
get_connection(PoolName, Url, Opts) ->
    case whereis(PoolName) of
        undefined ->
            {error, {pool_not_started, PoolName}};
        _ ->
            gen_server:call(PoolName, {get_connection, Url, Opts}, infinity)
    end.
```

The old `ensure_started/0` (line 206) becomes per-pool and returns `{error, _}` instead of throwing — `throw` crossed process boundaries and was awkward.

#### 4.1.5 `return_connection/2`, `connection_failed/2`, `close_all/1`, `stats/1`

Direct one-liners routing to the named gen_server:

```erlang
return_connection(PoolName, ConnPid) ->
    gen_server:cast(PoolName, {return_connection, ConnPid}).

connection_failed(PoolName, ConnPid) ->
    gen_server:cast(PoolName, {connection_failed, ConnPid}).

close_all(PoolName) ->
    gen_server:call(PoolName, close_all).

stats(PoolName) ->
    gen_server:call(PoolName, stats).
```

#### 4.1.6 `merge_config/2` — was `merge_config/1` at line 217

```erlang
merge_config(_PoolName, Config) ->
    Defaults = #{
        max_connections_per_host => ?DEFAULT_MAX_CONNECTIONS,
        connect_timeout => ?DEFAULT_CONN_TIMEOUT,
        idle_timeout => ?DEFAULT_IDLE_TIMEOUT,
        protocols => [http]
    },
    %% Per-pool app env override (Section 3.2/3.3)
    %% Caller (sup) has already merged http_pool / http_pools into Config,
    %% so we only merge module-level defaults + per-call overrides here.
    maps:merge(Defaults, Config).
```

The supervisor (4.2) is responsible for resolving `application:get_env(beamai_core, http_pools, ...)` and merging legacy `http_pool`. The pool module itself no longer reads app env — this is the key change that makes per-pool config possible.

#### 4.1.7 `create_new_connection/5` (lines 303–342) — hardcoding removal

Current code (line 307–315):

```erlang
GunOpts = #{
    connect_timeout => ConnTimeout,
    transport => Transport,
    %% Gun 2.1 的 TCP 连接路径做 [Protocol] = maps:get(protocols, Opts, [http]) 单元素匹配，
    %% 传 [http2, http] 会 {badmatch, [http2, http]} 崩掉所有连接。
    %% 用 [http]（HTTP/1.1）保证 HTTP/HTTPS 均可用；TLS 路径仍可正常工作。
    protocols => [http],
    tls_opts => get_tls_opts()
},
```

New code:

```erlang
create_new_connection(Host, Port, Transport, HostKey,
                      #state{config = Config} = State) ->
    ConnTimeout = maps:get(connect_timeout, Config),
    Protocols   = maps:get(protocols, Config),
    validate_protocols(Protocols),                       %% NEW guard, see Section 7 risk 2
    GunOpts = #{
        connect_timeout => ConnTimeout,
        transport => Transport,
        protocols => Protocols,
        tls_opts => get_tls_opts()
    },
    %% ... rest unchanged
```

And the new guard:

```erlang
validate_protocols(Protocols) when is_list(Protocols), Protocols =/= [] ->
    lists:foreach(fun
        (http)  -> ok;
        (http2) -> ok;
        (Other) -> error({invalid_protocol, Other})
    end, Protocols),
    ok.
```

This is the only line where the Gun 2.1 workaround comment lives now, and it's behind a configurable knob. The comment moves to `docs/HTTP.md` (Section 9, Acceptance).

#### 4.1.8 `init/1` (line 129)

Already shown in 4.1.3. Key delta: takes `[PoolName, Config]`, sets `name` in state, and **no longer reads `application:get_env(beamai_core, http_pool, ...)`** — that responsibility moves to the supervisor (4.2).

#### 4.1.9 `stats/1` reply — add config echo (line 150)

```erlang
handle_call(stats, _From, #state{name = Name, pools = Pools, conn_info = ConnInfo,
                                in_use = InUse, config = Config} = State) ->
    Stats = #{
        name => Name,                                     %% NEW
        pools => maps:map(fun(_, Conns) -> length(Conns) end, Pools),
        total_connections => maps:size(ConnInfo),
        in_use => maps:size(InUse),
        idle => maps:size(ConnInfo) - maps:size(InUse),
        config => Config                                  %% NEW (operator-facing)
    },
    {reply, Stats, State};
```

The config echo makes `:observer.start()` and ad-hoc introspection easy without needing to grep sys.config.

#### 4.1.10 Lines NOT changing

- `parse_url/1` (line 432)
- `default_port/1` (line 447)
- `make_host_key/3` (line 454)
- `to_charlist/1` (line 459)
- `get_tls_opts/0` (line 464)
- `do_cleanup_idle/1` (line 392) — keeps reading `idle_timeout` from `Config`, now via the new key name `connect_timeout`/`idle_timeout` etc.
- `do_get_connection/3`, `do_return_connection/2`, `do_remove_connection/2`, `do_close_all/1` — algorithmically identical; only the data they read from `state` changes.

### 4.2 `apps/beamai_core/src/beamai_core_sup.erl`

Replace the single `http_pool_spec/0` (lines 74–84) with `http_pool_specs/0` returning a list, and add legacy config migration.

#### 4.2.1 Replace `get_children/0` (line 53)

```erlang
get_children() ->
    case should_start_http_pool() of
        true -> http_pool_specs();
        false -> []
    end.
```

#### 4.2.2 New `http_pool_specs/0`

```erlang
http_pool_specs() ->
    Pools = resolve_pool_configs(),
    [pool_spec(PoolName, PoolConfig)
     || {PoolName, PoolConfig} <- maps:to_list(Pools)].

pool_spec(PoolName, PoolConfig) ->
    #{
        id => PoolName,
        start => {beamai_http_pool, start_link, [PoolName, PoolConfig]},
        restart => permanent,
        shutdown => 5000,
        type => worker,
        modules => [beamai_http_pool]
    }.
```

#### 4.2.3 New `resolve_pool_configs/0`

```erlang
resolve_pool_configs() ->
    Legacy = application:get_env(beamai_core, http_pool, undefined),
    New    = application:get_env(beamai_core, http_pools, #{}),
    case Legacy of
        undefined -> ok;
        _ ->
            logger:warning("beamai_core: ~tp is deprecated, "
                           "use ~tp with per-pool configs instead",
                           [http_pool, http_pools])
    end,
    Defaults = #{
        http_pool_short   => default_pool_config(http_pool_short),
        http_pool_stream  => default_pool_config(http_pool_stream),
        http_pool_longpoll => default_pool_config(http_pool_longpoll)
    },
    Merged = case Legacy of
        undefined -> Defaults;
        _ -> legacy_to_pools(Legacy, Defaults)            %% see 3.3
    end,
    %% New wins over legacy-derived
    Final = maps:fold(fun(PoolName, PoolCfg, Acc) ->
        Acc#{PoolName => maps:merge(maps:get(PoolName, Merged), PoolCfg)}
    end, Merged, New),
    validate_pool_names(maps:keys(Final)),
    Final.
```

#### 4.2.4 New helpers

```erlang
%% 三个池默认值完全一致，等于今天单池的默认值（见 Section 3.2）
default_pool_config(_PoolName) ->
    #{max_connections_per_host => 10, connect_timeout => 30000,
      idle_timeout => 60000, protocols => [http]}.

legacy_to_pools(Legacy, Defaults) ->
    Merged = maps:fold(fun(PoolName, Default, Acc) ->
        Base = maps:merge(Default, Legacy),
        %% handle connection_timeout (old) -> connect_timeout (new) rename
        Base1 = case maps:find(connection_timeout, Legacy) of
            {ok, V} -> Base#{connect_timeout => V};
            error -> Base
        end,
        Acc#{PoolName => Base1}
    end, #{}, Defaults).

validate_pool_names(Names) ->
    Known = [http_pool_short, http_pool_stream, http_pool_longpoll],
    case Names -- Known of
        [] -> ok;
        Unknown ->
            error({invalid_pool_names, Unknown})
    end.
```

`error/1` from a supervisor `init/1` makes the application fail to start with a clear message — better than silently spawning a phantom pool.

#### 4.2.5 Lines NOT changing

- `start_link/0` (line 28)
- `init/1` (line 37)
- `should_start_http_pool/0` (line 61)

### 4.3 `apps/beamai_core/src/beamai_core.app.src`

#### 4.3.1 `registered` list (line 4)

```erlang
{registered, [beamai_core_sup,
              http_pool_short,
              http_pool_stream,
              http_pool_longpoll]},
```

Remove `beamai_http_pool`. Drop `{pool, [http_pool_short, http_pool_stream, http_pool_longpoll]}` if/when the supervisor adds one (currently each pool is a child of the top-level supervisor, not a sub-supervisor — keep this simple).

#### 4.3.2 `modules` list (line 40)

Unchanged. The `beamai_http_pool` module still exists; it just spawns per-name instances.

### 4.4 `apps/beamai_core/src/http/beamai_http_gun.erl`

Plumb the pool name through every call that touches the pool.

#### 4.4.1 Add pool-name resolution helper (top of internal section, ~line 158)

```erlang
%% Resolve pool name from Opts; default to http_pool_short.
%% Returns either {ok, PoolName} or {error, Reason}.
resolve_pool_name(Opts) ->
    case maps:get(pool, Opts, http_pool_short) of
        P when P =:= http_pool_short;
               P =:= http_pool_stream;
               P =:= http_pool_longpoll ->
            {ok, P};
        Other ->
            {error, {invalid_pool_name, Other}}
    end.
```

#### 4.4.2 `request/5` (line 49)

```erlang
-spec request(atom(), binary() | string(), [{binary(), binary()}],
              binary(), map()) -> {ok, term()} | {error, term()}.
request(Method, Url, Headers, Body, Opts) ->
    ensure_started(),
    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
    case request_async(Method, Url, Headers, Body, Opts) of
        {ok, ConnPid, StreamRef, PoolName} ->
            Result = await_response(ConnPid, StreamRef, Timeout),
            beamai_http_pool:return_connection(PoolName, ConnPid),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.
```

#### 4.4.3 `request_meta/5` (line 69)

```erlang
request_meta(Method, Url, Headers, Body, Opts) ->
    ensure_started(),
    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
    case request_async(Method, Url, Headers, Body, Opts) of
        {ok, ConnPid, StreamRef, PoolName} ->
            Result = receive_response_meta(ConnPid, StreamRef, <<>>, undefined, [], Timeout),
            beamai_http_pool:return_connection(PoolName, ConnPid),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.
```

#### 4.4.4 `stream_request/6` (line 86)

```erlang
stream_request(Method, Url, Headers, Body, Opts, Handler) ->
    ensure_started(),
    Timeout = maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
    InitAcc = maps:get(init_acc, Opts, <<>>),
    FwdHeaders = maps:get(forward_headers, Opts, false),
    case request_async(Method, Url, Headers, Body, Opts) of
        {ok, ConnPid, StreamRef, PoolName} ->
            Result = stream_receive_loop(ConnPid, StreamRef, InitAcc, Handler, Timeout, FwdHeaders),
            beamai_http_pool:return_connection(PoolName, ConnPid),
            Result;
        {error, Reason} ->
            {error, Reason}
    end.
```

#### 4.4.5 `request_async/5` (line 110) — return tuple changes from 3-tuple to 4-tuple

```erlang
-spec request_async(atom(), binary() | string(), [{binary(), binary()}],
                    binary(), map()) ->
    {ok, pid(), reference(), pool_name()} | {error, term()}.
request_async(Method, Url, Headers, Body, Opts) ->
    case resolve_pool_name(Opts) of
        {error, _} = Err -> Err;
        {ok, PoolName} ->
            UrlBin = beamai_utils:to_binary(Url),
            case beamai_http_pool:get_connection(PoolName, UrlBin) of
                {ok, ConnPid} ->
                    {Path, Query} = parse_path_and_query(UrlBin),
                    FullPath = case Query of
                        <<>> -> Path;
                        _ -> <<Path/binary, "?", Query/binary>>
                    end,
                    ReqHeaders = prepare_headers(Headers),
                    StreamRef = case Method of
                        get     -> gun:get(ConnPid, FullPath, ReqHeaders);
                        head    -> gun:head(ConnPid, FullPath, ReqHeaders);
                        delete  -> gun:delete(ConnPid, FullPath, ReqHeaders);
                        post    -> gun:post(ConnPid, FullPath, ReqHeaders,
                                            beamai_utils:encode_body(Body));
                        put     -> gun:put(ConnPid, FullPath, ReqHeaders,
                                           beamai_utils:encode_body(Body));
                        patch   -> gun:patch(ConnPid, FullPath, ReqHeaders,
                                             beamai_utils:encode_body(Body));
                        options -> gun:options(ConnPid, FullPath, ReqHeaders)
                    end,
                    {ok, ConnPid, StreamRef, PoolName};
                {error, Reason} ->
                    {error, {connection_failed, Reason}}
            end
    end.
```

The 4-tuple return is a behavior change. **Update all 3 callers** (`request/5`, `request_meta/5`, `stream_request/6`) to destructure the new shape. Note: `request_async/5` **is publicly exported** from `beamai_http_gun` (line 27, under "额外 API（用于高级场景）"). In-repo callers are only the 3 wrappers in the same module, but the 3→4 tuple change breaks any downstream user of this advanced API — it must appear under "Breaking changes" in CHANGELOG (Acceptance criterion 7).

#### 4.4.6 Lines NOT changing

- `ensure_started/0` (line 42)
- `await_response/3` (line 152)
- `receive_response/4`, `receive_response_meta/6`, `receive_error_body/5`, `receive_error_body_meta/6` (lines 165–269) — pure stream consumption, pool-agnostic
- `stream_receive_loop/6` (line 276)
- `forward_headers/4` (line 314)
- `parse_path_and_query/1` (line 325)
- `prepare_headers/1` (line 339)

### 4.5 `apps/beamai_core/src/http/beamai_http.erl`

The `pool => atom()` opt already exists in the `options()` type (line 70). All this change does is **ensure the opt flows into the backend unchanged**.

#### 4.5.1 `do_request/6` (line 273) — pass Opts through unchanged

Currently:

```erlang
Backend:request(Method, UrlBin, Headers, BodyBin, Opts).
```

`Opts` already includes `pool => ...` if the caller set it, because `Opts` is passed through unmodified. **No code change needed here** — but add a one-line comment to make the contract explicit:

```erlang
%% Pool selection: caller passes `Opts#{pool => http_pool_short | ...}`.
%% When omitted, beamai_http_gun defaults to http_pool_short.
Backend:request(Method, UrlBin, Headers, BodyBin, Opts),
```

#### 4.5.2 `do_request_meta/6` (line 298) — same, no functional change

#### 4.5.3 `stream_request/5,6` (line 201) — same, no functional change

The Hackney backend (`beamai_http_hackney:request/5` line 46) already extracts `pool => maps:get(pool, Opts, default)` and passes it to hackney. That contract is preserved.

### 4.6 `apps/beamai_llm/src/beamai_llm_http_client.erl`

Add a helper to inject the right pool name based on request type, and apply it to both the synchronous and streaming request paths.

#### 4.6.1 New helper (private section, ~line 142)

```erlang
%% @private Select the appropriate pool for a given request shape.
%% `request_type` is one of: chat | stream | async_poll.
-spec select_pool(chat | stream | async_poll) -> atom().
select_pool(chat)       -> http_pool_short;
select_pool(stream)     -> http_pool_stream;
select_pool(async_poll) -> http_pool_longpoll.

%% @doc Inject the pool name for a request shape — ONLY when the active
%% backend is the Gun backend. `beamai_llm_http_client` is backend-agnostic:
%% it calls `beamai_http`, which dispatches to whatever backend is configured.
%% Passing a Gun pool atom (`http_pool_short` etc.) to `beamai_http_hackney`
%% would be interpreted as a *hackney* pool name that was never started.
%% Exported so providers that call `beamai_http` directly (Zhipu async poll)
%% can reuse the same gate.
-spec maybe_inject_pool(chat | stream | async_poll, map()) -> map().
maybe_inject_pool(RequestType, HttpOpts) ->
    case beamai_http:get_backend() of
        beamai_http_gun -> HttpOpts#{pool => select_pool(RequestType)};
        _ -> maps:remove(pool, HttpOpts)
    end.
```

#### 4.6.2 `request/5` (line 69)

Inject pool into `HttpOpts` *before* calling `beamai_http`:

```erlang
request(Url, Headers, Body, Opts, ResponseParser) ->
    HttpOpts0 = (maps:without([pool], Opts))#{
        timeout => maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
        connect_timeout => maps:get(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT)
    },
    HttpOpts = maybe_inject_pool(chat, HttpOpts0),
    %% ... rest unchanged but uses HttpOpts
```

Use `maps:without([pool], Opts)` to defend against a caller explicitly setting `pool => something_else` at this layer — if they want a non-default pool, they should call `beamai_http` directly with their own opts.

#### 4.6.3 `do_stream_request/7` (line 178)

```erlang
do_stream_request(Url, Headers, Body, Opts, Callback, Accumulator, Finalizer) ->
    OnHeaders = maps:get(on_headers, Opts, undefined),
    StreamBody = Body#{<<"stream">> => true},
    HttpOpts = maybe_inject_pool(stream, #{
        timeout => maps:get(timeout, Opts, ?DEFAULT_TIMEOUT),
        connect_timeout => maps:get(connect_timeout, Opts, ?DEFAULT_CONNECT_TIMEOUT),
        headers => Headers,
        forward_headers => is_function(OnHeaders, 1),
        init_acc => #{buffer => <<>>, acc => init_stream_acc(),
                     callback => Callback, accumulator => Accumulator}
    }),
    %% ... rest unchanged
```

#### 4.6.4 `request_with_headers/6` (line 91)

Update to also pass `pool => select_pool(chat)` into its `HttpOpts`.

#### 4.6.5 Lines NOT changing

- `init_stream_acc/0`, `finalize_stream/1` (lines 278, 289)
- `parse_sse/1`, `parse_sse_lines/2`, `safe_decode_json/1` (lines 244–270)
- `default_accumulator/2` (line 300)
- The 5-arg public `request/4` (line 62) — just delegates

### 4.7 `apps/beamai_llm/src/providers/beamai_llm_provider_zhipu.erl`

Only one site needs editing: `do_get_request/3` (line 227) used by `get_async_result/2` (line 206).

#### 4.7.1 `do_get_request/3` (line 227)

```erlang
do_get_request(Url, Headers, Opts) ->
    HttpOpts = beamai_llm_http_client:maybe_inject_pool(async_poll, #{
        timeout => beamai_llm_provider_common:request_timeout(Opts, zhipu),
        connect_timeout => maps:get(connect_timeout, Opts, ?ZHIPU_CONNECT_TIMEOUT),
        headers => Headers
    }),
    case beamai_http:get(Url, #{}, HttpOpts) of
        %% ... unchanged
    end.
```

One-line addition (via the backend-gated helper from 4.6.1 — do NOT hardcode `pool => http_pool_longpoll` here, or a Hackney-backend deployment would pass a Gun pool atom to hackney). This makes Zhipu's async-task status polling pin a small number of long-lived connections in `http_pool_longpoll` instead of competing with synchronous chat traffic for `http_pool_short` slots.

#### 4.7.2 Other providers — no change

OpenAI, Anthropic, DeepSeek, DashScope, Ollama all use only `chat`/`stream_chat`, which route through `beamai_llm_http_client:request/5` and `do_stream_request/7`. They get the right pool automatically via `select_pool/1`. **No provider file gets edited beyond zhipu.**

#### 4.7.3 Lines NOT changing in zhipu

- `chat/2`, `stream_chat/3` (lines 124, 131) — delegate to `chat_openai`/`chat_anthropic`
- `chat_openai/2`, `chat_anthropic/2`, `stream_chat_openai/3`, `stream_chat_anthropic/3` (lines 138, 164, 150, 175) — call `beamai_llm_http_client` which now picks the right pool
- `async_chat/2` (line 193) — *initial* async submission is a normal `chat`-shaped request; it should route to `http_pool_short`, which is what `beamai_llm_http_client:request/5` already does. **Don't override it.**
- `get_async_result/2` (line 206) — calls `do_get_request/3`, which now picks `http_pool_longpoll`
- All request/header/body builders (lines 247–327)

---

## 5. Backwards Compatibility

### 5.1 Legacy `http_pool` config

Detection is in `beamai_core_sup:resolve_pool_configs/0` (Section 4.2.3). When `application:get_env(beamai_core, http_pool, undefined)` returns anything non-undefined:

1. **Emit a one-time `logger:warning`** at supervisor init, naming the deprecated key and pointing at `http_pools`.
2. Apply the legacy value uniformly to all three new pools via `legacy_to_pools/2`. This preserves current single-pool behavior bit-identically:
   - `max_connections_per_host=10` → all three pools get 10
   - `connection_timeout=30000` → all three pools get `connect_timeout=30000` (renamed)
   - `idle_timeout=60000` → all three pools get 60000
   - `protocols` not present → all three pools get `[http]` (the hardcoded default)
3. If the user sets *both* `http_pool` *and* `http_pools`, the new `http_pools` wins per-pool. The legacy value is still logged as a warning.

### 5.2 Behavior preservation matrix

| Scenario                                          | Before refactor                                 | After refactor (with legacy `http_pool` set)                                              |
|---------------------------------------------------|-------------------------------------------------|-------------------------------------------------------------------------------------------|
| User has no sys.config for either key             | Pool starts with module defaults (`max_connections_per_host=10`, `connection_timeout=30000`, `idle_timeout=60000`, `protocols=[http]`) | All three pools start with those same defaults — identical *per traffic class*; note the per-host aggregate across classes can now reach 3×10 (Section 2.1) |
| User has `http_pool => #{max_connections_per_host => 50}` only | Single pool with 50 conns                       | All three pools get 50 conns (matches old behavior, just split across 3 instances)       |
| User has `http_pool => #{protocols => [http2, http]}` | Hardcoded `[http]` — setting ignored           | All three pools get `protocols=[http2, http]` — operators can finally tune (Section 7 risk 4) |
| User has `http_pools => #{...}` only               | N/A                                             | Per-pool overrides applied; no warning                                                    |
| User has both                                     | N/A                                             | `http_pools` wins per-pool; legacy still logged as warning                                |

### 5.3 Hackney backend — pool injection MUST be backend-gated

`beamai_http_hackney` (line 46) already reads `pool => atom()` from opts and passes it to hackney as a *hackney* pool name. `beamai_llm_http_client` is backend-agnostic — it calls `beamai_http`, which dispatches to whatever backend `http_backend` names. So an unconditional `pool => http_pool_short` injection would leak Gun pool atoms into hackney, naming a hackney pool that was never started.

The fix is `maybe_inject_pool/2` (Section 4.6.1): pool injection happens **only when `beamai_http:get_backend() =:= beamai_http_gun`**. When Hackney (or any other backend, including the test fake) is active, no `pool` key is injected and hackney keeps its existing `default` pool behavior.

- Zhipu's `do_get_request/3` (4.7.1) reuses the same exported helper — no call site hardcodes a Gun pool atom.
- Verify with a Hackney-mode integration test (Section 6): with `http_backend => beamai_http_hackney`, assert the opts reaching `Backend:request/5` contain no `pool` key (or only a caller-supplied one).

### 5.4 Module API breakage

- `beamai_http_pool:get_connection/1,2` → removed. Callers (only `beamai_http_gun`) updated to `get_connection/2,3`.
- `beamai_http_pool:return_connection/1` → removed. Callers updated to `return_connection/2`.
- `beamai_http_pool:start_link/0,1` → removed. Only the supervisor calls `start_link/2` now.
- `beamai_http_gun:request_async/5` return tuple grew from 3 to 4 elements. **This is a public exported API** (advanced-use export at `beamai_http_gun.erl:27`), not an internal helper. The 3 callers in the same file are updated; no other callers exist in the repo (`grep -rn "request_async"` shows only the gun module), but downstream users of the advanced API are broken — CHANGELOG entry required.
- `beamai_core.app.src` `registered` list changed. External code that depended on `whereis(beamai_http_pool)` would break — but no such callers exist (`grep -rn "beamai_http_pool" apps/` returns only the four files this plan touches).

---

## 6. Testing Strategy

### 6.1 New test module: `apps/beamai_core/test/beamai_http_pool_tests.erl`

EUnit. Each test starts an isolated pool with a unique registered name (e.g., via `start_link(http_pool_short, Config)` after unregistering any leftover process) to avoid interference.

#### 6.1.1 `config_merging_test_/0`

Verify that the supervisor's `resolve_pool_configs/0` correctly merges defaults + legacy + new. (`resolve_pool_configs_for_test/0` is not a normal export — expose it via `-ifdef(TEST). -export([...]). -endif.` in `beamai_core_sup`.)

```erlang
config_merging_test_() ->
    [
     {"defaults when neither env is set",
      fun() ->
          application:unset_env(beamai_core, http_pool),
          application:unset_env(beamai_core, http_pools),
          Pools = beamai_core_sup:resolve_pool_configs_for_test(),
          ?assertEqual(10, maps:get(max_connections_per_host,
                                     maps:get(http_pool_short, Pools))),
          ?assertEqual([http], maps:get(protocols,
                                        maps:get(http_pool_stream, Pools)))
      end},
     {"legacy http_pool migrates to all three",
      fun() ->
          application:set_env(beamai_core, http_pool,
                              #{max_connections_per_host => 50,
                                connection_timeout => 5000}),
          Pools = beamai_core_sup:resolve_pool_configs_for_test(),
          ?assertMatch(#{http_pool_short := #{max_connections_per_host := 50,
                                              connect_timeout := 5000},
                         http_pool_stream := #{connect_timeout := 5000},
                         http_pool_longpoll := #{connect_timeout := 5000}},
                       Pools)
      end},
     {"new http_pools wins over legacy",
      fun() ->
          application:set_env(beamai_core, http_pool, #{max_connections_per_host => 50}),
          application:set_env(beamai_core, http_pools,
                              #{http_pool_short => #{max_connections_per_host => 7}}),
          Pools = beamai_core_sup:resolve_pool_configs_for_test(),
          ?assertEqual(7, maps:get(max_connections_per_host,
                                   maps:get(http_pool_short, Pools))),
          ?assertEqual(50, maps:get(max_connections_per_host,
                                    maps:get(http_pool_stream, Pools)))
      end},
     {"unknown pool name in http_pools causes init error",
      fun() ->
          application:set_env(beamai_core, http_pools,
                              #{http_pool_xyz => #{}}),
          ?assertError({invalid_pool_names, [http_pool_xyz]},
                       beamai_core_sup:resolve_pool_configs_for_test())
      end}
    ].
```

Coverage target: ≥80% of `beamai_http_pool.erl`.

#### 6.1.2 `connection_acquire_return_test_/0`

Spin up a pool against `httpbin.local` (or skip if network unavailable — these tests should be marked `@tag network` and gated in CI), exercise:
- `get_connection/2,3` returns `{ok, Pid}` and Pid is monitored
- `return_connection/2` puts the connection back; a second `get_connection/2` may return the same Pid
- `connection_failed/2` evicts the connection
- Hitting `max_connections_per_host` returns `{error, pool_exhausted}`

#### 6.1.3 `idle_cleanup_test_/0`

Start pool with `idle_timeout => 100`. Acquire a connection (real or via stub), return it, wait 200 ms, trigger `cleanup_idle` (via `:sys.replace_state/2` or by waiting for the timer), assert it's evicted.

#### 6.1.4 `protocol_propagation_test_/0`

Inspect that `gun:open/3` is called with the configured `protocols`. Use `meck` to intercept `gun:open`:

```erlang
meck:new(gun, [passthrough]),
meck:expect(gun, open, fun(Host, Port, Opts) ->
    Self = self(),
    case proplists:get_value(test_capture, Opts, undefined) of
        undefined -> ok;
        Tag -> Self ! {gun_open_opts, Tag, Opts}
    end,
    meck:passthrough([Host, Port, Opts])
end),
```

Then assert that a pool with `protocols => [http2, http]` (only set in tests, not production default) propagates correctly. This test pins the "no longer hardcoded" guarantee.

### 6.2 Update LLM provider tests if needed

`grep -rn "beamai_http_pool" apps/beamai_llm/test/` returns no hits. The existing LLM tests use `beamai_llm_fake_backend` (which intercepts `request/5` *before* any pool is touched), so they are unaffected by the refactor.

Action: **No LLM test changes required.** Run the full LLM test suite after Phase 2 to confirm.

### 6.3 Integration test: provider → non-default pool routing

Add `apps/beamai_llm/test/beamai_http_pool_routing_tests.erl` (one small module, ~80 lines).

Caveat: `beamai_llm_fake_backend` intercepts at `Backend:request/5`, *before* any pool is touched — a meck on `beamai_http_pool:get_connection/2` never fires under the fake backend. Assert routing at the layer that actually runs: meck `beamai_http` (or inspect the opts the fake backend receives) and check the injected `pool` key. Also note `maybe_inject_pool/2` only injects when the Gun backend is active, so the test must either force `http_backend => beamai_http_gun` with a meck'd `beamai_http_gun`, or assert the *absence* of `pool` under non-Gun backends (which doubles as the Hackney regression test from 5.3).

```erlang
%% Asserts that a stream request goes to http_pool_stream and a chat
%% request goes to http_pool_short, by inspecting the `pool` key in the
%% opts that reach the HTTP layer (sketch — adapt per the caveat above).

routing_chat_uses_short_pool_test() ->
    meck:new(beamai_http_pool, [passthrough]),
    Counts = ets:new(routing_counts, [public]),
    ets:insert(Counts, {http_pool_short, 0}),
    ets:insert(Counts, {http_pool_stream, 0}),
    ets:insert(Counts, {http_pool_longpoll, 0}),
    meck:expect(beamai_http_pool, get_connection,
                fun(PoolName, _Url) ->
                    ets:update_counter(Counts, PoolName, 1),
                    meck:passthrough([PoolName, _Url])
                end),
    %% Run a fake chat
    Prev = beamai_http:get_backend(),
    beamai_llm_fake_backend:install(),
    try
        LLM = beamai_chat_completion:create(openai,
                  #{model => <<"gpt-4">>,
                    api_key => <<"sk-test">>}),
        beamai_chat_completion:chat(LLM, [#{role => user,
                                           content => <<"hi">>}]),
        ?assertEqual(1, ets:lookup(Counts, http_pool_short))
    after
        beamai_http:set_backend(Prev),
        meck:unload(beamai_http_pool),
        ets:delete(Counts)
    end.
```

Add a parallel test for stream → `http_pool_stream` (count `==1`) and for Zhipu async poll → `http_pool_longpoll` (use the existing Zhipu `get_async_result/2` mock).

### 6.4 Dialyzer

The 4-tuple return from `beamai_http_gun:request_async/5` will produce a Dialyzer warning if any `request_async` caller is missed. Run `rebar3 dialyzer` and fix before merging.

### 6.5 Coverage measurement

After tests land:

```bash
rebar3 eunit --app=beamai_core --module=beamai_http_pool
```

Expect ≥80% line coverage. If below, add tests for: `do_close_all/1`, `terminate/2`, the dead-process recovery paths in `handle_info({'DOWN', ...})`.

---

## 7. Risks & Mitigations

| # | Risk                                                                                                                                       | Likelihood | Impact | Mitigation                                                                                                                                                                                                                                                                                                                                  |
|---|--------------------------------------------------------------------------------------------------------------------------------------------|------------|--------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| 1 | Breaking all LLM traffic during migration.                                                                                                  | Medium     | High   | Phased rollout (Section 8). Defaults are bit-identical to current behavior (Section 5.2). A failed Phase 1 deployment leaves the system unchanged. Smoke test `rebar3 eunit` after each phase. Add a canary config in production that sets `http_pools` to override only `idle_timeout` for one pool — any bug surfaces in a non-critical path first. |
| 2 | Pool name typos silently fall through to wrong pool.                                                                                         | Medium     | Medium | `validate_pool_names/1` in supervisor (4.2.4) fails fast at startup for *sys.config* typos. Runtime typos (`pool => typo`) caught by `resolve_pool_name/1` in gun (4.4.1) returning `{error, {invalid_pool_name, _}}` — bubbles up as `{error, {connection_failed, {invalid_pool_name, typo}}}` so the LLM error layer marks it non-retryable.        |
| 3 | `idle_timeout` per pool leaks connections if misconfigured.                                                                                 | Low        | Medium | Defaults match current behavior (60 s for all three pools). Longer idle timeouts (e.g. 300 s for Zhipu async longpoll) are operator opt-in via `http_pools` — Appendix B documents the recommended values. The 30 s `cleanup_idle` interval in `beamai_http_pool.erl:78` stays as the upper bound on leak duration.                       |
| 4 | Gun 2.1 `protocols` `badmatch` regression re-surfaces if we enable HTTP/2.                                                                  | Medium     | High   | All three pools default to `protocols => [http]`. HTTP/2 requires explicit `protocols => [http2, http]` in sys.config — opt-in only. Add a regression test in `beamai_http_pool_tests.erl` that asserts the default config contains `[http]`. Add a docs note in `docs/HTTP.md` quoting the Gun 2.1 single-element match issue.                  |
| 5 | `beamai_http_gun:request_async/5` 3-tuple → 4-tuple return breaks unknown external callers.                                                | Low        | Medium | `request_async/5` is not in `beamai_http_behaviour` (not a behaviour callback), but it **is exported from `beamai_http_gun` as an advanced API**. Grep confirms only intra-module callers in this repo; downstream callers are broken by design. Mandatory CHANGELOG "Breaking changes" entry (Acceptance 7); Dialyzer will surface any missed in-repo caller.                                                                          |
| 6 | Three pools vs. one pool increases idle memory.                                                                                             | Low        | Low    | Each pool holds zero connections until first use. The gen_server state is small (a map per host). Negligible vs. Gun's own per-connection overhead.                                                                                                                                                                                       |
| 7 | `http_pool_short` becomes a load-bearing default; misuse creates an even worse single-pool bottleneck.                                       | Low        | Medium | Document the routing table in `docs/HTTP.md` (Section 9). Encourage callers who need a different pool to pass `pool => http_pool_stream` explicitly. Phase 4 (Section 8) adds per-request override so LLM providers can opt for `http_pool_stream` for long-context tool calls if needed.                                                  |
| 8 | Migration `connection_timeout` → `connect_timeout` rename is silent (no log) if user only sets the new key.                                  | Low        | Low    | The rename only matters for users using the *old* `http_pool` key — and they're already getting a warning. Users on the new `http_pools` always use `connect_timeout`.                                                                                                                                                                      |

---

## 8. Rollout Phases

Each phase is independently mergeable, can be reverted without breaking the others, and ends with the test suite green.

### Phase 1: Make pool config-driven (no behavior change)

**Goal:** Single pool, identical behavior, but now reads `connect_timeout` and `protocols` from config rather than hardcoded.

**Changes:**
- `beamai_http_pool.erl`:
  - Add `protocols` and `connect_timeout` to the config map.
  - Remove the hardcoded `protocols => [http]` (line 313) — read from config.
  - Rename `connection_timeout` → `connect_timeout` in `merge_config/1`.
  - Update `default_pool_config/0` (new private function) with the new shape.
  - **Do NOT** add the `name` parameter or pool-name API yet. The module still registers as `beamai_http_pool`.
- `beamai_core_sup.erl`:
  - Update `http_pool_spec/0` to pass through the new config keys (but still only spawn one pool).
- `beamai_core.app.src`:
  - No change to `registered`.
- `beamai_http_gun.erl`: **No change** — still calls `beamai_http_pool:get_connection/1` etc.
- `beamai_http.erl`: **No change.**
- LLM HTTP client/providers: **No change.**

**Validation:**
- `rebar3 eunit` — all tests still pass.
- Manual: `application:set_env(beamai_core, http_pool, #{protocols => [http, http2]})` then `beamai_http_pool:stats/0` shows `config => #{protocols := [http, http2], ...}`. Connection still works because Gun negotiates h2.
- (Actually for Phase 1 we don't enable http2 — we just *let it be configurable*. Default still `[http]`.)

**Rollback:** revert the two files. No consumer code changed.

### Phase 2: Split into 3 named pools with hardcoded routing table

**Goal:** Three pools running, hardcoded mapping `chat → short`, `stream → stream`, `async_poll → longpoll`.

**Changes:**
- `beamai_http_pool.erl`: full rewrite per Section 4.1 (name parameter, `start_link/2`, new API).
- `beamai_core_sup.erl`: `http_pool_specs/0` per Section 4.2.
- `beamai_core.app.src`: update `registered` list per Section 4.3.1.
- `beamai_http_gun.erl`: 4-tuple return, `resolve_pool_name/1` defaulting to `http_pool_short` per Section 4.4.
- `beamai_llm_http_client.erl`: add `select_pool/1` + backend-gated `maybe_inject_pool/2`, inject into HttpOpts per Section 4.6.
- `beamai_llm_provider_zhipu.erl`: use `beamai_llm_http_client:maybe_inject_pool(async_poll, ...)` in `do_get_request/3` per Section 4.7.1.

**Validation:**
- All existing tests pass.
- New `beamai_http_pool_tests.erl` (without coverage requirements met yet — just existence).
- `gun:stats/0` calls show three pools registered.
- Manual smoke: `beamai_chat_completion:chat(...)` still returns a result.

**Rollback:** revert. Phase 1's single-pool code is in git history; can fast-forward to that commit if disaster strikes.

### Phase 3: Allow sys.config overrides per pool

**Goal:** Operators can tune each pool independently.

**Changes:**
- `beamai_core_sup.erl`: full `resolve_pool_configs/0` per Section 4.2.3 — read `http_pools` env, merge with defaults.
- `beamai_http_pool.erl`: no change (already config-driven after Phase 1).
- Docs: add `http_pools` to `docs/API_REFERENCE.md` configuration section.

**Validation:**
- New `config_merging_test_/0` cases from Section 6.1.1 pass.
- Dialyzer clean.

**Rollback:** revert `resolve_pool_configs/0`. Pools continue starting with default configs.

### Phase 4: Allow per-request pool selection from LLM layer

**Goal:** LLM providers can opt for a non-default pool via opts.

**Changes:**
- `beamai_llm_http_client.erl`: stop forcing `select_pool(chat)` — only inject if `Opts` doesn't already carry `pool`. Or expose a config knob per provider that picks the pool.
- Optional: add `pool` to each provider's `default_config/0` so users can override per-provider (e.g., `deepseek => #{pool => http_pool_stream}` for heavy reasoning traffic).

**Validation:**
- Add an integration test that sets `pool => http_pool_stream` in `Opts` and confirms the stream pool is used.
- Document the per-request opt in `docs/API_REFERENCE.md`.

**Rollback:** revert opt-passthrough; `select_pool/1` returns the default again.

---

## 9. Acceptance Criteria

A merge into `main` requires *all* of the following:

1. **All existing tests pass without modification.**
   ```bash
   rebar3 eunit
   ```
   No test file in `apps/beamai_core/test/` or `apps/beamai_llm/test/` is edited to make a test pass (modulo the new tests added by this plan).

2. **New pool tests cover ≥80% of `beamai_http_pool.erl`.**
   ```bash
   rebar3 eunit --app=beamai_core --module=beamai_http_pool
   rebar3 cover --app=beamai_core
   ```
   Show coverage report in the PR description.

3. **Existing sys.config using `http_pool` continues to work.**
   Verified by:
   - The migration test in Section 6.1.1 (`legacy http_pool migrates to all three`).
   - Manual: take the existing `sys.config` block from `README.md:288` (with `max_connections => 100`, `connection_timeout => 30000`), `rebar3 shell`, confirm `whereis(http_pool_short)` is alive and `beamai_http_pool:stats(http_pool_short)` shows `max_connections_per_host => 100` and `connect_timeout => 30000`.
   - A deprecation warning is logged once at startup; assert this in a test that captures `logger` output.

4. **`protocols` is no longer hardcoded anywhere.**
   ```bash
   grep -rn "protocols\s*=>" apps/beamai_core/src/ apps/beamai_llm/src/
   ```
   The only matches should be in `beamai_core_sup:default_pool_config/1` (defaults) and `beamai_http_pool:merge_config/2` (fallback) — both of which can be overridden by sys.config. No matches in `beamai_http_pool:create_new_connection/5`.

5. **Documented.** Either `docs/HTTP.md` (preferred — new file) or a new section in `docs/API_REFERENCE.md` titled "HTTP connection pools" must cover:
   - The three pools and their defaults.
   - Routing table (chat/stream/async_poll → pool).
   - The `protocols` opt-in and the Gun 2.1 caveat.
   - Legacy `http_pool` deprecation.
   - Tuning guidance for `max_connections_per_host`, `idle_timeout`, `connect_timeout` per pool.

6. **Dialyzer clean.**
   ```bash
   rebar3 dialyzer
   ```

7. **`beamai_http_gun:request_async/5` 4-tuple change documented in CHANGELOG.**
   Even though no in-repo callers exist outside the module, downstream users may have wrapped it. Note the change in `CHANGELOG.md` under "Breaking changes".

8. **No leftover `beamai_http_pool` (singular) registrations.**
   ```bash
   erl -pa _build/default/lib/*/ebin -noshell -eval '
       beamai_core:start(),
       Registered = registered(),
       io:format("~p~n", [Registered]),
       halt().
   ' -s init stop
   ```
   Expected: `[beamai_core_sup, http_pool_short, http_pool_stream, http_pool_longpoll]`. Anything containing `beamai_http_pool` is a regression.

---

## 10. Open Questions

1. **Pool name as `atom()` or `binary()`?**
   The new design uses atoms (`http_pool_short`, etc.) because:
   - They align with how `gen_server` registration and `whereis/1` work.
   - They match the existing `pool => atom()` opt type in `beamai_http_behaviour:opts()` (line 41).
   - They're easier to type in `sys.config` and pattern-match in tests.
   
   But all other long-lived names in the project use atoms too (`beamai_core_sup`, `beamai_llm_error`). Atoms win.
   
   *Decision: atom. Resolve at plan approval.*

2. **Should providers expose pool selection as a config, or hardcode in this refactor?**
   The plan currently hardcodes `chat → short`, `stream → stream`, `async_poll → longpoll` in `beamai_llm_http_client:select_pool/1`. An alternative is to make it per-provider-config:
   ```erlang
   llm_client:create(openai, #{model => ..., pool => http_pool_stream})
   ```
   - **Pro:** Operators can route heavy reasoning models to the stream pool.
   - **Con:** Every provider needs to thread `pool` through `Config`, more API surface.
   - **Recommendation:** Phase 4 (Section 8) covers this — out of scope for the first merge. Phase 2–3 hardcode.

3. **Do we need a "default" pool alias for callers that don't care?**
   - `beamai_http_gun:resolve_pool_name/1` already defaults to `http_pool_short` when `pool` is unset. So callers that don't pass `pool` get sensible behavior automatically.
   - But the name `http_pool_short` leaks into LLM error messages (`{invalid_pool_name, typo}`). Some operators might prefer a generic `default` alias that maps to `http_pool_short` internally.
   - **Recommendation:** Skip for now. If operators ask, add `http_pool_default` as an alias in `validate_pool_names/1` that maps to the same registered process. Add to Phase 4.

4. **Should `http_pool_longpoll` have a per-connection `keepalive` strategy to avoid re-handshakes during multi-minute polls?**
   - Zhipu's `async-result/<task_id>` returns within seconds once the task finishes, but can take 30–60 s if not. With `idle_timeout=300s`, a held connection survives a poll cycle, but the *client* (Gun) doesn't know the server is still computing and may time out the response.
   - Out of scope for this refactor. File as a follow-up: "Investigate `gun:await_up/2` reconnect for longpoll pool."

5. **Should the `?CLEANUP_INTERVAL` (30 s) become per-pool?**
   - Currently a single hardcoded `?CLEANUP_INTERVAL` in `beamai_http_pool.erl:78`.
   - With three pools, each pool spawns its own timer. That's fine — cleanup work is bounded by per-pool conn count.
   - No change needed. Document as "interval is per-pool, so adding more pools multiplies timer overhead by N".

---

## Appendix A: File-touch summary

| File                                                                 | Lines (approx) | Nature of change                                |
|----------------------------------------------------------------------|----------------|-------------------------------------------------|
| `apps/beamai_core/src/http/beamai_http_pool.erl`                     | 1–481          | Major rewrite (singleton → named pool API)      |
| `apps/beamai_core/src/beamai_core_sup.erl`                           | 1–84           | Replace `http_pool_spec/0` with `http_pool_specs/0`, add `resolve_pool_configs/0`, `legacy_to_pools/2`, `validate_pool_names/1` |
| `apps/beamai_core/src/beamai_core.app.src`                           | 4, 40          | Update `registered` list                        |
| `apps/beamai_core/src/http/beamai_http_gun.erl`                      | 49, 69, 86, 110 | Add `resolve_pool_name/1`, plumb pool name through request/5, request_meta/5, stream_request/6, request_async/5 |
| `apps/beamai_core/src/http/beamai_http.erl`                          | n/a            | No functional change (comment update only)      |
| `apps/beamai_llm/src/beamai_llm_http_client.erl`                     | 142, 69, 178, 91 | Add `select_pool/1`, inject pool into HttpOpts |
| `apps/beamai_llm/src/providers/beamai_llm_provider_zhipu.erl`        | 227            | Add `pool => http_pool_longpoll` in `do_get_request/3` |
| `apps/beamai_core/test/beamai_http_pool_tests.erl`                   | new file       | ≥80% coverage of `beamai_http_pool.erl`         |
| `apps/beamai_llm/test/beamai_http_pool_routing_tests.erl`            | new file       | Provider → non-default pool routing             |
| `docs/API_REFERENCE.md` or `docs/HTTP.md`                            | new section    | Document pools, defaults, routing, deprecation  |
| `CHANGELOG.md`                                                       | new entry      | Breaking changes (`request_async/5` 3→4 tuple, `http_pool` deprecated) |

Files explicitly NOT touched:
- `apps/beamai_core/src/http/beamai_http_hackney.erl` (Hackney path unchanged)
- All other provider modules (`openai`, `anthropic`, `deepseek`, `dashscope`, `ollama`)
- `apps/beamai_core/src/beamai_core_app.erl` (app start callback unchanged)
- `apps/beamai_core/src/behaviours/beamai_http_behaviour.erl` (behaviour callbacks unchanged; `pool => atom()` opt was already in the type)

---

## Appendix B: Migration cookbook

For a downstream user upgrading from current `beamai_*` to this refactor:

### Before (current `sys.config`)
```erlang
{beamai_core, [
    {http_backend, beamai_http_gun},
    {http_pool, #{
        max_connections_per_host => 50,
        connection_timeout => 30000,
        idle_timeout => 60000
    }}
]}.
```

### After, drop-in replacement (preserves behavior, splits across 3 pools)
```erlang
{beamai_core, [
    {http_backend, beamai_http_gun},
    {http_pools, #{
        http_pool_short => #{
            max_connections_per_host => 50, connect_timeout => 30000,
            idle_timeout => 60000, protocols => [http]
        },
        http_pool_stream => #{
            max_connections_per_host => 50, connect_timeout => 30000,
            idle_timeout => 60000, protocols => [http]
        },
        http_pool_longpoll => #{
            max_connections_per_host => 50, connect_timeout => 30000,
            idle_timeout => 60000, protocols => [http]
        }
    }}
]}.
```

### After, recommended for production (purpose-tuned)
```erlang
{beamai_core, [
    {http_backend, beamai_http_gun},
    {http_pools, #{
        http_pool_short => #{
            max_connections_per_host => 30, connect_timeout => 10000,
            idle_timeout => 30000, protocols => [http]
        },
        http_pool_stream => #{
            max_connections_per_host => 50, connect_timeout => 30000,
            idle_timeout => 120000, protocols => [http]
        },
        http_pool_longpoll => #{
            max_connections_per_host => 10, connect_timeout => 30000,
            idle_timeout => 300000, protocols => [http]
        }
    }}
]}.
```

### Operator opt-in to HTTP/2 for chat pool
```erlang
{http_pools, #{
    http_pool_short => #{
        max_connections_per_host => 30,
        connect_timeout => 10000,
        idle_timeout => 30000,
        protocols => [http2, http]      %% Gun 2.1 single-element match — TEST FIRST
    },
    ...
}}
```

Add a `logger:info` line in the operator's deploy script that captures `beamai_http_pool:stats(http_pool_short)` to verify `protocols => [http2, http]` is actually applied.