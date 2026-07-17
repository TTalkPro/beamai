# HTTP Connection Pools

> 中文版本: [HTTP.md](HTTP.md)

BeamAI's Gun backend uses three purpose-shaped connection pool instances (all named instances of `beamai_http_pool`, supervised one_for_one by `beamai_core_sup` — a crash in one pool never affects the other two). The split gives each traffic shape its own connection budget: an SSE stream held for 30+ seconds or an async poll pinned for minutes no longer starves synchronous chat requests.

## The Three Pools and the Routing Table

| Pool | Traffic | Routed from |
|------|---------|-------------|
| `http_pool_short` | Synchronous chat / tool-call requests (1–5 s) | `beamai_llm_http_client:request/5`; requests without a `pool` opt default here |
| `http_pool_stream` | SSE streaming responses (held 30 s+) | `beamai_llm_http_client:stream_request/5,6` |
| `http_pool_longpoll` | Async task status polling (minutes, e.g. Zhipu `async-result`) | Zhipu provider's async result queries |

Routing happens automatically in the LLM HTTP client layer (`beamai_llm_http_client:maybe_inject_pool/2`); providers don't need to care. Every provider's chat/stream lands in the right pool.

## Defaults

All three pools ship with identical defaults, equal to the pre-split single pool — with no configuration, each traffic class behaves exactly as before (the only difference being that classes no longer contend with each other):

| Key | Default | Notes |
|-----|---------|-------|
| `max_connections_per_host` | 10 | Per-host cap; exceeding it returns `{error, pool_exhausted}` |
| `connect_timeout` | 30000 | Connect timeout (ms), maps to `gun:open`'s option of the same name |
| `idle_timeout` | 60000 | Idle-connection eviction threshold (ms); the cleanup timer sweeps every 30 s |
| `protocols` | `[http]` | Protocol list passed to `gun:open` — see the HTTP/2 caveat below |

Note: after the split, the **total** per-host cap is the sum across pools (3×10 by default), higher than the old single pool's 10 — this is the intended effect of eliminating cross-class contention.

## Per-Pool Configuration (`http_pools`)

```erlang
{beamai_core, [
    {http_backend, beamai_http_gun},
    {http_pools, #{
        http_pool_stream   => #{max_connections_per_host => 20,
                                idle_timeout => 120000},
        http_pool_longpoll => #{idle_timeout => 300000}
    }}
]}.
```

- Configure only the pools and keys you want to override; everything else uses defaults (mix-and-match).
- Unknown pool names fail at startup with `{invalid_pool_names, [...]}` — no silent phantom pools nothing routes to.
- Verify the effective config at runtime: `beamai_http_pool:stats(http_pool_stream)` returns a map with `config` and `name`.

### Recommended Production Values

| Pool | `max_connections_per_host` | `connect_timeout` | `idle_timeout` | Rationale |
|------|---------------------------|-------------------|----------------|-----------|
| `http_pool_short` | 30 | 10000 | 30000 | Short requests turn over quickly; a tighter connect timeout surfaces network issues sooner |
| `http_pool_stream` | 50 | 30000 | 120000 | Streams hold connections long; needs a bigger budget and a longer idle window |
| `http_pool_longpoll` | 10 | 30000 | 300000 | Polling clients are inherently serial — a small budget creates back-pressure; long idle keeps connections alive across a poll cycle |

## Legacy `http_pool` Key (Deprecated, Still Supported)

```erlang
%% Old style — still works, logs a deprecation warning once at startup
{beamai_core, [
    {http_pool, #{max_connections_per_host => 50,
                  connection_timeout => 30000}}
]}.
```

- Its value is applied **uniformly to all three pools**, preserving the old single-pool semantics.
- The legacy key name `connection_timeout` is normalized to `connect_timeout` (when both are set, the legacy key wins, so legacy configs behave unchanged).
- When both `http_pool` and `http_pools` are set, `http_pools` overrides the legacy value per pool.

## HTTP/2 (`protocols`) and the Gun 2.1 Caveat

`protocols` defaults to `[http]` (HTTP/1.1). Reason: **Gun 2.1's plain-TCP connect path does a single-element match on protocols** (`[Protocol] = maps:get(protocols, Opts, [http])`), so configuring `[http2, http]` crashes every `http://` connection with `{badmatch, [http2, http]}`; the TLS (`https://`) path negotiates fine.

For HTTP/2-capable providers (OpenAI, Anthropic, etc. — all HTTPS) you can opt in per pool:

```erlang
{http_pools, #{
    http_pool_short => #{protocols => [http2, http]}   %% test in your target environment first
}}.
```

Invalid protocol names (anything other than `http`/`http2`) fail at pool startup with `{invalid_protocol, Name}`.

## Overriding the Default Routing (Advanced)

Two explicit override levels sit above the automatic routing. Precedence: request Opts > provider Config > default routing table.

**Per provider** — set `pool` in the provider Config and all of that provider's requests (chat/stream, including Zhipu's async polling) are rerouted, e.g. to put a heavy reasoning model's traffic into the stream pool:

```erlang
LLM = beamai_chat_completion:create(deepseek,
          #{api_key => ..., model => <<"deepseek-reasoner">>,
            pool => http_pool_stream}).
```

**Per request** — when calling `beamai_llm_http_client` or `beamai_http` directly, set it in Opts:

```erlang
beamai_http:request(post, Url, Headers, Body, #{pool => http_pool_stream}).
```

Rules:

- When unset, the Gun backend applies the default routing table per request shape (other backends get no injection); an invalid pool name returns `{error, {invalid_pool_name, Name}}`.
- **An explicitly set `pool` is passed through as-is under any backend** (setting it explicitly means you know which backend runs): the Gun backend accepts only the three pool names; **the Hackney backend interprets `pool` as a hackney pool name** — when switching backends, re-check any `pool` values in provider Configs.
- The automatic-injection gate lives in `beamai_llm_http_client:maybe_inject_pool/3`; reuse that helper if you call `beamai_http` directly and the backend is switchable.

## Troubleshooting Quick Reference

| Symptom | Meaning / Action |
|---------|------------------|
| `{error, pool_exhausted}` | That pool hit `max_connections_per_host` for the host; check the traffic went to the right pool, or raise the pool's budget |
| `{error, {pool_not_started, Name}}` | beamai_core not started, or Gun backend disabled (`should_start_http_pool`) |
| `{error, {invalid_pool_name, Name}}` | Typo in the request opts' `pool` |
| Startup failure `{invalid_pool_names, [...]}` | Unknown pool name in `http_pools` |
| Config seemingly not applied | Inspect the `config` echo in `beamai_http_pool:stats(PoolName)` |
