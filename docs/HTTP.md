# HTTP 连接池

> English version: [HTTP_EN.md](HTTP_EN.md)

BeamAI 的 Gun 后端使用三个按用途划分的连接池实例（均为 `beamai_http_pool` 的命名实例，由 `beamai_core_sup` 以 one_for_one 策略管理——一个池崩溃不影响其他两个）。拆池的目的是让不同形态的流量各自持有连接预算：一条占用 30 秒以上的 SSE 流、一个持连数分钟的异步轮询，不再挤占同步 chat 请求的连接。

## 三个池与路由表

| 池名 | 承载流量 | 路由来源 |
|------|----------|----------|
| `http_pool_short` | 同步 chat / 工具调用等短请求（1–5 秒） | `beamai_llm_http_client:request/5`；未指定 `pool` 的请求默认落此池 |
| `http_pool_stream` | SSE 流式响应（30 秒以上持连） | `beamai_llm_http_client:stream_request/5,6` |
| `http_pool_longpoll` | 异步任务状态轮询（可达数分钟，如智谱 `async-result`） | `beamai_llm_provider_zhipu` 的异步结果查询 |

路由在 LLM HTTP 客户端层自动完成（`beamai_llm_http_client:maybe_inject_pool/2`），provider 无需关心。所有 provider 的 chat/stream 自动进对应池。

## 默认配置

三个池的默认值完全一致，等于拆分前单池的默认值——不做任何配置时，每类流量的行为与旧版本相同（区别仅是各类流量不再互相争用）：

| 配置键 | 默认值 | 说明 |
|--------|--------|------|
| `max_connections_per_host` | 10 | 每主机最大连接数，超出返回 `{error, pool_exhausted}` |
| `connect_timeout` | 30000 | 建连超时（ms），对应 `gun:open` 的同名选项 |
| `idle_timeout` | 60000 | 空闲连接回收阈值（ms），清理定时器每 30 秒扫一次 |
| `protocols` | `[http]` | 传给 `gun:open` 的协议列表，见下方 HTTP/2 注意事项 |

注意：拆池后每主机的**总**连接上限是三池之和（默认 3×10），高于旧版单池的 10——这是消除跨类争用的设计意图。

## 按池配置（`http_pools`）

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

- 只需配置想覆盖的池和键，其余用默认值（mix-and-match）。
- 未知池名启动即报 `{invalid_pool_names, [...]}`，应用启动失败——不会静默生成没人路由到的幽灵池。
- 生效配置可在运行时核对：`beamai_http_pool:stats(http_pool_stream)` 返回的 map 里带 `config` 与 `name`。

### 生产环境推荐值

| 池 | `max_connections_per_host` | `connect_timeout` | `idle_timeout` | 理由 |
|----|---------------------------|-------------------|----------------|------|
| `http_pool_short` | 30 | 10000 | 30000 | 短请求周转快，缩短建连超时尽早暴露网络问题 |
| `http_pool_stream` | 50 | 30000 | 120000 | 流式持连久，需要更大预算和更长空闲窗口 |
| `http_pool_longpoll` | 10 | 30000 | 300000 | 轮询客户端本质串行，小预算即可形成背压；长 idle 让连接活过整个轮询周期 |

## 遗留 `http_pool` 键（已废弃，仍兼容）

```erlang
%% 旧写法——仍然工作，启动时打一次废弃警告
{beamai_core, [
    {http_pool, #{max_connections_per_host => 50,
                  connection_timeout => 30000}}
]}.
```

- 其值**统一应用到三个池**，保持旧的单池语义。
- 旧键名 `connection_timeout` 自动归一化为 `connect_timeout`（两者同时设置时旧键优先，保证遗留配置行为不变）。
- `http_pool` 与 `http_pools` 同时设置时，`http_pools` 按池覆盖遗留值。

## HTTP/2（`protocols`）与 Gun 2.1 注意事项

`protocols` 默认 `[http]`（HTTP/1.1）。原因：**Gun 2.1 的纯 TCP 连接路径对 protocols 做单元素匹配**（`[Protocol] = maps:get(protocols, Opts, [http])`），配置 `[http2, http]` 时所有 `http://` 连接会以 `{badmatch, [http2, http]}` 崩溃；TLS（`https://`）路径正常协商。

面向 HTTP/2 provider（OpenAI、Anthropic 等，全部走 https）可以按池启用：

```erlang
{http_pools, #{
    http_pool_short => #{protocols => [http2, http]}   %% 先在目标环境验证
}}.
```

非法协议名（`http`/`http2` 之外）在池启动时即报 `{invalid_protocol, Name}`。

## 按请求指定池（高级）

直接调用 `beamai_http` 的代码可经 Opts 指定池：

```erlang
beamai_http:request(post, Url, Headers, Body, #{pool => http_pool_stream}).
```

- 未指定时 Gun 后端默认 `http_pool_short`；非法池名返回 `{error, {invalid_pool_name, Name}}`。
- **Hackney 后端把 `pool` 当作 hackney 池名**，勿向其传 Gun 池名。LLM 层的自动注入已由 `beamai_llm_http_client:maybe_inject_pool/2` 门控（仅 Gun 后端注入）；自己直调 `beamai_http` 且后端可切换时，请复用该函数。

## 排查速查

| 现象 | 含义 / 处理 |
|------|-------------|
| `{error, pool_exhausted}` | 该池对该主机连接已达 `max_connections_per_host`；确认流量是否进错池，或调大该池预算 |
| `{error, {pool_not_started, Name}}` | beamai_core 未启动，或 Gun 后端未启用（`should_start_http_pool`） |
| `{error, {invalid_pool_name, Name}}` | 请求 Opts 里的 `pool` 拼错 |
| 启动失败 `{invalid_pool_names, [...]}` | `http_pools` 里有未知池名 |
| 配置疑似未生效 | `beamai_http_pool:stats(池名)` 查看 `config` 回显 |
