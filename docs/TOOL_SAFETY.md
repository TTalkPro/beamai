# Tool 安全扩展点

> English version: [TOOL_SAFETY_EN.md](TOOL_SAFETY_EN.md)

BeamAI 工具执行已有进程隔离、超时和批级隔离。本页描述工具级安全声明与执行策略。

## 工具声明示例

```erlang
Tool = #{
    name => <<"search_files">>,
    handler => fun(Args, Ctx) -> search_files(Args, Ctx) end,
    parameters => #{
        query => #{type => string, required => true}
    },
    strict_args => true,
    permissions => #{
        filesystem => true,
        network => false,
        process => false,
        env => false
    },
    result_redaction => [
        {<<"sk-[A-Za-z0-9]+">>, <<"[REDACTED]">>}
    ],
    max_result_bytes => 262144
}.
```

## 字段语义

| 字段 | 默认 | 语义 |
|---|---|---|
| `strict_args` | `false` | 执行前用 `parameters` 生成的 JSON Schema 校验参数；失败返回 `invalid_tool_args` |
| `permissions` | 未声明 | 文件、网络、进程、环境访问权限的元数据约定；`beamai_tool:validate/1` 校验其形状 |
| `result_redaction` | `[]` | 工具结果编码成文本后按顺序执行正则替换 |
| `max_result_bytes` | 无限制 | 最终编码文本的硬上限；超过时返回/抛出 `tool_result_too_large` |

这些策略都是 opt-in。未配置安全字段的工具保持既有行为，不做强制迁移。

## 执行顺序

`beamai_tool_executor` 的最内层 terminal 在 handler 成功返回后执行：

1. 参数校验：`strict_args => true` 时在 handler 前执行。
2. handler 执行：仍走原有子进程、timeout、retry 机制。
3. 结果 redaction：只对配置了 `result_redaction` 的工具生效。
4. 结果大小检查：只对配置了 `max_result_bytes` 的工具生效。
5. around_tool filter：外层 filter 仍能继续改写 terminal 返回的结果。

## 与审批 / 流式脱敏的关系

- `sensitive => true` 交给 `beamai_filters:approval_filter/1,2` 做执行前审批。
- `token_redact_filter/2` 处理 streaming token。
- `result_redaction` 处理工具结果整体回灌模型前的文本。

三者关注点不同，可以组合使用。
