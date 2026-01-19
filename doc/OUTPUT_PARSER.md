# Output Parser 指南

本文档详细介绍 BeamAI Framework 的 Output Parser 系统，用于解析和处理 LLM 的结构化输出。

## 目录

- [概述](#概述)
- [核心模块](#核心模块)
- [JSON Parser](#json-parser)
- [重试机制](#重试机制)
- [格式指令](#格式指令)
- [API 参考](#api-参考)
- [使用示例](#使用示例)
- [最佳实践](#最佳实践)
- [扩展开发](#扩展开发)

---

## 概述

Output Parser 是 BeamAI Framework 中用于解析 LLM 输出的核心组件，类似于 LangChain 的 Output Parser 功能。它提供了容错解析、格式指令生成和自动重试等能力。

### 主要特性

- **容错解析**: 自动修复 LLM 输出中的常见格式错误
- **多格式支持**: 支持 JSON、XML、CSV 等格式（JSON 已完整实现）
- **智能提取**: 从 markdown 代码块或混合文本中提取结构化数据
- **重试机制**: 解析失败时支持自动重试和退避策略
- **格式指令**: 自动生成 LLM 提示词中的格式要求

### 模块结构

```
apps/beamai_llm/src/parser/
├── beamai_output_parser.erl       # 统一 API 门面（177 行）
├── beamai_parser_json.erl         # JSON 解析器核心（347 行）
├── beamai_parser_retry.erl        # 重试机制（123 行）
└── beamai_parser_instructions.erl # 格式指令生成器（163 行）
```

---

## 核心模块

### beamai_output_parser

统一的 Parser API 门面，提供以下功能：

```erlang
%% Parser 创建
-export([new/2, json/0, json/1, xml/0, csv/0]).

%% 解析操作
-export([parse/2, parse/3]).
-export([parse_with_retry/3, parse_with_retry/4]).

%% 格式指令
-export([get_instructions/1, get_instructions/2]).

%% 错误处理
-export([is_retryable_error/1]).
```

### 类型定义

```erlang
-type format() :: json | xml | csv | raw.

-type parser() :: #{
    type := format(),
    options := map()
}.

-type parse_result() :: {ok, term()} | {error, parse_error()}.

-type parse_error() ::
    {invalid_json, binary()} |
    {invalid_xml, binary()} |
    {invalid_csv, binary()} |
    {extract_failed, binary()} |
    {max_retries_exceeded, [parse_error()]}.
```

---

## JSON Parser

JSON Parser 是 Output Parser 中最完善的实现，提供强大的容错解析能力。

### 创建 JSON Parser

```erlang
%% 使用默认选项
Parser = beamai_output_parser:json().

%% 自定义选项
Parser = beamai_output_parser:json(#{
    extract_codeblock => true,   %% 从代码块提取 JSON
    repair_common => true,       %% 修复常见格式错误
    strip_markdown => true,      %% 移除 markdown 格式
    schema => JsonSchema         %% JSON Schema 验证（可选）
}).
```

### 解析流程

```
原始文本
    │
    ▼
┌─────────────────────────────────────────┐
│           预处理阶段                      │
│  1. 提取 json 代码块                      │
│  2. 查找 JSON 边界（{ } 或 [ ]）          │
│  3. 移除 markdown 格式                    │
│  4. 去除首尾空白                          │
└─────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────┐
│           直接解析尝试                    │
│  使用 jsx:decode 解析 JSON               │
└─────────────────────────────────────────┘
    │
    ├── 成功 → 返回 {ok, Result}
    │
    ▼ 失败
┌─────────────────────────────────────────┐
│           修复阶段                        │
│  1. 移除尾随逗号                          │
│  2. 移除 JavaScript 注释                  │
│  3. 为未引用的键添加引号                   │
│  4. 移除末尾分号                          │
└─────────────────────────────────────────┘
    │
    ▼
┌─────────────────────────────────────────┐
│           再次解析                        │
└─────────────────────────────────────────┘
    │
    ├── 成功 → 返回 {ok, Result}
    │
    ▼ 失败
返回 {error, {invalid_json, Text}}
```

### JSON 提取策略

Parser 使用多层策略从文本中提取 JSON：

#### 1. 代码块提取

优先查找 markdown json 代码块：

```markdown
这是一些说明文字...

```json
{"name": "example", "value": 42}
```

更多文字...
```

```erlang
%% 匹配 ```json ... ``` 格式
case re:run(Text, "```json\\s*([\\s\\S]+?)```", [caseless, {capture, all, binary}]) of
    {match, [_, Content]} -> {ok, Content};
    nomatch -> try_generic_codeblock(Text)
end.
```

#### 2. JSON 边界查找

如果没有代码块，查找完整的 `{ ... }` 或 `[ ... ]` 结构：

```erlang
%% 确定优先级：哪个字符先出现
ObjStart = find_position(Text, <<"{">>),
ArrStart = find_position(Text, <<"[">>),

%% 根据先出现的进行解析
if
    ArrStart < ObjStart -> find_array_boundaries(Text);
    ObjStart < ArrStart -> find_object_boundaries(Text);
    true -> {error, no_json_found}
end.
```

#### 3. 括号匹配算法

支持嵌套结构的正确解析：

```erlang
%% 查找匹配的括号，跳过字符串内容
find_matching_brace(Text, Pos, Depth, Open, Close) ->
    case binary:at(Text, Pos) of
        C when C =:= Open ->
            find_matching_brace(Text, Pos + 1, Depth + 1, Open, Close);
        C when C =:= Close ->
            case Depth of
                1 -> {ok, Pos};  %% 找到匹配
                _ -> find_matching_brace(Text, Pos + 1, Depth - 1, Open, Close)
            end;
        $" ->
            %% 跳过字符串内容
            {ok, NextPos} = skip_string(Text, Pos + 1),
            find_matching_brace(Text, NextPos, Depth, Open, Close);
        _ ->
            find_matching_brace(Text, Pos + 1, Depth, Open, Close)
    end.
```

### JSON 修复功能

Parser 可以自动修复 LLM 输出中的常见格式错误：

| 错误类型 | 示例 | 修复后 |
|----------|------|--------|
| 尾随逗号 | `{"a": 1,}` | `{"a": 1}` |
| 单行注释 | `{"a": 1} // comment` | `{"a": 1}` |
| 多行注释 | `{"a": /* note */ 1}` | `{"a": 1}` |
| 未引用键 | `{key: "value"}` | `{"key": "value"}` |
| 末尾分号 | `{"a": 1};` | `{"a": 1}` |

```erlang
%% 修复函数
repair_json(Text) ->
    T1 = remove_trailing_commas(Text),      %% 移除尾随逗号
    T2 = remove_js_comments(T1),            %% 移除 JS 注释
    T3 = quote_unquoted_keys(T2),           %% 引用键名
    T4 = remove_trailing_semicolon(T3),     %% 移除末尾分号
    T4.

%% 移除尾随逗号
remove_trailing_commas(Text) ->
    re:replace(Text, ",(\\s*[\\]}])", "\\1", [{return, binary}, global]).

%% 移除 JavaScript 注释
remove_js_comments(Text) ->
    T1 = re:replace(Text, "//.*?(?:\\n|$)", "", [{return, binary}, global]),
    re:replace(T1, "/\\*[\\s\\S]*?\\*/", "", [{return, binary}, global]).
```

---

## 重试机制

当解析失败时，可以使用重试机制自动重试。

### 基本用法

```erlang
%% 最多重试 3 次
{ok, Result} = beamai_output_parser:parse_with_retry(Parser, Text, 3).

%% 带选项的重试
{ok, Result} = beamai_output_parser:parse_with_retry(Parser, Text, 3, #{
    on_retry => fun(Error, Attempt) ->
        io:format("重试 #~w: ~p~n", [Attempt, Error])
    end,
    backoff => fun(Attempt) -> Attempt * 200 end,  %% 自定义退避
    max_delay => 10000  %% 最大延迟 10 秒
}).
```

### 退避策略

支持三种内置退避策略：

```erlang
%% 线性退避: attempt * 100ms
%% 尝试 1: 100ms, 尝试 2: 200ms, 尝试 3: 300ms
linear_backoff(Attempt) -> Attempt * 100.

%% 指数退避: 100ms * 2^(attempt-1)，最大 5 秒
%% 尝试 1: 100ms, 尝试 2: 200ms, 尝试 3: 400ms, 尝试 4: 800ms
exponential_backoff(Attempt) ->
    BaseDelay = 100 bsl (Attempt - 1),
    min(BaseDelay, 5000).

%% 斐波那契退避
%% 尝试 1: 100ms, 尝试 2: 100ms, 尝试 3: 200ms, 尝试 4: 300ms, 尝试 5: 500ms
fibonacci_backoff(Attempt) -> fibonacci(Attempt) * 100.
```

### 使用退避策略

```erlang
%% 使用指定的退避策略
{ok, Result} = beamai_parser_retry:parse_with_backoff(
    Parser,
    Text,
    5,              %% 最大重试次数
    exponential,    %% 退避策略: linear | exponential | fibonacci
    #{}             %% 其他选项
).
```

### 可重试错误

```erlang
%% 判断错误是否可重试
is_retryable_error({invalid_json, _}) -> true;     %% JSON 格式错误
is_retryable_error({extract_failed, _}) -> true;   %% 提取失败
is_retryable_error({invalid_xml, _}) -> true;      %% XML 格式错误
is_retryable_error({invalid_csv, _}) -> true;      %% CSV 格式错误
is_retryable_error({not_implemented, _}) -> false; %% 功能未实现
is_retryable_error({max_retries_exceeded, _}) -> false. %% 已达最大重试
```

---

## 格式指令

格式指令用于在 LLM 提示词中说明输出格式要求。

### 获取指令

```erlang
%% 获取默认 JSON 格式指令
Instructions = beamai_output_parser:get_instructions(json).

%% 带 Schema 的指令
Instructions = beamai_output_parser:get_instructions(json, #{
    schema => #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"name">> => #{<<"type">> => <<"string">>, <<"description">> => <<"用户名">>},
            <<"age">> => #{<<"type">> => <<"integer">>, <<"description">> => <<"年龄">>}
        },
        <<"required">> => [<<"name">>]
    },
    examples => [
        #{<<"name">> => <<"Alice">>, <<"age">> => 25}
    ]
}).
```

### 生成的指令示例

```
You must respond with valid JSON format. Your entire response should be a single JSON value (object or array).

Do NOT include any explanatory text before or after the JSON. Do NOT wrap the JSON in markdown code blocks unless explicitly requested.

Expected JSON structure:
  *name: string - 用户名
  age: integer - 年龄

Examples:
{"name":"Alice","age":25}

Requirements:
- Output must be valid, parseable JSON
- Use double quotes for strings and keys
- Do not include trailing commas
- Do not use JavaScript-style comments
- Escape special characters properly
- Ensure all brackets and braces are properly matched
```

### 在提示词中使用

```erlang
%% 构建带格式指令的系统提示词
FormatInstructions = beamai_output_parser:get_instructions(json, #{
    schema => UserSchema
}),

SystemPrompt = <<"You are a helpful assistant.\n\n",
                 "OUTPUT FORMAT:\n",
                 FormatInstructions/binary>>.
```

---

## API 参考

### beamai_output_parser

#### Parser 创建

```erlang
%% 创建通用 Parser
-spec new(format(), map()) -> parser().
new(json, #{repair_common => true}).

%% 创建 JSON Parser
-spec json() -> parser().
-spec json(map()) -> parser().

%% JSON 选项
#{
    extract_codeblock => boolean(),  %% 从代码块提取（默认 true）
    repair_common => boolean(),      %% 修复常见错误（默认 true）
    strip_markdown => boolean(),     %% 移除 markdown（默认 true）
    schema => map()                  %% JSON Schema（可选）
}
```

#### 解析操作

```erlang
%% 基本解析
-spec parse(parser(), binary()) -> parse_result().

%% 带覆盖选项的解析
-spec parse(parser(), binary(), map()) -> parse_result().

%% 带重试的解析
-spec parse_with_retry(parser(), binary(), non_neg_integer()) -> parse_result().
-spec parse_with_retry(parser(), binary(), non_neg_integer(), map()) -> parse_result().

%% 重试选项
#{
    on_retry => fun((Error, Attempt) -> ok),  %% 重试回调
    backoff => fun((Attempt) -> Delay),       %% 退避函数
    max_delay => pos_integer()                %% 最大延迟（默认 5000ms）
}
```

#### 格式指令

```erlang
%% 获取格式指令
-spec get_instructions(format()) -> binary().
-spec get_instructions(format(), map()) -> binary().

%% JSON 指令选项
#{
    schema => map(),      %% JSON Schema
    examples => [map()]   %% 示例列表
}
```

#### 错误处理

```erlang
%% 判断是否可重试
-spec is_retryable_error(parse_error()) -> boolean().
```

### beamai_parser_json

```erlang
%% 解析 JSON
-spec parse(binary(), parse_options()) -> parse_result().

%% 提取 JSON
-spec extract_json(binary()) -> {ok, binary()} | {error, term()}.

%% 提取代码块
-spec extract_json_codeblock(binary()) -> {ok, binary()} | {error, term()}.

%% 查找 JSON 边界
-spec find_json_boundaries(binary()) -> {ok, binary()} | {error, term()}.

%% 修复 JSON
-spec repair_json(binary()) -> binary().
```

### beamai_parser_retry

```erlang
%% 带重试的解析
-spec parse(parser(), binary(), non_neg_integer(), map()) -> parse_result().

%% 带退避策略的重试
-spec parse_with_backoff(parser(), binary(), non_neg_integer(), atom(), map()) -> parse_result().
```

### beamai_parser_instructions

```erlang
%% 生成 JSON 格式指令
-spec json() -> binary().
-spec json(map()) -> binary().

%% 生成 XML 格式指令
-spec xml() -> binary().

%% 生成 CSV 格式指令
-spec csv() -> binary().

%% JSON Schema 转指令
-spec json_schema_to_instruction(map()) -> binary().
```

---

## 使用示例

### 基本解析

```erlang
%% 创建 Parser
Parser = beamai_output_parser:json(),

%% 解析 LLM 输出
LLMOutput = <<"Here is the data:\n```json\n{\"name\": \"Alice\", \"age\": 25}\n```">>,
{ok, #{<<"name">> := <<"Alice">>, <<"age">> := 25}} = beamai_output_parser:parse(Parser, LLMOutput).
```

### 处理格式错误

```erlang
%% LLM 输出带有格式错误
LLMOutput = <<"{name: \"Alice\", age: 25,} // user info">>,

%% Parser 会自动修复
Parser = beamai_output_parser:json(#{repair_common => true}),
{ok, #{<<"name">> := <<"Alice">>, <<"age">> := 25}} = beamai_output_parser:parse(Parser, LLMOutput).
```

### 带重试的解析

```erlang
%% 配置重试选项
RetryOpts = #{
    on_retry => fun(Error, Attempt) ->
        logger:warning("Parse failed, retry #~p: ~p", [Attempt, Error])
    end,
    backoff => exponential
},

%% 最多重试 5 次
case beamai_output_parser:parse_with_retry(Parser, Text, 5, RetryOpts) of
    {ok, Result} ->
        process_result(Result);
    {error, {max_retries_exceeded, Errors}} ->
        logger:error("All retries failed: ~p", [Errors]),
        handle_failure()
end.
```

### 在 Agent 中使用

```erlang
%% 创建带结构化输出的 Agent
Parser = beamai_output_parser:json(#{
    schema => #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{
            <<"action">> => #{<<"type">> => <<"string">>},
            <<"parameters">> => #{<<"type">> => <<"object">>}
        }
    }
}),

%% 生成格式指令
FormatInstructions = beamai_output_parser:get_instructions(json),

%% 配置 Agent
AgentConfig = #{
    system_prompt => <<"You are an AI assistant.\n\n", FormatInstructions/binary>>,
    llm => LLMConfig,
    output_parser => Parser
}.
```

### 处理数组输出

```erlang
%% LLM 输出是数组
LLMOutput = <<"[{\"id\": 1, \"name\": \"A\"}, {\"id\": 2, \"name\": \"B\"}]">>,

Parser = beamai_output_parser:json(),
{ok, [
    #{<<"id">> := 1, <<"name">> := <<"A">>},
    #{<<"id">> := 2, <<"name">> := <<"B">>}
]} = beamai_output_parser:parse(Parser, LLMOutput).
```

---

## 最佳实践

### 1. 提供明确的格式指令

在系统提示词中使用 `get_instructions/2` 生成格式要求，帮助 LLM 生成正确格式的输出。

```erlang
%% 推荐：使用 Schema 和示例
Instructions = beamai_output_parser:get_instructions(json, #{
    schema => MySchema,
    examples => [ExampleOutput]
}).
```

### 2. 启用修复功能

默认情况下 `repair_common` 是启用的，建议保持启用以处理 LLM 的常见格式错误。

```erlang
%% 推荐配置
Parser = beamai_output_parser:json(#{
    extract_codeblock => true,
    repair_common => true,
    strip_markdown => true
}).
```

### 3. 使用适当的重试策略

对于可能失败的解析，使用重试机制并选择合适的退避策略：

```erlang
%% 对于 API 限流场景，使用指数退避
beamai_parser_retry:parse_with_backoff(Parser, Text, 5, exponential, #{}).

%% 对于快速重试场景，使用线性退避
beamai_parser_retry:parse_with_backoff(Parser, Text, 3, linear, #{}).
```

### 4. 处理解析失败

始终处理解析可能失败的情况：

```erlang
case beamai_output_parser:parse(Parser, Text) of
    {ok, Result} ->
        process_result(Result);
    {error, {invalid_json, _}} ->
        %% 可能需要让 LLM 重新生成
        retry_with_llm();
    {error, {extract_failed, _}} ->
        %% 文本中没有找到 JSON
        handle_no_json()
end.
```

### 5. 日志和监控

在重试回调中添加日志，便于调试和监控：

```erlang
RetryOpts = #{
    on_retry => fun(Error, Attempt) ->
        logger:warning("[OutputParser] Retry ~p/~p: ~p",
            [Attempt, MaxRetries, Error]),
        %% 可以发送到监控系统
        metrics:increment(<<"output_parser.retry_count">>)
    end
}.
```

---

## 扩展开发

### 添加新的格式支持

要添加新的输出格式（如 YAML），需要：

1. **创建解析器模块**

```erlang
-module(beamai_parser_yaml).
-export([parse/2]).

parse(Text, Opts) ->
    %% 实现 YAML 解析逻辑
    case yamerl:decode(Text) of
        [Doc] -> {ok, Doc};
        [] -> {error, {invalid_yaml, Text}};
        _ -> {error, {multiple_documents, Text}}
    end.
```

2. **更新主模块分派**

```erlang
%% beamai_output_parser.erl
do_parse(yaml, Text, Opts) ->
    beamai_parser_yaml:parse(Text, Opts);
```

3. **添加格式指令**

```erlang
%% beamai_parser_instructions.erl
yaml() ->
    <<"You must respond with valid YAML format.\n\n"
      "Requirements:\n"
      "- Use proper indentation (2 spaces)\n"
      "- Quote strings containing special characters\n"
      "...">>.
```

4. **更新类型定义**

```erlang
-type format() :: json | xml | csv | yaml | raw.
```

### 自定义修复函数

可以扩展 JSON 修复功能：

```erlang
%% 添加自定义修复
custom_repair_json(Text) ->
    T1 = beamai_parser_json:repair_json(Text),
    %% 添加自定义修复逻辑
    T2 = fix_unicode_escapes(T1),
    T3 = normalize_newlines(T2),
    T3.
```

---

## 更多资源

- [ARCHITECTURE.md](ARCHITECTURE.md) - 架构设计文档
- [DESIGN_PATTERNS.md](DESIGN_PATTERNS.md) - 设计模式文档
- [API_REFERENCE.md](API_REFERENCE.md) - API 参考文档
- [beamai_llm README](../apps/beamai_llm/README.md) - LLM 模块文档
