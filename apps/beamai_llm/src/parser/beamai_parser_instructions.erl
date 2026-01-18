%%%-------------------------------------------------------------------
%%% @doc Output Parser 格式指令生成器
%%%
%%% 为不同的输出格式生成 LLM 提示词指令。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_parser_instructions).

-export([json/0, json/1, xml/0, csv/0]).
-export([json_schema_to_instruction/1]).

%%====================================================================
%% JSON 格式指令
%%====================================================================

%% @doc 默认 JSON 格式指令
-spec json() -> binary().
json() ->
    json(#{}).

%% @doc JSON 格式指令（带选项）
%%
%% 选项：
%% - schema: JSON Schema，会包含在指令中
%% - examples: 示例列表
%% - require_complete: 是否要求完整的 JSON 对象（默认 true）
-spec json(map()) -> binary().
json(Opts) ->
    Base = base_json_instruction(),
    Schema = maybe_add_schema(Opts),
    Examples = maybe_add_examples(Opts),
    Requirements = json_requirements(),
    <<Base/binary, Schema/binary, Examples/binary, Requirements/binary>>.

%% @private 基础 JSON 指令
-spec base_json_instruction() -> binary().
base_json_instruction() ->
    <<"You must respond with valid JSON format. "
      "Your entire response should be a single JSON value (object or array).\n\n"
      "Do NOT include any explanatory text before or after the JSON. "
      "Do NOT wrap the JSON in markdown code blocks unless explicitly requested.\n\n">>.

%% @private JSON 要求
-spec json_requirements() -> binary().
json_requirements() ->
    <<"Requirements:\n"
      "- Output must be valid, parseable JSON\n"
      "- Use double quotes for strings and keys\n"
      "- Do not include trailing commas\n"
      "- Do not use JavaScript-style comments\n"
      "- Escape special characters properly\n"
      "- Ensure all brackets and braces are properly matched\n\n">>.

%% @private 添加 Schema
-spec maybe_add_schema(map()) -> binary().
maybe_add_schema(#{schema := Schema}) when is_map(Schema), map_size(Schema) > 0 ->
    Instruction = json_schema_to_instruction(Schema),
    <<"Expected JSON structure:\n", Instruction/binary, "\n\n">>;
maybe_add_schema(_) ->
    <<>>.

%% @private 添加示例
-spec maybe_add_examples(map()) -> binary().
maybe_add_examples(#{examples := Examples}) when is_list(Examples), length(Examples) > 0 ->
    ExamplesBin = format_examples(Examples),
    <<"Examples:\n", ExamplesBin/binary, "\n\n">>;
maybe_add_examples(_) ->
    <<>>.

%% @private 格式化示例列表
-spec format_examples([map()]) -> binary().
format_examples(Examples) ->
    lists:foldl(
        fun(Example, Acc) ->
            Json = jsx:encode(Example),
            <<Acc/binary, Json/binary, "\n">>
        end,
        <<>>,
        Examples
    ).

%% @doc 将 JSON Schema 转换为指令
-spec json_schema_to_instruction(map()) -> binary().
json_schema_to_instruction(Schema) ->
    Type = maps:get(<<"type">>, Schema, <<"object">>),
    Properties = maps:get(<<"properties">>, Schema, #{}),
    Required = maps:get(<<"required">>, Schema, []),

    case Type of
        <<"object">> ->
            format_object_schema(Properties, Required);
        <<"array">> ->
            Items = maps:get(<<"items">>, Schema, #{}),
            <<"Array of objects with this structure:\n",
              (json_schema_to_instruction(Items))/binary>>;
        _ ->
            <<"Type: ", Type/binary>>
    end.

%% @private 格式化对象 Schema
-spec format_object_schema(map(), [binary()]) -> binary().
format_object_schema(Properties, Required) ->
    Lines = maps:fold(
        fun(Key, PropSchema, Acc) ->
            IsRequired = lists:member(Key, Required),
            Type = maps:get(<<"type">>, PropSchema, <<"any">>),
            Desc = maps:get(<<"description">>, PropSchema, <<>>),
            Line = format_property_line(Key, Type, Desc, IsRequired),
            <<Acc/binary, Line/binary, "\n">>
        end,
        <<>>,
        Properties
    ),
    Lines.

%% @private 格式化属性行
-spec format_property_line(binary(), binary(), binary(), boolean()) -> binary().
format_property_line(Key, Type, Desc, IsRequired) ->
    ReqMarker = case IsRequired of
        true -> <<"*">>;
        false -> <<"">>
    end,
    case Desc of
        <<>> ->
            iolist_to_binary(io_lib:format("  ~s~s: ~s", [ReqMarker, Key, Type]));
        _ ->
            iolist_to_binary(io_lib:format("  ~s~s: ~s - ~s", [ReqMarker, Key, Type, Desc]))
    end.

%%====================================================================
%% XML 格式指令
%%====================================================================

%% @doc XML 格式指令
-spec xml() -> binary().
xml() ->
    <<"You must respond with valid XML format.\n\n"
      "Requirements:\n"
      "- Output must be well-formed XML\n"
      "- All tags must be properly closed\n"
      "- Attribute values must be quoted\n"
      "- Special characters must be escaped (&lt;, &gt;, &amp;, etc.)\n"
      "- Include proper XML declaration if needed\n\n">>.

%%====================================================================
%% CSV 格式指令
%%====================================================================

%% @doc CSV 格式指令
-spec csv() -> binary().
csv() ->
    <<"You must respond with valid CSV format.\n\n"
      "Requirements:\n"
      "- First row should contain headers\n"
      "- Use comma as field separator\n"
      "- Quote fields containing commas or newlines\n"
      "- Use double quotes to escape quotes within fields\n"
      "- Do not include trailing commas\n\n"
      "Example:\n"
      "name,age,city\n"
      "\"John Doe\",30,\"New York\"\n"
      "\"Jane Smith\",25,\"Los Angeles\"\n\n">>.
