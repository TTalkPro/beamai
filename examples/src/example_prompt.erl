%%%-------------------------------------------------------------------
%%% @doc Prompt 模板 + Output Parser 示例
%%%
%%% 演示：
%%%   - beamai:render/2 模板渲染（{{variable}} 替换）
%%%   - beamai_output_parser JSON 解析（容错、代码块提取）
%%%   - 结合 LLM 获取结构化输出
%%%
%%% 使用方法:
%%% ```
%%% ERL_LIBS=../_build/default/lib rebar3 shell
%%% example_prompt:run_template().
%%% example_prompt:run_parser().
%%% example_prompt:run_structured().  %% 需要 LLM
%%% ```
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(example_prompt).

-export([run_template/0, run_parser/0, run_structured/0, run_structured/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc 演示 Prompt 模板渲染（不需要 LLM）
-spec run_template() -> ok.
run_template() ->
    io:format("=== BeamAI Prompt Template Example ===~n~n"),

    %% 1. 基本模板
    Template1 = <<"你好 {{name}}，请用 {{language}} 写一个 {{task}}。"/utf8>>,
    Vars1 = #{
        name => <<"Alice">>,
        language => <<"Erlang">>,
        task => <<"Hello World 程序"/utf8>>
    },
    {ok, Result1} = beamai:render(Template1, Vars1),
    io:format("Template: ~ts~n", [Template1]),
    io:format("Result:   ~ts~n~n", [Result1]),

    %% 2. 多变量模板
    Template2 = <<"分析以下代码的{{aspect}}：\n\n```{{lang}}\n{{code}}\n```"/utf8>>,
    Vars2 = #{
        aspect => <<"性能瓶颈"/utf8>>,
        lang => <<"erlang">>,
        code => <<"lists:foldl(fun(X, Acc) -> [X|Acc] end, [], List)">>
    },
    {ok, Result2} = beamai:render(Template2, Vars2),
    io:format("Rendered prompt:~n~ts~n~n", [Result2]),

    %% 3. 数值变量
    Template3 = <<"生成 {{count}} 个随机数，范围 {{min}} 到 {{max}}"/utf8>>,
    Vars3 = #{count => 5, min => 1, max => 100},
    {ok, Result3} = beamai:render(Template3, Vars3),
    io:format("With numbers: ~ts~n", [Result3]),
    ok.

%% @doc 演示 Output Parser（不需要 LLM）
%%
%% 展示 beamai_output_parser 对常见 LLM 输出格式的容错解析。
-spec run_parser() -> ok.
run_parser() ->
    io:format("~n=== BeamAI Output Parser Example ===~n~n"),

    Parser = beamai_output_parser:json(#{
        extract_codeblock => true,
        repair_common => true
    }),

    %% 1. 正常 JSON
    Input1 = <<"{\"name\": \"Erlang\", \"year\": 1986}">>,
    {ok, R1} = beamai_output_parser:parse(Parser, Input1),
    io:format("1. Normal JSON: ~p~n", [R1]),

    %% 2. Markdown 代码块中的 JSON
    Input2 = <<"Here is the result:\n```json\n{\"status\": \"ok\", \"count\": 42}\n```\nDone.">>,
    {ok, R2} = beamai_output_parser:parse(Parser, Input2),
    io:format("2. From code block: ~p~n", [R2]),

    %% 3. 带尾逗号的 JSON（常见 LLM 错误）
    Input3 = <<"{\"items\": [\"a\", \"b\", \"c\",], \"total\": 3,}">>,
    {ok, R3} = beamai_output_parser:parse(Parser, Input3),
    io:format("3. Trailing comma fixed: ~p~n", [R3]),

    %% 4. 获取格式指令（用于构建 prompt）
    Instructions = beamai_output_parser:get_instructions(json),
    io:format("~n4. Format instructions for LLM:~n~ts~n", [Instructions]),
    ok.

%% @doc 结合 LLM 获取结构化输出（需要 LLM）
-spec run_structured() -> ok.
run_structured() ->
    LLMConfig = example_llm_config:zhipu(),
    run_structured(LLMConfig).

%% @doc 结合 LLM 获取结构化 JSON 输出
%%
%% 使用 Prompt 模板 + Output Parser，让 LLM 返回可解析的 JSON。
-spec run_structured(beamai_chat_completion:config()) -> ok.
run_structured(LLMConfig) ->
    io:format("~n=== BeamAI Structured Output Example ===~n~n"),

    Kernel = beamai:add_llm(beamai:kernel(), LLMConfig),

    %% 1. 获取格式指令
    FormatInstructions = beamai_output_parser:get_instructions(json),

    %% 2. 构建模板
    Template = <<"请分析编程语言 {{language}} 并返回 JSON 格式结果。\n\n"
                 "返回字段：name(名称), year(创建年份), paradigm(编程范式), "
                 "strengths(优势列表, 最多3个)\n\n{{format}}"/utf8>>,
    {ok, Prompt} = beamai:render(Template, #{
        language => <<"Erlang">>,
        format => FormatInstructions
    }),

    io:format("Prompt:~n~ts~n~n", [Prompt]),

    %% 3. 调用 LLM
    Messages = [
        #{role => user, content => Prompt}
    ],

    case beamai:chat(Kernel, Messages) of
        {ok, #{content := Content}, _} ->
            io:format("Raw response:~n~ts~n~n", [Content]),

            %% 4. 用 Output Parser 解析
            Parser = beamai_output_parser:json(#{
                extract_codeblock => true,
                repair_common => true
            }),
            case beamai_output_parser:parse(Parser, Content) of
                {ok, Parsed} ->
                    io:format("Parsed JSON:~n"),
                    print_map(Parsed);
                {error, ParseErr} ->
                    io:format("Parse error: ~p~n", [ParseErr])
            end;
        {error, Reason} ->
            io:format("LLM Error: ~p~n", [Reason])
    end,
    ok.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 格式化打印 map
print_map(Map) when is_map(Map) ->
    maps:foreach(fun(K, V) ->
        io:format("  ~ts: ~ts~n", [format_key(K), format_value(V)])
    end, Map).

format_key(K) when is_binary(K) -> K;
format_key(K) when is_atom(K) -> atom_to_binary(K, utf8);
format_key(K) -> iolist_to_binary(io_lib:format("~p", [K])).

format_value(V) when is_binary(V) -> V;
format_value(V) when is_integer(V) -> integer_to_binary(V);
format_value(V) when is_list(V) ->
    Items = [format_value(I) || I <- V],
    iolist_to_binary([<<"[">>, lists:join(<<", ">>, Items), <<"]">>]);
format_value(V) -> iolist_to_binary(io_lib:format("~p", [V])).
