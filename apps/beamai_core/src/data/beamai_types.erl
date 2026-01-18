%%%-------------------------------------------------------------------
%%% @doc Agent Framework Core Types
%%%
%%% 定义 Agent 框架中所有核心数据类型。
%%%
%%% 设计原则:
%%%   1. 内部统一使用 binary keys
%%%   2. 明确类型约定
%%%   3. 只在边界处做格式转换
%%%   4. 提供类型转换辅助函数
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_types).

%% 类型导出
-export_type([
    llm_role/0,
    llm_message/0,
    tool_call/0,
    tool_response/0,
    llm_response/0,
    tool_spec/0,
    tool_parameters/0
]).

%% API 导出
-export([
    %% 类型转换
    atom_keys_to_binary/1,
    binary_keys_to_atom/1,

    %% 消息构造
    make_system_message/1,
    make_user_message/1,
    make_assistant_message/2,
    make_tool_message/2,

    %% Tool Call 构造
    make_tool_call/3,

    %% 验证
    validate_llm_message/1,
    validate_tool_call/1
]).

%%====================================================================
%% 类型定义
%%====================================================================

%%  LLM 消息角色
%% 注意: 实际使用时这些值会转换为 binary
-type llm_role() :: system | user | assistant | tool.

%%  Tool Call 函数部分（使用 binary keys）
-type tool_call_function() :: #{
    binary() => binary()
}.

%%  Tool Call 格式（标准化，使用 binary keys）
-type tool_call() :: #{
    binary() => binary() | tool_call_function()
}.

%%  Tool Response 格式（使用 binary keys）
-type tool_response() :: #{
    binary() => binary()
}.

%%  LLM 消息格式（标准化，使用 binary keys）
-type llm_message() :: #{
    binary() => binary() | null | [tool_call()]
}.

%%  Token 使用统计
-type usage_info() :: #{
    prompt_tokens := non_neg_integer(),
    completion_tokens := non_neg_integer(),
    total_tokens := non_neg_integer()
}.

%%  LLM 响应格式（标准化）
%% 所有 key 在实际使用时都是 binary 类型
-type llm_response() :: #{
    id := binary(),
    model := binary(),
    content := binary() | null,
    tool_calls := [tool_call()] | [],
    finish_reason := binary() | null,
    usage := usage_info() | undefined
}.

%%  Tool 参数 JSON Schema
%% 所有 key 在实际使用时都是 binary 类型
-type tool_parameters() :: #{
    type := binary(),  % <<"object">>
    properties := map(),
    required := [binary()]
}.

%%  Tool 定义
%% 所有 key 在实际使用时都是 binary 类型
-type tool_spec() :: #{
    name := binary(),
    description := binary(),
    parameters := tool_parameters()
}.

%%====================================================================
%% API - 类型转换
%%====================================================================

%%  将 atom key map 转换为 binary key map
%% 递归处理嵌套的 map 和 list
%% 注意: 也会转换 atom values 为 binary
-spec atom_keys_to_binary(map() | term()) -> map() | term().
atom_keys_to_binary(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Key = case is_atom(K) of
            true -> atom_to_binary(K, utf8);
            false -> K
        end,
        Value = convert_value_for_binary(V),
        Acc#{Key => Value}
    end, #{}, Map);
atom_keys_to_binary(List) when is_list(List) ->
    [atom_keys_to_binary(E) || E <- List];
atom_keys_to_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
atom_keys_to_binary(Term) ->
    Term.

%%  将 binary key map 转换为 atom key map
%% 递归处理嵌套的 map 和 list
%% 注意: 对于不存在的 atom，会保持 binary 形式
%% 注意: 只转换 keys，不转换 values
-spec binary_keys_to_atom(map() | term()) -> map() | term().
binary_keys_to_atom(Map) when is_map(Map) ->
    maps:fold(fun(K, V, Acc) ->
        Key = case is_binary(K) of
            true ->
                try binary_to_existing_atom(K, utf8) of
                    Atom -> Atom
                catch
                    error:badarg -> K
                end;
            false -> K
        end,
        Value = convert_value_for_atom_no_value_conversion(V),
        Acc#{Key => Value}
    end, #{}, Map);
binary_keys_to_atom(List) when is_list(List) ->
    [binary_keys_to_atom(E) || E <- List];
binary_keys_to_atom(Term) ->
    Term.

%% @private 转换值（递归）- 用于 atom_keys_to_binary
convert_value_for_binary(Map) when is_map(Map) ->
    atom_keys_to_binary(Map);
convert_value_for_binary(List) when is_list(List) ->
    [atom_keys_to_binary(E) || E <- List];
convert_value_for_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
convert_value_for_binary(Term) ->
    Term.

%% @private 转换值（递归）- 用于 binary_keys_to_atom（不转换 values）
convert_value_for_atom_no_value_conversion(Map) when is_map(Map) ->
    binary_keys_to_atom(Map);
convert_value_for_atom_no_value_conversion(List) when is_list(List) ->
    [binary_keys_to_atom(E) || E <- List];
convert_value_for_atom_no_value_conversion(Term) ->
    Term.

%%====================================================================
%% API - 消息构造
%%====================================================================

%%  创建系统消息
-spec make_system_message(binary()) -> llm_message().
make_system_message(Content) ->
    #{
        <<"role">> => <<"system">>,
        <<"content">> => Content,
        <<"tool_calls">> => []
    }.

%%  创建用户消息
-spec make_user_message(binary()) -> llm_message().
make_user_message(Content) ->
    #{
        <<"role">> => <<"user">>,
        <<"content">> => Content,
        <<"tool_calls">> => []
    }.

%%  创建助手消息
-spec make_assistant_message(binary(), [tool_call()]) -> llm_message().
make_assistant_message(Content, ToolCalls) ->
    #{
        <<"role">> => <<"assistant">>,
        <<"content">> => Content,
        <<"tool_calls">> => ToolCalls
    }.

%%  创建工具响应消息
-spec make_tool_message(binary(), binary()) -> llm_message().
make_tool_message(ToolCallId, Content) ->
    #{
        <<"role">> => <<"tool">>,
        <<"tool_call_id">> => ToolCallId,
        <<"content">> => Content,
        <<"tool_calls">> => []
    }.

%%  创建 Tool Call
-spec make_tool_call(binary(), binary(), map()) -> tool_call().
make_tool_call(Id, Name, ArgsMap) ->
    ArgsJSON = jsx:encode(ArgsMap),
    #{
        <<"id">> => Id,
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => Name,
            <<"arguments">> => ArgsJSON
        }
    }.

%%====================================================================
%% API - 验证
%%====================================================================

%%  验证 LLM 消息格式
-spec validate_llm_message(term()) -> ok | {error, term()}.
validate_llm_message(#{<<"role">> := Role} = Msg) ->
    case lists:member(Role, [<<"system">>, <<"user">>, <<"assistant">>, <<"tool">>]) of
        true ->
            Content = maps:get(<<"content">>, Msg, null),
            validate_content(Content);
        false ->
            {error, {invalid_role, Role}}
    end;
validate_llm_message(_) ->
    {error, missing_role}.

%%  验证 Tool Call 格式
-spec validate_tool_call(term()) -> ok | {error, term()}.
validate_tool_call(#{<<"id">> := Id, <<"type">> := <<"function">>, <<"function">> := Fun}) ->
    case is_binary(Id) of
        true ->
            Name = maps:get(<<"name">>, Fun, undefined),
            Args = maps:get(<<"arguments">>, Fun, undefined),
            validate_function_fields(Name, Args);
        false ->
            {error, {invalid_id, Id}}
    end;
validate_tool_call(_) ->
    {error, invalid_tool_call_format}.

%% @private 验证 content 字段
validate_content(null) -> ok;
validate_content(Content) when is_binary(Content) -> ok;
validate_content(Content) -> {error, {invalid_content, Content}}.

%% @private 验证 function 字段
validate_function_fields(Name, Args) when is_binary(Name), is_binary(Args) -> ok;
validate_function_fields(Name, _) when not is_binary(Name) -> {error, {invalid_name, Name}};
validate_function_fields(_, Args) -> {error, {invalid_arguments, Args}}.
