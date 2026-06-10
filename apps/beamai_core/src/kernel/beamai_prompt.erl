%%%-------------------------------------------------------------------
%%% @doc 提示词模板：{{变量}} 替换
%%%
%%% 提供简单的模板渲染功能：
%%% - 支持 {{variable}} 占位符语法
%%% - 自动提取模板中的变量名
%%% - 支持从 Context 或 Map 渲染
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(beamai_prompt).

%% API
-export([new/1]).
-export([render/2]).
-export([get_variables/1]).

%% Types
-export_type([prompt_template/0]).

-type prompt_template() :: #{
    template := binary(),
    input_variables := [binary()]
}.

%%====================================================================
%% API
%%====================================================================

%% @doc 创建提示词模板
%%
%% 解析模板字符串中的 {{variable}} 占位符，提取变量名列表。
%%
%% @param Template 模板字符串（如 <<"你好 {{name}}">>）
%% @returns 模板对象（包含原始模板和变量名列表）
-spec new(binary()) -> prompt_template().
new(Template) ->
    Variables = extract_variables(Template),
    #{
        template => Template,
        input_variables => Variables
    }.

%% @doc 渲染模板（替换变量占位符）
%%
%% 支持两种变量来源：
%% - 普通 Map：直接作为变量表
%% - beamai_context:t()：从 variables 字段提取变量表
%%
%% 变量值会自动转为二进制（通过 beamai_utils:to_binary/1）。
%%
%% @param PromptTemplate 模板对象
%% @param VarsOrContext 变量 Map 或执行上下文
%% @returns {ok, 渲染后的二进制} | {error, 原因}
-spec render(prompt_template(), map() | beamai_context:t()) -> {ok, binary()} | {error, term()}.
render(#{template := Template}, #{'__context__' := true} = Context) ->
    do_render(Template, Context);
render(#{template := Template}, Vars) when is_map(Vars) ->
    do_render(Template, Vars).

%% @doc 获取模板中声明的变量名列表
%%
%% 返回去重排序后的变量名列表。
-spec get_variables(prompt_template()) -> [binary()].
get_variables(#{input_variables := Vars}) -> Vars.

%%====================================================================
%% 内部函数
%%====================================================================

%% @private 执行模板渲染
%%
%% 单遍扫描：re:split 以 {{var}} 为分隔符且带捕获组，结果为
%% [文本, 变量名, 文本, 变量名, ..., 文本] 交替列表，逐项查表拼装。
%% 缺失的变量保持 {{var}} 原样（与旧行为一致）。
%% 约束：变量名不能包含 '}'（与 extract_variables 的正则一致）。
do_render(Template, Vars) ->
    try
        NormVars = maps:fold(fun(Key, Value, Acc) ->
            Acc#{beamai_utils:to_binary(Key) => beamai_utils:to_binary(Value)}
        end, #{}, Vars),
        Parts = re:split(Template, <<"\\{\\{([^}]+)\\}\\}">>, [{return, binary}]),
        {ok, iolist_to_binary(render_parts(Parts, NormVars))}
    catch
        _:Reason ->
            {error, {render_failed, Reason}}
    end.

%% @private 拼装 re:split 的交替结果（偶数位文本，奇数位变量名）
render_parts([], _Vars) ->
    [];
render_parts([Text], _Vars) ->
    [Text];
render_parts([Text, Var | Rest], Vars) ->
    Value = maps:get(Var, Vars, <<"{{", Var/binary, "}}">>),
    [Text, Value | render_parts(Rest, Vars)].

%% @private 从模板中提取所有 {{variable}} 变量名
%%
%% 使用正则表达式匹配 {{...}} 模式，返回去重排序的变量名列表。
extract_variables(Template) ->
    case re:run(Template, <<"\\{\\{([^}]+)\\}\\}">>, [global, {capture, [1], binary}]) of
        {match, Matches} ->
            lists:usort([V || [V] <- Matches]);
        nomatch ->
            []
    end.

