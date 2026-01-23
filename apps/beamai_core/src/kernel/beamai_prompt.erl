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

-spec new(binary()) -> prompt_template().
new(Template) ->
    Variables = extract_variables(Template),
    #{
        template => Template,
        input_variables => Variables
    }.

-spec render(prompt_template(), map() | beamai_context:t()) -> {ok, binary()} | {error, term()}.
render(#{template := Template}, #{'__context__' := true} = Context) ->
    Vars = maps:get(variables, Context, #{}),
    do_render(Template, Vars);
render(#{template := Template}, Vars) when is_map(Vars) ->
    do_render(Template, Vars).

-spec get_variables(prompt_template()) -> [binary()].
get_variables(#{input_variables := Vars}) -> Vars.

%%====================================================================
%% Internal
%%====================================================================

do_render(Template, Vars) ->
    try
        Result = maps:fold(fun(Key, Value, Acc) ->
            KeyBin = beamai_utils:to_binary(Key),
            Pattern = <<"{{", KeyBin/binary, "}}">>,
            ValueBin = beamai_utils:to_binary(Value),
            binary:replace(Acc, Pattern, ValueBin, [global])
        end, Template, Vars),
        {ok, Result}
    catch
        _:Reason ->
            {error, {render_failed, Reason}}
    end.

extract_variables(Template) ->
    case re:run(Template, <<"\\{\\{([^}]+)\\}\\}">>, [global, {capture, [1], binary}]) of
        {match, Matches} ->
            lists:usort([V || [V] <- Matches]);
        nomatch ->
            []
    end.

