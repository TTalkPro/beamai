%%%-------------------------------------------------------------------
%%% @doc LLM 工具适配器
%%%
%%% 提供统一的工具定义格式转换，支持不同 LLM Provider。
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(llm_tool_adapter).

%% API
-export([to_openai/1, from_openai/1]).
-export([to_anthropic/1, from_anthropic/1]).
-export([to_provider/2, from_provider/2]).

%% 类型
-type tool() :: #{
    name := binary(),
    description := binary(),
    parameters := map()
}.

-type tool_spec() :: #{
    type := function,
    function := #{
        name := binary(),
        description := binary(),
        parameters := map()
    }
}.

-export_type([tool/0, tool_spec/0]).

%%====================================================================
%% 统一转换接口
%%====================================================================

%% @doc 转换为指定 Provider 格式
-spec to_provider([tool() | tool_spec()], atom()) -> [map()].
to_provider(Tools, openai) -> to_openai(Tools);
to_provider(Tools, anthropic) -> to_anthropic(Tools);
to_provider(Tools, ollama) -> to_openai(Tools);  %% Ollama 使用 OpenAI 格式
to_provider(Tools, _) -> to_openai(Tools).

%% @doc 从指定 Provider 格式转换
-spec from_provider([map()], atom()) -> [tool()].
from_provider(Tools, openai) -> from_openai(Tools);
from_provider(Tools, anthropic) -> from_anthropic(Tools);
from_provider(Tools, ollama) -> from_openai(Tools);
from_provider(Tools, _) -> from_openai(Tools).

%%====================================================================
%% OpenAI 格式转换
%%====================================================================

%% @doc 转换为 OpenAI 工具格式
-spec to_openai([tool() | tool_spec()]) -> [map()].
to_openai(Tools) ->
    [to_openai_tool(T) || T <- Tools].

to_openai_tool(#{type := function, function := Func}) ->
    %% 已经是 OpenAI 格式
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => maps:get(name, Func),
            <<"description">> => maps:get(description, Func),
            <<"parameters">> => maps:get(parameters, Func)
        }
    };
to_openai_tool(#{name := Name, description := Desc, parameters := Params}) ->
    %% 统一格式
    #{
        <<"type">> => <<"function">>,
        <<"function">> => #{
            <<"name">> => Name,
            <<"description">> => Desc,
            <<"parameters">> => Params
        }
    };
to_openai_tool(#{name := Name, description := Desc}) ->
    to_openai_tool(#{name => Name, description => Desc, parameters => default_params()});
to_openai_tool(#{name := Name}) ->
    to_openai_tool(#{name => Name, description => <<"Tool: ", Name/binary>>, parameters => default_params()}).

%% @doc 从 OpenAI 工具格式转换
-spec from_openai([map()]) -> [tool()].
from_openai(Tools) ->
    [from_openai_tool(T) || T <- Tools].

from_openai_tool(#{<<"type">> := <<"function">>, <<"function">> := Func}) ->
    #{
        name => maps:get(<<"name">>, Func, <<>>),
        description => maps:get(<<"description">>, Func, <<>>),
        parameters => maps:get(<<"parameters">>, Func, default_params())
    };
from_openai_tool(Func) when is_map(Func) ->
    #{
        name => maps:get(<<"name">>, Func, <<>>),
        description => maps:get(<<"description">>, Func, <<>>),
        parameters => maps:get(<<"parameters">>, Func, default_params())
    }.

%%====================================================================
%% Anthropic 格式转换
%%====================================================================

%% @doc 转换为 Anthropic 工具格式
-spec to_anthropic([tool() | tool_spec()]) -> [map()].
to_anthropic(Tools) ->
    [to_anthropic_tool(T) || T <- Tools].

to_anthropic_tool(#{type := function, function := Func}) ->
    #{
        <<"name">> => maps:get(name, Func),
        <<"description">> => maps:get(description, Func),
        <<"input_schema">> => maps:get(parameters, Func)
    };
to_anthropic_tool(#{name := Name, description := Desc, parameters := Params}) ->
    #{
        <<"name">> => Name,
        <<"description">> => Desc,
        <<"input_schema">> => Params
    };
to_anthropic_tool(#{name := Name, description := Desc}) ->
    to_anthropic_tool(#{name => Name, description => Desc, parameters => default_params()}).

%% @doc 从 Anthropic 工具格式转换
-spec from_anthropic([map()]) -> [tool()].
from_anthropic(Tools) ->
    [from_anthropic_tool(T) || T <- Tools].

from_anthropic_tool(#{<<"name">> := Name, <<"description">> := Desc, <<"input_schema">> := Schema}) ->
    #{
        name => Name,
        description => Desc,
        parameters => Schema
    };
from_anthropic_tool(#{<<"name">> := Name}) ->
    #{
        name => Name,
        description => <<>>,
        parameters => default_params()
    }.

%%====================================================================
%% 内部函数
%%====================================================================

default_params() ->
    #{type => object, properties => #{}, required => []}.
