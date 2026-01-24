%%%-------------------------------------------------------------------
%%% @doc beamai_plugin public header
%%%
%%% Type definitions and macros for plugin development.
%%%
%%% @end
%%%-------------------------------------------------------------------

-ifndef(BEAMAI_PLUGIN_HRL).
-define(BEAMAI_PLUGIN_HRL, true).

%%====================================================================
%% Type definitions
%%====================================================================

%% Tool categories
-type tool_category() :: file | shell | http | search | todo | human | plan | mcp | custom.

%% Tool permissions
-type tool_permission() :: file_read | file_write | shell_access | network_access | all.

%% Parameter types
-type param_type() :: string | integer | number | boolean | array | object.

%% Tool definition (map format)
-type tool_def() :: #{
    name := binary(),
    description := binary(),
    parameters := map(),
    handler := function(),
    category => tool_category(),
    permissions => [tool_permission()],
    metadata => map()
}.

%%====================================================================
%% Parameter macros
%%====================================================================

-define(STRING_PARAM(Desc), #{type => string, description => Desc}).
-define(INT_PARAM(Desc), #{type => integer, description => Desc}).
-define(NUMBER_PARAM(Desc), #{type => number, description => Desc}).
-define(BOOL_PARAM(Desc), #{type => boolean, description => Desc}).
-define(ARRAY_PARAM(ItemType, Desc), #{type => array, items => ItemType, description => Desc}).
-define(ENUM_PARAM(Values, Desc), #{type => string, enum => Values, description => Desc}).
-define(OBJECT_PARAM(Props, Desc), #{type => object, properties => Props, description => Desc}).

%%====================================================================
%% Tool construction macros
%%====================================================================

-define(PARAMS(Props, Required), #{
    type => object,
    properties => Props,
    required => Required
}).

-define(PARAMS(Props), ?PARAMS(Props, maps:keys(Props))).

-define(NO_PARAMS, #{type => object, properties => #{}, required => []}).

-define(TOOL(Name, Desc, Params, Handler), #{
    name => Name,
    description => Desc,
    parameters => Params,
    handler => Handler
}).

-define(TOOL(Name, Desc, Category, Params, Handler), #{
    name => Name,
    description => Desc,
    category => Category,
    parameters => Params,
    handler => Handler
}).

-define(TOOL_FULL(Name, Desc, Category, Perms, Params, Handler, Meta), #{
    name => Name,
    description => Desc,
    category => Category,
    permissions => Perms,
    parameters => Params,
    handler => Handler,
    metadata => Meta
}).

-endif. %% BEAMAI_PLUGIN_HRL
