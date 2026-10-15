#!/usr/bin/env escript
%%! -nologo

main([]) ->
    Root = ".",
    AppFiles = filelib:wildcard(filename:join(Root, "apps/*/src/*.app.src")),
    case check_apps(AppFiles) of
        ok ->
            io:format("application modules: ok (~p apps)~n", [length(AppFiles)]);
        error ->
            halt(1)
    end;
main(_) ->
    io:put_chars("usage: scripts/check_app_modules.escript\n"),
    halt(2).

check_apps(Files) ->
    Results = [check_app(File) || File <- Files],
    case lists:member(error, Results) of
        true -> error;
        false -> ok
    end.

check_app(File) ->
    {ok, Terms} = file:consult(File),
    {application, App, Options} = lists:keyfind(application, 1, Terms),
    {modules, Declared} = lists:keyfind(modules, 1, Options),
    DeclaredSet = sets:from_list(Declared),
    SrcDir = filename:join(filename:dirname(filename:dirname(File)), "src"),
    ActualSet = sets:from_list(sets:to_list(sets:from_list(source_modules(SrcDir)))),
    Missing = lists:sort(sets:to_list(sets:subtract(ActualSet, DeclaredSet))),
    Stale = lists:sort(sets:to_list(sets:subtract(DeclaredSet, ActualSet))),
    case {Missing, Stale} of
        {[], []} ->
            ok;
        _ ->
            io:format("~ts (~ts)~n", [App, File]),
            print_diff("missing", Missing),
            print_diff("stale", Stale),
            error
    end.

source_modules(Dir) ->
    Files = filelib:wildcard(filename:join(Dir, "**/*.erl")),
    [list_to_atom(filename:basename(File, ".erl")) || File <- Files].

print_diff(_Label, []) ->
    ok;
print_diff(Label, Modules) ->
    io:format("  ~ts: ~ts~n", [Label, format_modules(Modules)]).

format_modules(Modules) ->
    lists:join(", ", [atom_to_list(Module) || Module <- Modules]).
