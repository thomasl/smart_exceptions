
-module(smart1).
-author('thomasl_erlang@yahoo.com').
-export([parse_transform/2]).
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Current typical usage:
%%   erlc -pa $PATH +'{parse_transform, smart_exceptions}' $ERLFILE
%%
%% PATH must provide smart_exceptions.beam to enable the parse transform.
%%
%% If you want to see what the transform produces, provide the flag '-E'.

parse_transform(Forms, Opts) ->
    %% Opts = compiler options
    M = get_module_name(Forms),
    forms(M, Forms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

forms(M, Forms0) when atom(M) ->
    Forms = simple_resolve_imports(Forms0),
    [ form(M, Form) || Form <- Forms ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% Resolve local calls to primitive operations as follows:
%% 1. Collect all function names defined in module, including imports
%% 2. Walk each form, replacing local calls with remote ones
%%
%% As a side effect, we also resolve imported functions into remote
%% calls at this point.

simple_resolve_imports(Forms) ->
    Imps_and_funcs = func_defs(Forms),
    resolve_calls(Forms, Imps_and_funcs).

func_defs([{attribute, La, import, {M, FAs}}|Forms]) ->
    [ {{F,A}, {import, M}} || {F,A} <- FAs ] ++ func_defs(Forms);
func_defs([{function,Lf, F, A, Clss}|Forms]) ->
    [ {{F,A}, local} | func_defs(Forms) ];
func_defs([_|Forms]) ->
    func_defs(Forms);
func_defs([]) ->
    [].

resolve_calls(Forms, FuncDefs) ->
    [ resolve_form(Form, FuncDefs) || Form <- Forms ].

