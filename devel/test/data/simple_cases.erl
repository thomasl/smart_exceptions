%%% File    : simple_cases.erl
%%% Author  : Thomas Lindgren <>
%%% Description : 
%%% Created : 15 Mar 2009 by Thomas Lindgren <>
%%
%% A number of test cases for smart_exceptions.
%% Compile this module with exns enabled, run the test cases to check that the
%% exceptions look right.

%% param_cases: same, but uses a parametrized module
-module(simple_cases).
-compile(export_all).

%% function clause
%% case, if
%% fun
%% catch, try
%% exit, throw, error
%% BIFs

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% function clause

f1(1) ->
    ok.

f2(_) ->
    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% case
%% - incl nested

c1(X,Y) ->
    case cmp(X, Y) of
	lt ->
	    X;
	eq ->
	    X;
	gt ->
	    Y
    end.

c1(X,Y) ->
    case cmp2(X, Y) of
	le ->
	    if
		X == Y ->
		    eq;
		true ->
		    lt
	    end;
	ge ->
	    if
		X == Y ->
		    eq;
		true ->
		    gt
	    end
    end.

cmp(X, Y) when X < Y ->
    lt;
cmp(X, X) ->
    eq;
cmp(X, Y) when X > Y ->
    gt.

cmp2(X, Y) when X >= Y ->
    ge;
cmp2(X, Y) ->
    le.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% if

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% case+if

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fun

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% catch
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% try

exn1(X, Y) ->
    case catch (1/0) of
	{'EXIT', Rsn} ->
	    {error, Rsn};
	X ->
	    {ok, X}
    end.

exn2(X, Y) ->
    try
	1/0
    catch
	exit:Rsn ->
	    {error, Rsn};
	error:Rsn ->
	    {error, Rsn}
    end.

exn2b(X, Y) ->
    try
	1/(2-2)
    catch
	exit:Rsn ->
	    {error, Rsn};
	error:Rsn ->
	    {error, Rsn}
    end.

exn3(X, Y) ->
    try
	throw(away)
    catch
	exit:Rsn ->
	    {error, Rsn};
	error:Rsn ->
	    {error, Rsn}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

try1(F, X, Y) ->
    try
	F(X), F(Y) of
	{ok, Res} ->
	    ok;
	Else ->
	    {error, Else}
    after
	io:format("done\n", [])
    end.

try2(F, X, Y) ->
    try
	F(X), F(Y) of
	{ok, Res} ->
	    ok;
	Else ->
	    {error, Else}
    catch
	_:Exn ->
	    {exception, Exn}
    after
	io:format("done\n", [])
    end.

try3(F, X, Y) ->
    try
	F(X), F(Y) of
	{ok, Res} ->
	    ok;
	Else ->
	    {error, Else}
    catch
	throw:Exn ->
	    {throw, Exn};
	exit:Exn ->
	    {exit, Exn};
	error:Exn ->
	    {error, Exn}
    after
	io:format("done\n", [])
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% exit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% throw
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% error

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BIFs

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% build binary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% undef function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% higher-order call

