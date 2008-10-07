-module(ex1).
-compile(export_all).

f(X, Y) ->
    case X of
	{ok, Z} ->
	    Z;
	_ ->
	    Y
    end.

max(X, Y) ->
    if
	X > Y ->
	    X;
	true ->
	    Y
    end.
