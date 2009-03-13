-module(ex2).
-compile(export_all).

f1(X) ->
    try X+1 of
	A when atom(A) -> 10;
	N when float(N) -> 11;
	Z -> Z
    catch
    throw:Term ->
	    io:format("threw ~p\n", [Term]);
    exit:Rsn ->
	    io:format("exited with ~p\n", [Rsn]);
    error:Rsn ->
	    io:format("error with ~p\n", [Rsn])
    end.

f2(X) ->
    try X+1 of
	A when atom(A) -> 10;
	N when float(N) -> 11;
	Z -> Z
    catch
    throw:Term ->
	    io:format("threw ~p\n", [Term]);
    exit:Rsn ->
	    io:format("exited with ~p\n", [Rsn]);
    error:Rsn ->
	    io:format("error with ~p\n", [Rsn])
    after
	io:format("done!\n", [])
    end.

f3(X) ->
    try X+1 of
	A when atom(A) -> 10;
	N when float(N) -> 11;
	Z -> Z
    after
	io:format("done!\n", [])
    end.

f4(X) ->
    try X+1
    after
	io:format("done!\n", []),
      io:format("done!\n", [])
    end.

