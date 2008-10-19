
-module(ex3).
-author('thomasl_erlang@yahoo.com').
-compile(export_all).

f1(X, Y, Z, W) ->
    <<X:16, Y:16, Z:8, W:40>>.

f2(X, Y, Z, W) ->
    <<X:16, Y:16, 
      Z:8, W:40>>.
