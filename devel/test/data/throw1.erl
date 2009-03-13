-module(throw1).
-compile(export_all).

f1(X) ->
    Z = throw(X).

f2(X) ->
    Z = (catch throw(X)).
