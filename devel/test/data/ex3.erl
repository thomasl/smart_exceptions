
-module(ex3).
-author('thomasl_erlang@yahoo.com').
-compile(export_all).

f1(X, Y, Z, W) ->
    <<X:16, Y:16, Z:8, W:40>>.

f2(X, Y, Z, W) ->
    <<X:16, Y:16, 
      Z:8, W:40>>.

f3(X, Y, Z, W) ->
    <<X:16/unsigned-big, Y:16/unsigned-little, Z:8, W:40/signed-big>>.

f4(X, Y, Z, W) ->
    <<X/unsigned-big, Y/unsigned-little, Z/native, W/signed-big>>.

f5(X) ->
    <<X>>.

%% nb: cmd_bits uses that terrible property of macros ...
-define(dia_vsn, 1).
-define(cmd_bits(R, P, E, T, Rsrv), R:1, P:1, E:1, T:1, Rsrv:4).

enc_dia(Cmd_code, R, P, E, T, AppID, HopID, EE_ID, Body) ->
    Len = 20 + size(Body),
    <<?dia_vsn:8, Len:24, 
      ?cmd_bits(R, P, E, T, 0), Cmd_code:24,
      AppID:32,
      HopID:32,
      EE_ID:32,
      Body/binary>>.

