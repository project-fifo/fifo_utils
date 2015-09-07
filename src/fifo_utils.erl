-module(fifo_utils).
-export([ensure_str/1, ensure_bin/1]).

ensure_str(V) when is_atom(V) ->
    atom_to_list(V);
ensure_str(V) when is_binary(V) ->
    binary_to_list(V);
ensure_str(V) when is_integer(V) ->
    integer_to_list(V);
ensure_str(V) when is_float(V) ->
    float_to_list(V);
ensure_str(V) when is_list(V) ->
    V.

ensure_bin(B) when is_binary(B) ->
    B;
ensure_bin(O) ->
    list_to_binary(ensure_str(O)).
