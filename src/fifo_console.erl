-module(fifo_console).

-export([hdr/1, fields/2, print_config/2]).

hdr(F) ->
    hdr_lines(lists:reverse(F), {"~n", [], "~n", []}).


hdr_lines([{N, n} | R], {Fmt, Vars, FmtLs, VarLs}) ->
    hdr_lines(R, {
                "~20s " ++ Fmt,
                [N | Vars],
                "~20c " ++ FmtLs,
                [$- | VarLs]});

hdr_lines([{N, S}|R], {Fmt, Vars, FmtLs, VarLs}) ->
    %% there is a space that matters here ---------v
    hdr_lines(R, {
                [$~ | integer_to_list(S) ++ [$s, $\s | Fmt]],
                [N | Vars],
                [$~ | integer_to_list(S) ++ [$c, $\s | FmtLs]],
                [$- | VarLs]});

hdr_lines([], {Fmt, Vars, FmtL, VarLs}) ->
    io:format(Fmt, Vars),
    io:format(FmtL, VarLs).


fields(F, Vs) ->
    fields(lists:reverse(F),
           lists:reverse(Vs),
           {"~n", []}).

print_config(Prefix, SubPrefix) ->
    Fmt = [{"Key", 20}, {"Value", 50}],
    hdr(Fmt),
    PrintFn = fun({K, [V|_]}, _) ->
                      fields(Fmt, [key(Prefix, SubPrefix, K), V])
              end,
    riak_core_metadata:fold(PrintFn, ok, {Prefix, SubPrefix}).


fields([{_, n}|R], [V | Vs], {Fmt, Vars}) when is_list(V)
                                     orelse is_binary(V) ->
    fields(R, Vs, {"~s " ++ Fmt, [V | Vars]});

fields([{_, n}|R], [V | Vs], {Fmt, Vars}) ->
    fields(R, Vs, {"~p " ++ Fmt, [V | Vars]});

fields([{_, S}|R], [V | Vs], {Fmt, Vars}) when is_list(V)
                                     orelse is_binary(V) ->
    %% there is a space that matters here ------------v
    fields(R, Vs, {[$~ | integer_to_list(S) ++ [$s, $\s | Fmt]], [V | Vars]});


fields([{_, S}|R], [V | Vs], {Fmt, Vars}) when is_integer(V) ->
    %% there is a space that matters here ------------v
    fields(R, Vs, {[$~ | integer_to_list(S) ++ [$b, $\s | Fmt]], [V | Vars]});

fields([{_, S}|R], [V | Vs], {Fmt, Vars}) ->
    %% there is a space that matters here ------------v
    fields(R, Vs, {[$~ | integer_to_list(S) ++ [$p, $\s | Fmt]], [V | Vars]});

fields([], [], {Fmt, Vars}) ->
    io:format(Fmt, Vars).


key(Prefix, SubPrefix, Key) ->
    io_lib:format("~s.~s.~s", [Prefix, SubPrefix, Key]).

