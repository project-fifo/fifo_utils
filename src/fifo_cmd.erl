-module(fifo_cmd).

-export([run/1, run/2, run_json/1, run_json/2]).

run_json(Cmd) ->
    run_json(Cmd, []).

run_json(Cmd, Args) ->
    case run(Cmd, Args) of
        {ok, Data} ->
            {ok, jsx:decode(Data)};
        Error ->
            Error
    end.

run(Cmd) ->
    run(Cmd, []).

run(Cmd, Args) ->
    Port = open_port({spawn_executable, Cmd},
                     [use_stdio, binary, {line, 2048}, {args, mk_args(Args)},
                      stderr_to_stdout, exit_status]),
    wait_for_port(Port).

wait_for_port(Port) ->
    wait_for_port(Port, <<>>).

wait_for_port(Port, Reply) ->
    wait_for_port(Port, Reply, infinity).

wait_for_port(Port, Reply, Timeout) ->
    receive
        {Port, {data, {eol, Data}}} ->
            wait_for_port(Port, <<Reply/binary, Data/binary>>);
        {Port, {data, Data}} ->
            wait_for_port(Port, <<Reply/binary, Data/binary>>);
        {Port,{exit_status, 0}} ->
            {ok, Reply};
        {Port,{exit_status, S}} ->
            {error, S, Reply}
    after
        Timeout ->
            port_close(Port),
            {error, timeout, Reply}
    end.


mk_args([]) ->
    [];
mk_args([{K, V} | R]) ->
    [ mk_val(K), mk_val(V) | mk_args(R)];
mk_args([K | R]) ->
    [ mk_val(K) | mk_args(R)].

mk_val(I) when is_integer(I) ->
    integer_to_list(I);
mk_val(A) when is_atom(A) ->
    [$- | atom_to_list(A)];
mk_val(L) when is_list(L)->
    L;
mk_val(B) when is_binary(B) ->
    binary_to_list(B).
