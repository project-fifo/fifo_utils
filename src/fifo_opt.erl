-module(fifo_opt).

-ifdef(TEST).
-export([valid_type/2]).
-endif.

-export([get/3, get/6, set/3, unset/2]).

get(Prefix, SubPrefix, Key) ->
    P = fifo_utils:ensure_bin(Prefix),
    SP = fifo_utils:ensure_bin(SubPrefix),
    K = fifo_utils:ensure_bin(Key),
    riak_core_metadata:get({P, SP}, K).

get(Opts, Prefix, SubPrefix, Key, {EnvApp, EnvKey}, Dflt) ->
    case get(Prefix, SubPrefix, Key) of
        undefined ->
            V = case application:get_env(EnvApp, EnvKey) of
                    {ok, Val} ->
                        Val;
                    undefined ->
                        Dflt
                end,
            set(Opts, [Prefix, SubPrefix, Key], V),
            V;
        V ->
            V
    end.

set(Opts, Ks, Val) ->
    [Prefix, SubPrefix, Key] =
        [fifo_utils:ensure_bin(K) || K <- Ks],
    set(Opts, Prefix, SubPrefix, Key, Val).

unset(Opts, Ks) ->
    set(Opts, Ks, undefined).

set(Opts, Prefix, SubPrefix, Key, Val) ->
    case is_valid(Opts, [Prefix, SubPrefix, Key], Val) of
        {true, V1} ->
            riak_core_metadata:put({Prefix, SubPrefix}, Key, V1);
        E ->
            E
    end.

is_valid(Opts, Ks, V) ->
    Ks1 = [fifo_utils:ensure_str(K) || K <- Ks],
    case get_type(Ks1, Opts) of
        {ok, Type} ->
            case  valid_type(Type, V) of
                {true, V1} ->
                    {true, V1};
                false ->
                    {invalid, type, Type}
            end;
        E ->
            E
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

get_type([], _) ->
    {invalid, path};

get_type([K], Os) ->
    case proplists:get_value(fifo_utils:ensure_str(K), Os) of
        undefined ->
            {invalid, key, K};
        Type ->
            {ok, Type}
    end;

get_type([K|R], Os) ->
    case proplists:get_value(fifo_utils:ensure_str(K), Os) of
        undefined ->
            case proplists:get_value('_', Os) of
                undefined ->
                    {invalid, key, K};
                Os1 ->
                    get_type(R, Os1)
            end;
        Os1 ->
            get_type(R, Os1)
    end.

valid_type(integer, I) when is_list(I) ->
    try list_to_integer(I) of
        V ->
            {true, V}
    catch
        _:_ ->
            false
    end;
valid_type(integer, I) when is_integer(I) ->
    {true, I};

valid_type(float, I) when is_list(I) ->
    try list_to_float(I) of
        V ->
            {true, V}
    catch
        _:_ ->
            false
    end;

valid_type(_, undefined) ->
    {true, undefined};

valid_type(float, I) when is_float(I) ->
    {true, I};

valid_type(string, L) when is_binary(L) ->
    {true, binary_to_list(L)};
valid_type(string, L) when is_list(L) ->
    {true, L};

valid_type(binary, B) when is_binary(B) ->
    {true, B};
valid_type(binary, L) when is_list(L) ->
    {true, list_to_binary(L)};

valid_type({enum, Vs}, V) when is_atom(V)->
    valid_type({enum, Vs}, atom_to_list(V));
valid_type({enum, Vs}, V) when is_binary(V)->
    valid_type({enum, Vs}, binary_to_list(V));
valid_type({enum, Vs}, V) ->
    case lists:member(V, Vs) of
        true ->
            {true, list_to_atom(V)};
        false ->
            false
    end;

valid_type(_, _) ->
    false.

