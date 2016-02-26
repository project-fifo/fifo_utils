-module(fifo_utils).
-export([uuid/0,
         uuid/1,
         uuid_type/1,
         ensure_str/1, ensure_bin/1]).

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

uuid() ->
    uuid(other).

uuid(Type) ->
    uuid:uuid_to_string(uuid_(Type), binary_standard).

uuid_type(UUID) ->
    uuid_type_(uuid:string_to_uuid(UUID)).

uuid_type_(<<1, _/binary>>) ->
    user;
uuid_type_(<<2, _/binary>>) ->
    role;
uuid_type_(<<3, _/binary>>) ->
    org;
uuid_type_(<<4, _/binary>>) ->
    client;
uuid_type_(<<5, _/binary>>) ->
    vm;
uuid_type_(<<6, _/binary>>) ->
    hypervisor;
uuid_type_(<<7, _/binary>>) ->
    package;
uuid_type_(<<8, _/binary>>) ->
    dataset;
uuid_type_(<<9, _/binary>>) ->
    network;
uuid_type_(<<10, _/binary>>) ->
    iprange;
uuid_type_(<<11, _/binary>>) ->
    grouping;
uuid_type_(<<12, _/binary>>) ->
    dtrace;
uuid_type_(<<255, _/binary>>) ->
    ohter;
uuid_type_(_) ->
    undefined.

uuid_(I) when is_integer(I) ->
    <<_, R/binary>> = uuid:get_v4(),
    <<I, R/binary>>;

uuid_(user) ->
    uuid_(1);

uuid_(role) ->
    uuid_(2);

uuid_(org) ->
    uuid_(3);

uuid_(client) ->
    uuid_(4);

uuid_(vm) ->
    uuid_(5);

uuid_(hypervisor) ->
    uuid_(6);

uuid_(package) ->
    uuid_(7);

uuid_(dataset) ->
    uuid_(8);

uuid_(network) ->
    uuid_(9);

uuid_(iprange) ->
    uuid_(10);

uuid_(grouping) ->
    uuid_(11);

uuid_(dtrace) ->
    uuid_(12);

uuid_(other) ->
    uuid_(255).

