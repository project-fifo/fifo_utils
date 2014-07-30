-module(fifo_top_test).

-include_lib("eunit/include/eunit.hrl").

-define(M, fifo_opt).

valid_integer_test() ->
    ?assertEqual({true, 1}, ?M:valid_type(integer, 1)),
    ?assertEqual({true, 42}, ?M:valid_type(integer, "42")),
    ?assertEqual(false, ?M:valid_type(integer, "42a")),
    ?assertEqual(false, ?M:valid_type(integer, "a")),
    ?assertEqual(false, ?M:valid_type(integer, a)).

valid_string_test() ->
    ?assertEqual({true, "abc"}, ?M:valid_type(string, "abc")),
    ?assertEqual({true, "abc"}, ?M:valid_type(string, <<"abc">>)),
    ?assertEqual(false, ?M:valid_type(string, 42)),
    ?assertEqual(false, ?M:valid_type(string, a)).

valid_binary_test() ->
    ?assertEqual({true, <<"abc">>}, ?M:valid_type(binary, "abc")),
    ?assertEqual({true, <<"abc">>}, ?M:valid_type(binary, <<"abc">>)),
    ?assertEqual(false, ?M:valid_type(binary, 42)),
    ?assertEqual(false, ?M:valid_type(binary, a)).

valid_enum_test() ->
    ?assertEqual({true, abc}, ?M:valid_type({enum, ["abc", "bcd"]}, "abc")),
    ?assertEqual({true, abc}, ?M:valid_type({enum, ["abc", "bcd"]}, "abc")),
    ?assertEqual({true, abc}, ?M:valid_type({enum, ["abc", "bcd"]}, <<"abc">>)),
    ?assertEqual(false, ?M:valid_type({enum, ["bc", "bcd"]}, "abc")),
    ?assertEqual(false, ?M:valid_type({enum, ["bc", "bcd"]}, "abc")),
    ?assertEqual(false, ?M:valid_type({enum, ["bc", "bcd"]}, <<"abc">>)),
    ?assertEqual(false, ?M:valid_type({enum, ["abc", "bcd"]}, 42)).
