-module(fifo_opt_test).

-include_lib("eunit/include/eunit.hrl").

-define(M, fifo_opt).

valid_integer_test() ->
    ?assertEqual({true, 1}, ?M:valid_type(integer, 1)),
    ?assertEqual({true, 42}, ?M:valid_type(integer, "42")),
    ?assertEqual(false, ?M:valid_type(integer, "42a")),
    ?assertEqual(false, ?M:valid_type(integer, "a")),
    ?assertEqual(false, ?M:valid_type(integer, a)),
    ?assertEqual({true, [1, 2]}, ?M:valid_type({list, integer}, [1, "2"])),
    ok.

valid_float_test() ->
    ?assertEqual({true, 1.0}, ?M:valid_type(float, 1.0)),
    ?assertEqual({true, 1.0}, ?M:valid_type(float, 1)),
    ?assertEqual({true, 42.0}, ?M:valid_type(float, "42.0")),
    ?assertEqual({true, 42.0}, ?M:valid_type(float, "42")),
    ?assertEqual(false, ?M:valid_type(float, "42a")),
    ?assertEqual(false, ?M:valid_type(float, "a")),
    ?assertEqual(false, ?M:valid_type(float, a)),
    ?assertEqual({true, [1.0, 2.0]}, ?M:valid_type({list, float}, [1.0, "2.0"])),
    ok.

valid_string_test() ->
    ?assertEqual({true, "abc"}, ?M:valid_type(string, "abc")),
    ?assertEqual({true, "abc"}, ?M:valid_type(string, <<"abc">>)),
    ?assertEqual(false, ?M:valid_type(string, 42)),
    ?assertEqual(false, ?M:valid_type(string, a)),
    ?assertEqual({true, ["abc", "123"]}, ?M:valid_type({list, string}, [<<"abc">>, "123"])),
    ?assertEqual(false, ?M:valid_type({list, binary}, [<<"abc">>, "123", 1])),
    ok.

valid_binary_test() ->
    ?assertEqual({true, <<"abc">>}, ?M:valid_type(binary, "abc")),
    ?assertEqual({true, <<"abc">>}, ?M:valid_type(binary, <<"abc">>)),
    ?assertEqual(false, ?M:valid_type(binary, 42)),
    ?assertEqual(false, ?M:valid_type(binary, a)),
    ?assertEqual({true, [<<"abc">>, <<"123">>]}, ?M:valid_type({list, binary}, [<<"abc">>, "123"])),
    ?assertEqual(false, ?M:valid_type({list, binary}, [<<"abc">>, "123", 1])),
    ok.

valid_enum_test() ->
    ?assertEqual({true, abc}, ?M:valid_type({enum, ["abc", "bcd"]}, "abc")),
    ?assertEqual({true, abc}, ?M:valid_type({enum, ["abc", "bcd"]}, "abc")),
    ?assertEqual({true, abc}, ?M:valid_type({enum, ["abc", "bcd"]}, <<"abc">>)),
    ?assertEqual(false, ?M:valid_type({enum, ["bc", "bcd"]}, "abc")),
    ?assertEqual(false, ?M:valid_type({enum, ["bc", "bcd"]}, "abc")),
    ?assertEqual(false, ?M:valid_type({enum, ["bc", "bcd"]}, <<"abc">>)),
    ?assertEqual(false, ?M:valid_type({enum, ["abc", "bcd"]}, 42)).
