-module(binary2_tests).

-import(binary2, [trim/1, reverse/1, inverse/1, join/2, suffix/2, prefix/2]).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

trim1_test_() ->
    [ ?_assertEqual(trim(<<>>), <<>>)
    , ?_assertEqual(trim(<<0,1,2>>), <<1,2>>)
    , ?_assertEqual(trim(<<0,0,1,2>>), <<1,2>>)
    , ?_assertEqual(trim(<<1,2,0,0>>), <<1,2>>)
    , ?_assertEqual(trim(<<0,1,2,0>>), <<1,2>>)
    , ?_assertEqual(trim(<<0,0,0,1,2,0,0,0>>), <<1,2>>)
    ].

reverse_test_() ->
    [ ?_assertEqual(reverse(<<0,1,2>>), <<2,1,0>>)
    ].

inverse_test_() ->
    [ ?_assertEqual(inverse(inverse(<<0,1,2>>)), <<0,1,2>>)
    ].

join_test_() ->
    [ ?_assertEqual(join([<<1,2>>, <<3,4>>, <<5,6>>], <<0>>), <<1,2,0,3,4,0,5,6>>)
    ].

suffix_test_() ->
    [ ?_assertEqual(suffix(<<1,2,3,4,5>>, 2), <<4,5>>)
    , ?_assertError(badarg, prefix(<<1,2,3,4,5>>, 25))
    ].

prefix_test_() ->
    [ ?_assertEqual(prefix(<<1,2,3,4,5>>, 2), <<1,2>>)
    , ?_assertError(badarg, prefix(<<1,2,3,4,5>>, 25))
    ].

-endif.
