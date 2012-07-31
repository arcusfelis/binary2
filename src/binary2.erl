-module(binary2).

%% Bytes
-export([ reverse/1
        , join/2
        , suffix/2
        , prefix/2
        ]).

%% Bits
-export([ union/2
        , subtract/2
        , intersection/2
        , inverse/1
        ]).

%% Trimming
-export([ rtrim/1
        , rtrim/2
        , ltrim/1
        , ltrim/2
        , trim/1
        , trim/2
        ]).

rtrim(B) when is_binary(B) ->
    S = byte_size(B),
    do_rtrim(S, B, 0).


rtrim(B, X) ->
    S = byte_size(B),
    do_rtrim(S, B, X).


do_rtrim(0, _B, _X) ->
    <<>>;
do_rtrim(S, B, X) ->
    S2 = S - 1,
    case binary:at(B, S2) of
        X -> do_rtrim(S2, B, X);
        _ -> binary_part(B, 0, S)
    end.


ltrim(B) when is_binary(B) ->
    ltrim(B, 0).


ltrim(<<X, B/binary>>, X) ->
    ltrim(B, X);
ltrim(B, _X) ->
    B.


trim(B) when is_binary(B) ->
    trim(B, 0).


trim(B, X) ->
    From = ltrimc(B, X, 0),
    case byte_size(B) of
        From ->
            <<>>;
        S ->
            To = do_rtrimc(S, B, X),
            binary:part(B, From, To - From)
    end.


ltrimc(<<X, B/binary>>, X, C) ->
    ltrimc(B, X, C+1);
ltrimc(_B, _X, C) ->
    C.


do_rtrimc(0, _B, _X) ->
    0;
do_rtrimc(S, B, X) ->
    S2 = S - 1,
    case binary:at(B, S2) of
        X -> do_rtrimc(S2, B, X);
        _ -> S
    end.


%% @doc Reverse the bytes' order.
reverse(Bin) when is_binary(Bin) ->
    S = bit_size(Bin),
    <<V:S/integer-little>> = Bin,
    <<V:S/integer-big>>.


join([B|Bs], Sep) when is_binary(Sep) ->
    R = << <<Sep/binary, X/binary>> || X <- Bs >>,
    <<B/binary, R/binary>>;

join([], _Sep) ->
    <<>>.


prefix(B, L) when is_binary(B), is_integer(L), L > 0 ->
    binary:part(B, 0, L).


suffix(B, L) when is_binary(B), is_integer(L), L > 0 ->
    S = byte_size(B),
    binary:part(B, S-L, L).


union(B1, B2) ->
    S = bit_size(B1),
    <<V1:S>> = B1,
    <<V2:S>> = B2,
    V3 = V1 bor V2,
    <<V3:S>>.


subtract(B1, B2) ->
    S = bit_size(B1),
    <<V1:S>> = B1,
    <<V2:S>> = B2,
    V3 = (V1 bxor V2) band V1,
    <<V3:S>>.


intersection(B1, B2) ->
    S = bit_size(B1),
    <<V1:S>> = B1,
    <<V2:S>> = B2,
    V3 = V1 band V2,
    <<V3:S>>.


inverse(B1) ->
    S = bit_size(B1),
    <<V1:S>> = B1,
    V2 = bnot V1,
    <<V2:S>>.


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
