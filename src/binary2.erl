-module(binary2).
-export([ reverse/1
        , union/2
        , subtract/2
        , intersection/2
        , inverse/1
        , rtrim/1
        , rtrim/2
        , ltrim/1
        , ltrim/2
        , trim/1
        , trim/2
        ]).


rtrim(B) ->
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


ltrim(B) ->
    ltrim(B, 0).


ltrim(<<X, B/binary>>, X) ->
    ltrim(B, X);
ltrim(B, _X) ->
    B.


trim(B) ->
    trim(B, 0).


trim(B, X) ->
    From = ltrimc(B, X, 0),
    case byte_size(B) of
        From ->
            <<>>;
        S ->
            To = do_rtrimc(S, B, X),
            binary_part(B, From, To)
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
        X -> do_rtrim(S2, B, X);
        _ -> S
    end.


reverse(Bin) when is_binary(Bin) ->
    S = size(Bin),
    <<V:S/integer-little>> = Bin,
    <<V:S/integer-big>>.


union(B1, B2) ->
    S = size(B1),
    <<V1:S>> = B1,
    <<V2:S>> = B2,
    V3 = V1 bor V2,
    <<V3:S>>.


subtract(B1, B2) ->
    S = size(B1),
    <<V1:S>> = B1,
    <<V2:S>> = B2,
    V3 = (V1 bxor V2) band V1,
    <<V3:S>>.


intersection(B1, B2) ->
    S = size(B1),
    <<V1:S>> = B1,
    <<V2:S>> = B2,
    V3 = V1 band V2,
    <<V3:S>>.


inverse(B1) ->
    S = size(B1),
    <<V1:S>> = B1,
    V2 = not V1,
    <<V2:S>>.


