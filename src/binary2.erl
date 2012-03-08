-module(binary2).
-export([ delimit/2
        , first/2
        , atom_to_binary/1
        , reverse/1
        , union/2
        , subtract/2
        , intersection/2
        , inverse/1
        ]).


delimit(Bin, Byte) when is_binary(Bin), is_integer(Byte) ->
    Idx = first(Bin, Byte),
    <<H:Idx/binary, _, T/binary>> = Bin,
    {H, T}.


first(Bin, Byte) when is_binary(Bin), is_integer(Byte) ->
    do_first(Bin, Byte, 0).

do_first(<<X, T/binary>>, Byte, Idx) ->
    case X of
    Byte -> Idx;
    _    -> do_first(T, Byte, Idx+1)
    end.


atom_to_binary(Atom) when is_atom(Atom) ->
    list_to_binary(atom_to_list(Atom)).


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


