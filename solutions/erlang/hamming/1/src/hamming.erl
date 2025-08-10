-module(hamming).

-export([distance/2]).

distance(Strand1, Strand2) when length(Strand1) /= length(Strand2) ->
    {error, badarg};
distance([], []) -> 0;
distance([H1|T1], [H2|T2]) when H1 == H2 -> distance(T1, T2);
distance([_|T1], [_|T2]) -> 1 + distance(T1, T2);
distance(_, _) -> {error, badarg}.