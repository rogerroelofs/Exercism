-module(rna_transcription).

-export([to_rna/1]).

complement($G) -> $C;
complement($C) -> $G;
complement($T) -> $A;
complement($A) -> $U;
complement(_) -> ''.

to_rna(Strand) ->
    lists:map(fun complement/1, Strand).
