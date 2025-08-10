-module(protein_translation).

-export([proteins/1]).

proteins(Strand) -> proteins(Strand, []).

proteins([], Acc) -> {ok, lists:reverse(Acc)};
proteins([A, B, C | Rest], Acc) ->
    P = protein([A, B, C]),
    case P of
        {error, badarg} -> {error, badarg};
        stop -> {ok, lists:reverse(Acc)};
        _ -> proteins(Rest, [P|Acc])
    end;
proteins(_, _) -> {error, badarg}.

protein("AUG") -> methionine;
protein("UUU") -> phenylalanine;
protein("UUC") -> phenylalanine;
protein("UUA") -> leucine;
protein("UUG") -> leucine;
protein("UCU") -> serine;
protein("UCC") -> serine;
protein("UCA") -> serine;
protein("UCG") -> serine;
protein("UAU") -> tyrosine;
protein("UAC") -> tyrosine;
protein("UGU") -> cysteine;
protein("UGC") -> cysteine;
protein("UGG") -> tryptophan;
protein("UAA") -> stop;
protein("UAG") -> stop;
protein("UGA") -> stop;
protein(_) -> {error, badarg}.