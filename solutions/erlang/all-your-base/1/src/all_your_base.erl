-module(all_your_base).

-export([rebase/3]).

rebase(_digits, inputBase, _outputBase) when inputBase < 2 ->
  {error, "input base must be >= 2"};
rebase(_digits, _inputBase, outputBase) when outputBase < 2 ->
  {error, "output base must be >= 2"};
rebase(digits, inputBase, outputBase) ->
  case convert_to_base_10(digits, inputBase, length(digits) - 1, 0) of
    {error, Message} -> {error, Message};
    0 -> {ok, [0]};
    Number ->  convert_to_base(Number, outputBase, [])
  end.

convert_to_base_10([], _base, _n, sum) ->
  sum;
convert_to_base_10([h|_], base, _n, _sum) when h < 0 orelse h >= base ->
  {error , "all digits must satisfy 0 <= d < input base"};
convert_to_base_10([h|t], base, n, sum) ->
  convert_to_base_10(t, base, n - 1, sum + trunc(h * math:pow(base, n))).

convert_to_base(0, _base, acc) -> {ok, acc};
convert_to_base(n, base, acc) -> convert_to_base(n div base, base, [n rem base | acc]).