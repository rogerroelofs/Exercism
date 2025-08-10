-module(all_your_base).

-export([rebase/3]).

rebase(_Digits, InputBase, _OutputBase) when InputBase < 2 ->
  {error, "input base must be >= 2"};
rebase(_Digits, _InputBase, OutputBase) when OutputBase < 2 ->
  {error, "output base must be >= 2"};
rebase(Digits, InputBase, OutputBase) ->
  case convert_to_base_10(Digits, InputBase, length(Digits) - 1, 0) of
    {error, Message} -> {error, Message};
    0 -> {ok, [0]};
    Number ->  convert_to_base(Number, OutputBase, [])
  end.

convert_to_base_10([], _Base, _N, Sum) ->
  Sum;
convert_to_base_10([H|_], Base, _N, _Sum) when H < 0 orelse H >= Base ->
  {error , "all digits must satisfy 0 <= d < input base"};
convert_to_base_10([H|T], Base, N, Sum) ->
  convert_to_base_10(T, Base, N - 1, Sum + trunc(H * math:pow(Base, N))).

convert_to_base(0, _Base, Acc) -> {ok, Acc};
convert_to_base(N, Base, Acc) -> convert_to_base(N div Base, Base, [N rem Base | Acc]).