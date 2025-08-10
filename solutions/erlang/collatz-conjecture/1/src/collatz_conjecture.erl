-module(collatz_conjecture).

-export([steps/1]).

steps(_N) when _N =< 0 -> erlang:error(badarg);
steps(_N) when _N > 0 -> steps(_N, 0).

steps(_N, _S) when _N == 1 -> _S;
% even
steps(_N, _S) when  _N >= 0, (_N band 1) == 0 -> steps(_N div 2, _S + 1);
% odd
steps(_N, _S) when _N > 0, (_N band 1) /= 0 -> steps(_N * 3 + 1, _S + 1).
