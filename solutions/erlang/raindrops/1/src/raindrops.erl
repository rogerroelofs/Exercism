-module(raindrops).

-export([convert/1]).

convert(Number) when Number rem 3 * Number rem 5 * Number rem 7 /= 0 -> integer_to_list(Number);
convert(Number) -> 
    SomePrimes = [3, 5, 7],
    Divisors = lists:filter(fun(N) -> Number rem N == 0 end, SomePrimes),
    Sounds = lists:map(fun sound/1, Divisors),
    lists:concat(Sounds).

sound(N) when N == 3 -> "Pling";
sound(N) when N == 5 -> "Plang";
sound(N) when N == 7 -> "Plong".
