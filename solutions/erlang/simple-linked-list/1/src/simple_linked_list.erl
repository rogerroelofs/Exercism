-module(simple_linked_list).

-export([cons/2, count/1, empty/0, from_native_list/1,
     head/1, reverse/1, tail/1, to_native_list/1]).

-record(item, {payload = none :: undefined | any(), 
               next = none :: undefined | sl_list()}).
-type sl_list() :: #item{}.

empty() -> #item{payload = none, next = none}.

cons(Elt, List) -> #item{payload = Elt, next = List}.

head(List) -> 
     case List =/= empty() of
        true -> List#item.payload;
        false -> error(badarg)
     end.

tail(List) -> 
     case List =/= empty() of
        true -> List#item.next;
        false -> empty()
     end.

reverse(List) -> 
     case List =:= empty() of
        true -> empty();
        false -> 
          fold(fun(X, Acc) -> 
                    cons(X, Acc) 
               end,
               empty(),
               List)
    end.

count(List) -> count(List, 0).
count(List, Length) -> 
     case List  =/= empty() of
        true -> count(List#item.next, Length + 1);
        false -> Length
    end.

to_native_list(List) -> 
     #item{payload = H, next = T} = List,
     case List  =:= empty() of 
        true -> [];
        false -> [H | to_native_list(T)]
     end.

from_native_list(NativeList) -> 
     case NativeList of 
        [] -> empty();
        [H | T] -> #item{payload = H, next = from_native_list(T)}
    end.

fold(Function, Start, List) -> 
    #item{payload = H, next = T} = List,
    case List =:= empty() of
        true -> Start;
        false -> fold(Function, Function(H, Start), T)
    end.