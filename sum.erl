-module(sum).
-export([ sum/1, sum1/1]).

sum([]) -> 0;
sum([X|Xs]) -> X+sum(Xs).

sum1(X) ->
    if (X == []) ->
            0;
       true ->
            hd(X) + sum(tl(X))
    end.

