-module(case_x).
-export([sum/1]).

sum([]) -> 0;
sum([H|T]) -> H+sum(T).
 
    
