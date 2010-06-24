-module(zip).
-export([zip/2]).

zip([],[]) -> [];
zip([X|Xs],[Y|Ys]) -> [X+Y|zip(Xs,Ys)].
    
