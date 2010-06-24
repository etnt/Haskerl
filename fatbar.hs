modue Fatbar where

fatbar _ []      = Primop;
fatbar [] (x:_)  = x;
fatbar vs (x:xs) = fatbar2 vs (fatbar3 vs x) xs;

fatbar2 _  _ []          = Primop;
fatbar2 _ (True,fun) _   = fun;
fatbar2 vs  False (x:xs) = fatbar2 vs (fatbar3 vs x) xs;

fatbar3 [] x     = x;
fatbar3 (v:vs) x = fatbar3 vs (x v);
