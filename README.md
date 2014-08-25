Prerequisites:
GHC
Parsec
(easiest way to get it is from: <a href="https://www.haskell.org/platform/">Haskell Platform</a>)

How to compile beam code from haskell code:

First off, run make to build the Etnt compiler:
<strong>    make all</strong>

Then, generate erlang core code:
<strong>    ./Etnt gt.tnt gt.core</strong>

Compile the erlang core code to beam:
<strong>    erlc +from_core gt.core</strong>

Now you have an output beam file: <strong>gt.beam</strong>

Open up the erlang shell, and type gt:gt(300,29). The result should be:

<strong>
Eshell V5.10.4  (abort with ^G)
1> gt:gt(300,29).
true
</strong>
