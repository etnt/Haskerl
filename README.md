<h3>Prerequisites:</h3>
<ul>
    <li>
        <a href="https://www.erlang-solutions.com/downloads/download-erlang-otp">Erlang OTP</a>
    </li>
    <li>
        GHC (easiest way to get it is from: <a href="https://www.haskell.org/platform/">Haskell Platform</a>)
    </li>
    <li>
        Parsec (Through cabal: cabal install parsec)
    </li>
</ul>
<br/>
Build the Etnt compiler (Etnt):
<br/>
<strong>make all</strong>
<br/>
<br/>
<h3>Compile beam code from haskell/tnt code:</h3>
<br/>
<strong>./etnt gt.tnt</strong>
<br/>
<br/>
If everything went well (i.e. no bugs in your tnt code), you have an output beam file: <strong>gt.beam</strong>
<br/>
<br/>
Open up the erlang shell (erl), and type gt:gt(300,29). The result should be:
<br/>
<br/>
<strong>
Eshell V5.10.4  (abort with ^G)
<br/>
1> gt:gt(300,29).
<br/>
true
<br/>
</strong>
