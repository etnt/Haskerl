
all:
	ghc --make -o Etnt Lexer2.hs Ebif2.hs Bkeep2.hs Parser3.hs Ecore3.hs Epretty3.hs Main3.hs

clean:
	rm -f *.hi *.o *.hx

