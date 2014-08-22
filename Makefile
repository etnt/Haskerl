
all:
	ghc --make -o Etnt Lexer.hs Ebif.hs Bkeep.hs Parser.hs Ecore.hs Epretty.hs Main.hs

clean:
	rm -f *.hi *.o *.hx

