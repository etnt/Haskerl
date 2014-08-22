--
-- Created: 9 Jul 2007 by tobbe@tornkvist.org
--

module Main where

import System.Environment (getArgs)
import Ecore
import Epretty

{-

 Compile as:  ghc --make -o Etnt Lexer.hs Ebif.hs Bkeep.hs
                            Parser.hs Ecore.hs Epretty.hs Main.hs

-}

main = do
  (inFile:outFile:_) <- getArgs
  s <- readFile inFile
  process_file outFile s

process_file outFile s =
    let result = (epMod . tm . head) (parser s)
    in do
        writeFile outFile result
