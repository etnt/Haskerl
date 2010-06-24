--
-- Created: 9 Jul 2007 by tobbe@tornkvist.org
--

module Main where

import System
import Ecore3
import Epretty3

{-

 Compile as:  ghc --make -o Etnt Lexer2.hs Ebif2.hs Bkeep2.hs Parser3.hs Ecore3.hs Epretty3.hs Main3.hs

-}

main = do
  (inFile:outFile:_) <- getArgs
  s <- readFile inFile
  process_file outFile s

process_file outFile s = 
    let result = (epMod . tm . head) (parser s)
    in do
        writeFile outFile result

