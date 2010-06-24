--
-- Created: 9 Jul 2007 by tobbe@tornkvist.org
--

module Main where

import System
import Ecore2 

{-

 Compile as:  ghc --make -o etnt Lexer2.hs Parser2.hs Ecore2.hs Main2.hs

-}

main = do
  (inFile:outFile:_) <- getArgs
  s <- readFile inFile
  process_file outFile s

process_file outFile s = 
    let result = pgm (parser s)
    in do
        writeFile outFile result

