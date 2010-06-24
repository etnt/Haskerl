--
-- Created: 9 Jul 2007 by tobbe@tornkvist.org
--

module Pretty2 where

import Text.ParserCombinators.Parsec
import Parser2


pprint :: CoreProgram -> String
pprint prog = iDisplay (pprProgram prog)

mkMultiAp :: Int -> CoreExpr -> CoreExpr -> CoreExpr
mkMultiAp n e1 e2 = foldll EAp e1 (take n e2s)
    where
      e2s = e2 : e2s

iNil     :: Iseq                  -- The empty iseq

iStr     :: String -> Iseq        -- Turn a string into an iseq

iAppend  :: Iseq -> Iseq -> Iseq  -- Append two iseqs

iNewline :: Iseq                  -- New line with indentation

iIndent  :: Iseq -> Iseq          -- Indent an iseq

iDisplay :: Iseq -> String        -- Turn an iseq into a string

iConcat     :: [Iseq] -> Iseq

iInterleave :: Iseq -> [Iseq] -> Iseq

iNum :: Int -> Iseq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> Iseq
iFWNum width n
    = iStr (space (width - length digits) ++ digits)
      where
        digits = show n

digits = show n

iLayn :: [Iseq] -> Iseq
iLayn seqs = iConcat (map lay_item (zip [1..] seqs))
    where
      lay_item (n, seq)
          = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]


clex :: String -> [Token]
clex (c:cs) | isWhiteSpace c = clex cs
clex (c:cs) | isDigit c = num_token : clex rest_cs
            where
              num_token = c : takeWhile isDigit cs
              rest_cs   = dropWhile isDigit cs
clex (c:cs) = [c] : clex cs
clex [] = []


syntax :: [Token] -> CoreProgram

parse :: String -> CoreProgram

parse = syntax . clex

-- In Gofer I propose to compose this with some function
-- CoreProgram -> String, which will illustrate some sort of
-- execution machine, and then give this composition to catWith
-- from my utils

type Token = String           -- A token is never empty

isIdChar, isWhiteSpace :: Char -> Bool
isIdChar c = isAlpha c || isDigit c || (c == '_')


isWhiteSpace c = c `elem` " \t\n"

twoCharOps :: [String]
twoCharOps = ["==", "~=", ">=", "<=", "->"]


type Parser a = [Token] -> [(a, [Token])]

pLit :: String -> Parser String

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = (p1 toks) ++ (p2 toks)


pHelloOrGoodbye :: Parser String
pHelloOrGoodbye = (pLit "hello") `pAlt` (pLit "goodbye")


pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks
    = [ (combine v1 v2, toks2) | (v1,toks1) <- p1 toks,
        (v2,toks2) <- p2 toks1]

pGreeting :: Parser (String, String)
pGreeting = pThen mk_pair pHelloOrGoodbye pVar
    where
      mk_pair hg name = (hg, name)

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = (pOneOrMore p) `pAlt` (pEmpty [])

pGreetings :: Parser [(String, String)]


pEmpty :: a -> Parser a

pOneOrMore :: Parser a -> Parser [a]

pGreetingsN :: Parser Int
pGreetingsN = (pZeroOrMore pGreeting) `pApply` length

pApply :: Parser a -> (a -> b) -> Parser b

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]

pSat :: (String -> Bool) -> Parser String

keywords :: [String]

pNum :: Parser Int

pProgram :: Parser CoreProgram

pSc :: Parser CoreScDefn

data PartialExpr = NoOp | FoundOp Name CoreExpr

pExpr1c :: Parser PartialExpr
pExpr1c = (pThen FoundOp (pLit "|") pExpr1) `pAlt` (pEmpty NoOp)

pExpr1 :: Parser CoreExpr
pExpr1 = pThen assembleOp pExpr2 pExpr1c

assembleOp :: CoreExpr -> PartialExpr -> CoreExpr
assembleOp e1 NoOp = e1
assembleOp e1 (FoundOp op e2) = EAp (EAp (EVar op) e1) e2



