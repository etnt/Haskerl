--
-- Created: 10 Jul 2007 by tobbe@tornkvist.org
--
-- Taken from:  http://haskell.org/hawiki/ScanningInHaskell
-- slightly extended with SourcePos and some Lexemes.
--
module Lexer2 where

import Text.ParserCombinators.Parsec


type Token = (SourcePos,Tok)
data Tok
      = TokenLet
      | TokenLetRec
      | TokenCase
      | TokenIf
      | TokenThen
      | TokenElse
      | TokenIn
      | TokenOf
      | TokenWhere
      | TokenModule
      | TokenInt Integer
      | TokenVar String
      | TokenTcon String
      | TokenLambda
      | TokenDcare
      | TokenEq
      | TokenGt
      | TokenLt
      | TokenGe
      | TokenEqual
      | TokenLe
      | TokenAnd
      | TokenOr
      | TokenBar
      | TokenPlus
      | TokenMinus
      | TokenTimes
      | TokenDiv
      | TokenSemi
      | TokenComma
      | TokenCons
      | TokenRightArrow
      | TokenTsign
      | TokenOP
      | TokenCP
      | TokenOB
      | TokenCB
    deriving (Eq, Show)

identifier = do
    c <- lower
    cs <- many (letter <|> digit <|> oneOf ['_','\''])
    return (c:cs)

constructorid = do
    c <- upper
    cs <- many (letter <|> digit)
    return (c:cs)

comment = do
    string "--"
    anyChar `manyTill` newline

parseToken = choice [
        many1 space          >> return [],
        try (comment         >> return []),
        try (string "case"   >> getPosition   >>= \p -> return [(p,TokenCase)]),
        try (string "if"     >> getPosition   >>= \p -> return [(p,TokenIf)]),
        try (string "then"   >> getPosition   >>= \p -> return [(p,TokenThen)]),
        try (string "else"   >> getPosition   >>= \p -> return [(p,TokenElse)]),
        try (string "let"    >> getPosition   >>= \p -> return [(p,TokenLet)]),
        try (string "letrec" >> getPosition   >>= \p -> return [(p,TokenLetRec)]),
        try (string "in"     >> getPosition   >>= \p -> return [(p,TokenIn)]),
        try (string "of"     >> getPosition   >>= \p -> return [(p,TokenOf)]),
        try (string "where"  >> getPosition   >>= \p -> return [(p,TokenWhere)]),
        try (string "module" >> getPosition   >>= \p -> return [(p,TokenModule)]),
        try (string ">="     >> getPosition   >>= \p -> return [(p,TokenGe)]),
        try (string "<="     >> getPosition   >>= \p -> return [(p,TokenLe)]),
        try (string "=="     >> getPosition   >>= \p -> return [(p,TokenEqual)]),
        try (string "->"     >> getPosition   >>= \p -> return [(p,TokenRightArrow)]),
        try (string "&&"     >> getPosition   >>= \p -> return [(p,TokenAnd)]),
        try (string "||"     >> getPosition   >>= \p -> return [(p,TokenOr)]),
        try (do {string "::"; p <- getPosition; manyTill anyChar (string ";"); 
                 p2 <- getPosition; return [(p,TokenTsign),(p2,TokenSemi)]}),
        many1 digit     >>= \s -> getPosition >>= \p -> return [(p,TokenInt (read s))],
        char '\\'       >> getPosition        >>= \p -> return [(p,TokenLambda)],
        identifier      >>= \s -> getPosition >>= \p -> return [(p,TokenVar s)],
        constructorid   >>= \s -> getPosition >>= \p -> return [(p,TokenTcon s)],
        char '_'        >> getPosition        >>= \p -> return [(p,TokenDcare)],
        char '='        >> getPosition        >>= \p -> return [(p,TokenEq)],
        char '+'        >> getPosition        >>= \p -> return [(p,TokenPlus)],
        char '-'        >> getPosition        >>= \p -> return [(p,TokenMinus)],
        char '*'        >> getPosition        >>= \p -> return [(p,TokenTimes)],
        char '/'        >> getPosition        >>= \p -> return [(p,TokenDiv)],
        char '>'        >> getPosition        >>= \p -> return [(p,TokenGt)],
        char '<'        >> getPosition        >>= \p -> return [(p,TokenLt)],
        char '|'        >> getPosition        >>= \p -> return [(p,TokenBar)],
        char ';'        >> getPosition        >>= \p -> return [(p,TokenSemi)],
        char ','        >> getPosition        >>= \p -> return [(p,TokenComma)],
        char ':'        >> getPosition        >>= \p -> return [(p,TokenCons)],
        char '('        >> getPosition        >>= \p -> return [(p,TokenOP)],
        char ')'        >> getPosition        >>= \p -> return [(p,TokenCP)],
        char '['        >> getPosition        >>= \p -> return [(p,TokenOB)],
        char ']'        >> getPosition        >>= \p -> return [(p,TokenCB)]
    ]

lexer :: GenParser Char st [Token]
lexer = do xs <- fmap concat (many parseToken)
           eof
           return $ xs
