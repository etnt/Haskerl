--
-- Created: 11 Jul 2007 by tobbe@tornkvist.org
--

-- Test: parseTest expr "1+2*3"   -- '*' has higher priority

module Expr where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr

expr    :: Parser Integer
expr    = buildExpressionParser table factor
        <?> "expression"

table   = [[op "*" (*) AssocLeft, op "/" div AssocLeft]
          ,[op "+" (+) AssocLeft, op "-" (-) AssocLeft]
          ]          
        where
          op s f assoc
             = Infix (do{ string s; return f}) assoc

factor  = do{ char '('
            ; x <- expr
            ; char ')'
            ; return x 
            }
        <|> number
        <?> "simploe expression"

number  :: Parser Integer
number  = do{ ds <- many1 digit
            ; return $ read ds
            }
        <?> "number"
