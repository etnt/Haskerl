--
-- Created: 9 Jul 2007 by tobbe@tornkvist.org
--

module Lexer where


import Text.ParserCombinators.Parsec


{- ***************************************************************************

 http://haskell.org/onlinereport/syntax-iso.html

9.2 Lexical Syntax

 program -> {lexeme | whitespace }

 lexeme  -> literal | special

 special -> ( | ) | , | ; | [ | ] | `| { | }

 whitespace -> whitestuff {whitestuff}

 literal -> integer | string

 integer -> decimal 

 decimal -> digit{digit}

 digit -> ascDigit

 ascDigit -> 0 | 1 | ... | 9

-}


data Literal = LitInteger Integer
             | LitString String
               deriving (Show)


integer :: GenParser Char st Integer
integer = many1 digit >>= \xs -> return $ read xs


doubleQuote = char '"'

litInteger = (integer >>= \x -> return $ LitInteger x ) <?> "integer"

litString = (doubleQuote >>
             manyTill anyChar doubleQuote >>= \str ->
             return $ LitString str) <?> "string"


--
-- parseTest literal "123"  
-- parseTest literal "\"12er3\""
--
literal :: GenParser Char st Literal
literal = litInteger <|> litString


lexeme = literal >>= \x -> return x

lexemeOrWhitespace = spaces >> lexeme >>= \x -> return x

--
-- parseTest program "123  \"abc\"  456"
--
program = lexemeOrWhitespace >>= \x ->
          many lexemeOrWhitespace >>= \xs ->
          return (x:xs)


-- fbind -> qvar = exp
-- qvar -> qvarid
-- qvarid -> [modid.]varid
-- modid -> conid
-- conid -> large{small|large|digit|'}
-- varid -> small{small|large|digit|'}

--
-- parseTest varid "flatMap"
--
varid = lower >>= \x -> 
        many (lower <|> upper <|> digit) >>= \xs -> 
        return (x:xs)


--
-- parseTest conid "Maybe"
--
conid = upper >>= \x -> 
        many (lower <|> upper <|> digit) >>= \xs -> 
        return (x:xs)

--
-- parseTest modid "Mnesia"
--
modid = conid

--
-- parseTest qvarid "Mnesia.transaction"  |  parseTest qvarid "length"
--
data Qvarid = Qvarid String String
              deriving (Show)


maybeModid = modid >>= \m ->
             string "." >>
             return m

{-
qvarid = try (modid >>= \m ->
              string "." >>
              varid >>= \f ->
              return $ Qvarid m f)
         <|>
         (varid >>= \f ->
          return $ Qvarid "" f)
-}


