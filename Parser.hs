--
-- Created: 10 Jul 2007 by tobbe@tornkvist.org
--

module Parser where

import Text.ParserCombinators.Parsec
import Control.Monad
import Lexer2


--parse =  parseFroomFile programP () $ Lexer2.lexer

{-

 decls     -> decl{decl}
 decl      -> funlhs rhs
 funlhs    -> var apat{apat}
 rhs       -> '=' exp
 exp       -> var '+' var

-}

--data Program = Program [Decl]
--               deriving (Show)


data Def = Fun Fname [Fvar] Expr

type Fname = String

data Fvar = Fvar String

data Expr = Expr Oper Expr Expr
          | Var String   
          | Lit Integer
            deriving (Show)

data Oper = Plus
          | Mult
           deriving (Show)


--
-- To test: parser "x"
--
scanner input = let lexres = runParser lexer () ""
               in case (lexres input) of
                    Left _ -> []
                    Right r -> r

type MyParser a   = GenParser Token () a

mytoken :: (Tok -> Maybe a) -> MyParser a
mytoken test
  = token showToken posToken testToken
  where
    showToken (pos,tok)   = show tok
    posToken  (pos,tok)   = pos
    testToken (pos,tok)   = test tok

-- Now, it is easy to define the basic parser that work on tokens.

-- Test:  parseTest var (scanner "x")

var :: MyParser Expr
var = mytoken (\tok -> case tok of 
                         TokenVar name -> Just (Var name)
                         other         -> Nothing)

lit :: MyParser Expr
lit = mytoken (\tok -> case tok of 
                         TokenInt name -> Just (Lit name)
                         other         -> Nothing)

plusOp :: MyParser Oper
plusOp = mytoken (\tok -> case tok of 
                         TokenPlus -> Just Plus
                         other     -> Nothing)

multOp :: MyParser Oper
multOp = mytoken (\tok -> case tok of 
                         TokenTimes -> Just Mult
                         other      -> Nothing)

bracketOP :: MyParser Bool
bracketOP = mytoken (\tok -> case tok of 
                         TokenOP -> Just True
                         other   -> Nothing)

bracketCP :: MyParser Bool
bracketCP = mytoken (\tok -> case tok of 
                         TokenCP -> Just True
                         other   -> Nothing)

oper :: MyParser Oper
oper = 
    choice 
    [
     plusOp >>= \v -> return v,
     multOp >>= \v -> return v
    ]

factor :: MyParser Expr
factor = 
    choice 
    [
     var     >>= \v -> return v,
     lit     >>= \v -> return v
    ]


expr :: MyParser Expr
expr = 
    (try (expr    >>= \e -> 
          oper    >>= \o -> 
          factor  >>= \f -> 
          return $ Expr o e f))
    <|>
    (factor  >>= \f -> 
     return $ f)


{-
expr2 :: MyParser Expr
expr2 = factor `chainl1` oper
-}















{-
--
-- To test: parser "x"
--
parser input = let lexres = runParser lexer () ""
               in case (lexres input) of
                    Left _ -> []
                    Right r -> var (MyParser r)


--type MyParser a = [Token] -> [(a,[Token])]
data MyParser a t = MyParser ([t] -> [(a,[t])])
-}

{-
instance Monad  (MyParser a) where
    return v = \inp -> [(v,inp)]
    p >>= f = \inp -> concat [f v out | (v,out) <- p inp]

instance MonadPlus (MyParser a) where
    mzero = \inp -> []
    p `mplus` q = \inp -> (p inp `mplus`  q inp) 


var :: MyParser Expr Token
var = \inp -> case inp of
                [] -> mzero
                (pos, TokenVar v):xs -> return [(Var v,xs)]

lit :: MyParser Expr Token
lit = \inp -> case inp of
                [] -> mzero
                (pos, TokenInt v):xs -> return [(Lit v,xs)]

oper :: MyParser Oper Token
oper = \inp -> case inp of
                []                   -> mzero
                (pos, TokenPlus):xs  -> return [(Plus,xs)]
                (pos, TokenTimes):xs -> return [(Mult,xs)]
-}
