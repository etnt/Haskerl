--
-- Created: 9 Jul 2007 by tobbe@tornkvist.org
--

module Parser where

import Text.ParserCombinators.Parsec
import Data.List
import Lexer

--
-- To test: parser "x"
--
scanner input = let lexres = runParser lexer () ""
                in case (lexres input) of
                    Left _ -> []
                    Right r -> r


type CoreExpr = Hxpr

type Name = String

type IsRec = Bool

type Alter a = (Integer, [a], Hxpr)
type CoreAlt = Alter Name

type Program = [TopDecl]
type CoreProgram = Program

type CoreModule = (Name, [CoreProgram])

type ScDefn = (Name, [Hxpr], [Hxpr], Hxpr)

data TopDecl = Decl ScDefn
             | ETcon CType [CTvar] [CTcon]
             | Tsign                           -- Not used (yet...)
               deriving (Show)

type CType = String
type CTvar = String
data CTcon = CTcon CType [CTvar]
             deriving (Show)


type EVar = String                        -- Variable name

data EPat                                 -- Pattern definition
    = EPnil
    | EPvar String
    | EPatom String
    | EPdcare
    | EPnum Integer
    | EPtuple [EPat]
    | EPtcon String [EPat]
    | EPcons EPat EPat
    deriving (Show)

data Hxpr
    = EVar Name                  -- Variable
    | EIVar Name                 -- Internally generated variable
    | ENum Integer               -- Number
    | HPat EPat                  -- Constructor pattern
    | EAp (Hxpr) (Hxpr)          -- Applications
    | EPrimop String [Hxpr]      -- Primop
    | EPapply String [String]      -- Apply (internal usage)
    | EIf (Hxpr) (Hxpr) (Hxpr)   -- Conditional
    | ELet                       -- Let(rec) expressions
      IsRec                      --   boolean with True = recursive,
      [(Name, Hxpr)]             --   Definitions
      (Hxpr)                     --   Body of let(rec)
    | ECase                      -- Case expression
      (Hxpr)                     --   Hxpression to scrutinise
      [CoreAlt]                  --   Alternatives
    | ELam [Name] (Hxpr)         -- Lambda abstractions
    deriving (Show)



--
-- T H E  P A R S E R
--

type MyParser a   = GenParser Token () a

mytoken :: (Tok -> Maybe a) -> MyParser a
mytoken test
  = token showToken posToken testToken
  where
    showToken (pos,tok)   = show tok
    posToken  (pos,tok)   = pos
    testToken (pos,tok)   = test tok


flatHxpr :: [Hxpr] -> Hxpr
flatHxpr (x:xs) = foldl mkEAp x xs

mkEAp e acc  = EAp e acc

--
-- Test:  parseTest evar (scanner "x")
--


eSymbol :: Tok -> MyParser Bool
eSymbol symbol = mytoken (\tok -> if (tok == symbol) then
                                      Just True
                                  else
                                      Nothing)

eModule :: MyParser Bool
eModule = eSymbol TokenModule

eComma :: MyParser Bool
eComma = eSymbol TokenComma

eSemi :: MyParser Bool
eSemi = eSymbol TokenSemi

rightArrow :: MyParser Bool
rightArrow = eSymbol TokenRightArrow

typeSign :: MyParser Bool
typeSign = eSymbol TokenTsign

eOP :: MyParser Bool
eOP = eSymbol TokenOP

eCP :: MyParser Bool
eCP = eSymbol TokenCP

eOB :: MyParser Bool
eOB = eSymbol TokenOB

eCB :: MyParser Bool
eCB = eSymbol TokenCB

eCons :: MyParser Bool
eCons = eSymbol TokenCons

eLet :: MyParser Bool
eLet = eSymbol TokenLet

eLetRec :: MyParser Bool
eLetRec = eSymbol TokenLetRec

eLambda :: MyParser Bool
eLambda = eSymbol TokenLambda

eIn :: MyParser Bool
eIn = eSymbol TokenIn

eCase :: MyParser Bool
eCase = eSymbol TokenCase

eIf :: MyParser Bool
eIf = eSymbol TokenIf

eThen :: MyParser Bool
eThen = eSymbol TokenThen

eElse :: MyParser Bool
eElse = eSymbol TokenElse

eOf :: MyParser Bool
eOf = eSymbol TokenOf

eWhere :: MyParser Bool
eWhere = eSymbol TokenWhere

eDcare :: MyParser Bool
eDcare = eSymbol TokenDcare

ebar :: MyParser Bool
ebar = eSymbol TokenBar

eor :: MyParser Bool
eor = eSymbol TokenOr

edata :: MyParser Name
edata = mytoken (\tok -> case tok of
                         TokenVar "data" -> Just "data"
                         other           -> Nothing)

ename :: MyParser Name
ename = mytoken (\tok -> case tok of
                         TokenVar name -> Just name
                         other         -> Nothing)

evar :: MyParser (Hxpr)
evar = mytoken (\tok -> case tok of
                         TokenVar name -> Just (EVar name)
                         other         -> Nothing)

conid :: MyParser Name
conid = mytoken (\tok -> case tok of
                         TokenTcon name -> Just name
                         other          -> Nothing)

enum :: MyParser (Hxpr)
enum = mytoken (\tok -> case tok of
                         TokenInt name -> Just (ENum name)
                         other         -> Nothing)

eint :: MyParser Integer
eint = mytoken (\tok -> case tok of
                         TokenInt name -> Just name
                         other         -> Nothing)

eqOp :: MyParser (Hxpr)
eqOp = mytoken (\tok -> case tok of
                         TokenEq -> Just (EVar "=")
                         other   -> Nothing)

multOp :: MyParser (Hxpr)
multOp = mytoken (\tok -> case tok of
                         TokenTimes -> Just (EVar "*")
                         other      -> Nothing)

divOp :: MyParser (Hxpr)
divOp = mytoken (\tok -> case tok of
                         TokenDiv -> Just (EVar "/")
                         other    -> Nothing)

plusOp :: MyParser (Hxpr)
plusOp = mytoken (\tok -> case tok of
                         TokenPlus -> Just (EVar "+")
                         other     -> Nothing)

minusOp :: MyParser (Hxpr)
minusOp = mytoken (\tok -> case tok of
                         TokenMinus -> Just (EVar "-")
                         other      -> Nothing)

arithOp :: MyParser (Hxpr)
arithOp = mytoken (\tok -> case tok of
                           TokenPlus  -> Just (EVar "+")
                           TokenMinus -> Just (EVar "-")
                           TokenTimes -> Just (EVar "*")
                           TokenDiv   -> Just (EVar "/")
                           other      -> Nothing)

relOp :: MyParser (Hxpr)
relOp = mytoken (\tok -> case tok of
                           TokenGt    -> Just (EVar ">")
                           TokenLt    -> Just (EVar "<")
                           TokenGe    -> Just (EVar ">=")
                           TokenLe    -> Just (EVar "<=")
                           TokenEqual -> Just (EVar "==")
                           other      -> Nothing)

boolOp :: MyParser (Hxpr)
boolOp = mytoken (\tok -> case tok of
                           TokenAnd -> Just (EVar "&&")
                           TokenOr  -> Just (EVar "||")
                           other    -> Nothing)

binOp :: MyParser (Hxpr)
binOp = do b <- arithOp
           return b
        <|>
        do b <- relOp
           return b
        <|>
        do b <- boolOp
           return b

defn :: MyParser (Name, Hxpr)
defn = do v <- ename
          eqOp
          e <- expr
          return $ (v,e)

--
-- parseTest defns (scanner "x = 3 * 4 ; y = 5 + 3").
--
defns :: MyParser [(Name, Hxpr)]
defns = do d <- sepEndBy1 defn eSemi
           return $ d

--
-- parseTest alt (scanner "1 n -> 0").
--
alt :: MyParser CoreAlt
alt = do n <- eint
         v <- many ename
         rightArrow
         e <- expr
         return $ (n,v,e)

--alts :: MyParser [Alter a]
alts :: MyParser [CoreAlt]
alts = do d <- sepEndBy1 alt eSemi
          return $ d

-- parseTest expr (scanner "let x = 3 * 4 in x * 4;")
-- parseTest expr (scanner "foldl f 0 xs;")
-- parseTest expr (scanner "foldl (+) 0 xs;")
-- parseTest expr (scanner "[1,2,3];")
expr :: MyParser (Hxpr)
expr = do eLet
          d <- defns
          eIn
          e <- expr
          return $ ELet False d e
       <|>
       do eLetRec
          d <- defns
          eIn
          e <- expr
          return $ ELet True d e
       <|>
       do eCase
          e <- expr
          eOf
          a <- alts
          return $ ECase e a
       <|>
       do eIf
          t <- expr
          eThen
          e1 <- expr
          eElse
          e2 <- expr
          return $ EIf t e1 e2
       <|>
       do eLambda
          vars <- many ename
          rightArrow
          e <- expr
          return $ ELam vars e
       <|>
       do e <- expr1
          return e

expr1 :: MyParser (Hxpr)
expr1 = do e <- many1 expr2
           return $ flatHxpr e

-- parseTest expr2 (scanner "x + y * z > c & x > 4")
expr2 :: MyParser (Hxpr)
expr2 =  try (expr3  >>= \e1 ->
              boolOp >>= \op ->
              expr2  >>= \e2 ->
              return $ EAp (EAp op e1) e2)
         <|>
         expr3

expr3 :: MyParser (Hxpr)
expr3 =  try (expr4 >>= \e1 ->
              relOp >>= \op ->
              expr4 >>= \e2 ->
              return $ EAp (EAp op e1) e2)
         <|>
         expr4

expr4 :: MyParser (Hxpr)
expr4 =  try (expr5 >>= \e1 ->
              plusOp >>= \op ->
              expr5 >>= \e2 ->
              return $ EAp (EAp op e1) e2)
         <|>
         try (expr5 >>= \e1 ->
              minusOp >>= \op ->
              expr5 >>= \e2 ->
              return $ EAp (EAp op e1) e2)
         <|>
         (expr5 >>= \e ->
          return $ e)

expr5 :: MyParser (Hxpr)
expr5 =  try (expr6 >>= \e1 ->
              multOp >>= \op ->
              expr6 >>= \e2 ->
              return $ EAp (EAp op e1) e2)    -- FIXME wrap a lambda to
                                              -- make Op curried!!
         <|>
         try (expr6 >>= \e1 ->
              divOp >>= \op ->
              expr6 >>= \e2 ->
              return $ EAp (EAp op e1) e2)
         <|>
         (expr6 >>= \e ->
          return $ e)

expr6 :: MyParser (Hxpr)
expr6 = do a <- many1 aexpr
           return $ flatHxpr a

aexpr :: MyParser (Hxpr)
aexpr = try (do p <- apat
                return p
                )
        <|>
        do a <- evar
           return a
        <|>
        do a <- enum
           return a
        <|>
        try (do eOP
                e <- binOp
                eCP
                return e
                )
        <|>
        do eOP
           e <- expr
           eCP
           return e

--
-- parseTest sc (scanner "add (x:xs) (y:ys) = x + y")
-- parseTest sc (scanner "data Bool = True | False")
-- parseTest sc (scanner "head :: [a] -> a;head (x:_) = x;")
-- parseTest sc (scanner "plus = (+)")
-- parseTest sc (scanner "procString xs | isString xs = True;")
-- parseTest sc (scanner "procString xs | xs > 34 = True;")
-- parseTest sc (scanner "isString (x:xs) | x >= 0 && x <= 255 = isString xs;"
-- parseTest sc (scanner "xx = [1,2,3];")
--
sc :: MyParser TopDecl
sc = try (do edata
             (t,tvs) <- simpletype
             eqOp
             tcs <- constrs
             return $ ETcon t tvs tcs)
     <|>
     try (do evar
             typeSign
             return Tsign)
     <|>
     decl

-- parseTest decl (scanner "procString xs | xs > 34 = True;")
-- parseTest decl (scanner "fatbar2 vs False (x:xs) = 32")
decl :: MyParser TopDecl
decl = do (name,apats)  <- funlhs
          (guard, expr) <- rhs
          return $ Decl (name, apats, guard, expr)

rhs :: MyParser ([Hxpr],Hxpr)
rhs = try (do eqOp
              e <- expr
              return $ ([],e))
      <|>
      gdrhs


gdrhs :: MyParser ([Hxpr],Hxpr)
gdrhs = do guard <- gd
           eqOp
           e <- expr
           return $ ([guard],e)

gd :: MyParser Hxpr
gd = do ebar
        e <- expr
        return e

--
-- parseTest funlhs (scanner "sum (x:xs)")
-- parseTest decl (scanner "fatbar2 vs False (x:xs) = 32")
--
funlhs :: MyParser (Name,[(Hxpr)])
funlhs = do (EVar name) <- evar
            apats <- many apat
            return $ (name, apats)

--
-- parseTest apat (scanner "(x:xs)")
-- parseTest apat (scanner "[]")      only allow an empty list for now!
-- parseTest apat (scanner "[x:xs,y]")
-- parseTest apat (scanner "[x:(z:xs),y]")
-- parseTest apat (scanner "[1,2,3]")
-- parseTest apat (scanner "False")
--
apat :: MyParser (Hxpr)
apat = do p <- xpat
          return $ HPat p

xpat :: MyParser EPat
xpat = try (do eOP
               p <- pat
               eCP
       	       return $ p)
       <|>
       try (do eOB
               a <- apatList
               eCB
       	       return $ a)
       <|>
       try (do (EVar var) <- evar
       	       return $ EPvar var)
       <|>
       try (do eDcare
       	       return $ EPdcare)
       <|>
       try (do c <- con
       	       return $ EPtcon c [])

apatList :: MyParser EPat
apatList = try (do p1 <- pat
                   eComma
                   p2 <- apatList
       	           return $ EPcons p1 p2)
           <|>
           try (do p1 <- pat
       	           return $ EPcons p1 EPnil)
           <|>
           return EPnil

apatTuple :: MyParser [EPat]
apatTuple = try (do p <- pat
                    eComma
                    ps <- apatTuple
       	            return $ (p:ps))
           <|>
           try (do p <- pat
       	           return $ [p])
           <|>
           return []

pat :: MyParser EPat
pat = try (do p1 <- xpat
              eCons
              p2 <- xpat
       	      return $ EPcons p1 p2)
      <|>
      try (do p <- xpat
              eComma
              ps <- apatTuple
       	      return $ EPtuple (p:ps))
      <|>
      try (do (EVar var) <- evar
       	      return $ EPvar var)
      <|>
      try (do (ENum n) <- enum
       	      return $ EPnum n)
      <|>
      try (do eDcare
       	      return $ EPdcare)
      <|>
      try (do c <- con
              ps <- many xpat
       	      return $ EPtcon c ps)

simpletype :: MyParser (String,[String])
simpletype = do t <- tycon
                tvs <- many tyvar
                return $ (t,tvs)

constrs :: MyParser [CTcon]
constrs = do tc <- sepBy1 constr ebar
             return $ tc

constr :: MyParser CTcon
constr = do c <- con
            tvs <- many atype
            return $ CTcon c tvs

tycon :: MyParser String
tycon = conid

con ::MyParser String
con = conid

atype :: MyParser String
atype = tyvar

tyvar :: MyParser String
tyvar = varid

varid :: MyParser String
varid = ename

--
-- parseTest program (scanner "add x y = x + y; mul x z = z*x;")
-- parseTest program (scanner "data Bool = True | False; add x y = x + y; mul x z = z*x;")
-- parseTest program (scanner "head :: [a] -> a;")
--
program :: MyParser CoreProgram
program = do p <- endBy1 sc eSemi
             return $ p

--
-- parseTest corePgm (scanner "module add where add (x:xs) (y:ys) = x + y; mul x z = z*x;")
-- parseTest corePgm (scanner "module abc where add (x:xs) (y:ys) = x + y; sum [] = 0; sum (x:xs) = x + sum xs; mul x z = z*x;")
-- parseTest corePgm (scanner "module zip where zip [] [] = []; zip (x:xs) (y:ys) = (x+y):zip xs ys")
-- parseTest corePgm (scanner "module zip where zip [] [] = []; zip (x:xs) (y:ys) = let z = x+y;zs = zip xs ys in (z:zs)")
--
corePgm :: MyParser CoreModule
corePgm = do eModule
             modName <- conid
             eWhere
             pgm <- program
             return $ (modName, (groupBy sortByClause pgm))

sortByClause :: TopDecl -> TopDecl -> Bool
sortByClause (Decl (name1,_,_,_)) (Decl (name2,_,_,_)) | name1 == name2 = True
sortByClause _ _ = False
