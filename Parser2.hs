--
-- Created: 9 Jul 2007 by tobbe@tornkvist.org
--

module Parser2 where

import Text.ParserCombinators.Parsec
import Data.List
import Lexer2

--
-- To test: parser "x"
--
scanner input = let lexres = runParser lexer () ""
               in case (lexres input) of
                    Left _ -> []
                    Right r -> r

{-

--
-- Haskell Module definition
-- 
module  -> module modid [exports] where body
	|  body
body 	-> { impdecls ; topdecls }
	|  { impdecls }
	|  { topdecls }

modid 	-> conid

impdecls  -> impdecl1 ; ... ; impdecln 	(n>=1)
topdecls  -> topdecl1 ; ... ; topdecln 	(n>=1)

--
-- BNF syntax for the Core language (with some additions...)
--

module  -> module modid where body

body 	-> program

program  ->  sc_1 ; ... ; sc_n      ,n >= 1

sc  -> var var_1 ... var_n = expr   ,n >= 0

expr  -> let defns  in expr            -- Local definitions
       | letrec defns  in expr         -- Local recursive definitions
       | case  expr  of  alts          -- Case expression
       | if expr then expr else expr   -- Conditional
       | \  var_1 ... var_n -> expr    -- Lambda abstraction (n >= 1)
       | expr1                         -- Atomic expression 

aexpr  -> var                        -- Variable
       |  num                        -- Number
       |  Pack{num,num}              -- Constructor
       |  ( expr )                   -- Parenthesised expression

aexp    ->   qvar 		      (variable)
	|    gcon 		      (general constructor)
	|    literal


funlhs 	 ->   var apat {apat }
	 |    pati+1 varop(a,i) pati+1
	 |    lpati varop(l,i) pati+1
	 |    pati+1 varop(r,i) rpati
	 |    ( funlhs ) apat {apat }


apat 	 ->   var [@ apat]	 (as pattern)
	 |    gcon   (arity gcon = 0)
	 |    qcon { fpat1 , ... , fpatk }	(labeled pattern, k>=0)
	 |    literal
	 |    _		(wildcard)
	 |    ( pat ) 	(parenthesized pattern)
	 |    ( pat1 , ... , patk )    (tuple pattern, k>=2)
	 |    [ pat1 , ... , patk ]    (list pattern, k>=1)
	 |    ~ apat   (irrefutable pattern)

pat  ->  var + integer  (successor pattern)
| pat0
pati -> pati+1 [qconop(n,i) pati+1]
| lpati
| rpati
lpati -> (lpati | pati+1) qconop(l,i) pati+1
lpat6 -> - (integer | float) (negative literal)
rpati -> pati+1 qconop(r,i) (rpati | pati+1)
pat10 -> apat
| gcon apat1 ... apatk (arity gcon = k, k>=1)

	     
gcon 	     ->		()
	     | 		[]
	     | 		(,{,})
	     | 		qcon
var 	     -> 	varid | ( varsym )	(variable)
qvar 	     -> 	qvarid | ( qvarsym ) 	(qualified variable)
con 	     -> 	conid | ( consym ) 	(constructor)
qcon 	     -> 	qconid | ( gconsym ) 	(qualified constructor)
varop 	     -> 	varsym | `varid `  (variable operator)
qvarop 	     -> 	qvarsym | `qvarid `	     (qualified variable operator)
conop 	     -> 	consym | `conid ` (constructor operator)
qconop 	     -> 	gconsym | `qconid `	       (qualified constructor operator)
op 	     -> 	varop | conop 	  (operator)
qop 	     -> 	qvarop | qconop   (qualified operator)
gconsym      -> 	: | qconsym




defns  -> defn_1; ...;  defn_n     ,n >= 1 
defn   -> var = expr  

alts   -> alt_1; ...; alt_n              ,n >= 1 
alt    -> <num> var_1 ... var_n -> expr  ,n >= 0 

binop   -> arithop | relop | boolop  
arithop -> + | - | * | /	     -- Arithmetic
relop   -> < | <= | == | = | >= | >  -- Comparison
boolop  -> & | \|	             -- Boolean

var     -> alpha varch_1 ... varch_n     ,n >= 0  
alpha   -> "an alphabetic character"  
varch   -> alpha | digit | _  

num     -> digit_1 ... digit_n           ,n >= 1


--
-- Grammar, expressing operator precedence and associativity.
--


expr1  -> expr2 expr1c
expr1c -> | expr1
	  | epsilon
expr2  ->  expr3 boolop expr2 
	 |  expr3 
expr3  ->  expr4  relop expr4 
	 |  expr4 
expr4  ->  expr5  +  expr4 
	 |  expr5  -  expr5 
	 |  expr5 
expr5  ->  expr6  *  expr5 
	 |  expr6  /  expr6 
	 |  expr6 
expr6  ->  aexpr_1 ... aexpr_n  (n >= 1)


-}


type CoreExpr = Expr Name

type Name = String

type IsRec = Bool

type Alter a = (Integer, [a], Expr a)
type CoreAlt = Alter Name

type Program a = [ScDefn a]
type CoreProgram = Program Name

type ScDefn a = (Name, [APat], Expr a)
type CoreScDefn = ScDefn Name

type CoreModule = (Name, [CoreProgram])

data EPat a
    = EPvar Name
    | ECons (EPat a) (EPat a)
    | ENil
    deriving (Show)

type APat = EPat Name

data Expr a
    = EVar Name                  -- Variables
    | ENum Integer               -- Numbers
    | EConstr Integer Integer    -- Constructor tag arity
    | EAp (Expr a) (Expr a)      -- Applications
    | EIf (Expr a) (Expr a) (Expr a)  -- Conditional
    | ELet                       -- Let(rec) expressions
      IsRec                      --   boolean with True = recursive,
      [(Name, Expr a)]           --   Definitions
      (Expr a)                   --   Body of let(rec)
    | ECase                      -- Case expression
      (Expr a)                   --   Expression to scrutinise
      [CoreAlt]                  --   Alternatives
    | ELam [Name] (Expr a)          -- Lambda abstractions
    deriving (Show)

recursive, nonRecursive :: IsRec
recursive    = True
nonRecursive = False


bindersOf :: [(a,b)] -> [a]
bindersOf defns =  [name | (name, rhs) <- defns]

rhssOf        :: [(a,b)] -> [b]
rhssOf defns  =  [rhs  | (name, rhs) <- defns]


isAtomicExpr :: Expr a -> Bool
isAtomicExpr (EVar v) = True
isAtomicExpr (ENum n) = True
isAtomicExpr e        = False

{-
preludeDefs :: CoreProgram
preludeDefs
    = [ ("I", ["x"], EVar "x"),
        ("K", ["x","y"], EVar "x"),
        ("K1",["x","y"], EVar "y"),
        ("S", ["f","g","x"], EAp (EAp (EVar "f") (EVar "x"))
                (EAp (EVar "g") (EVar "x"))),
        ("compose", ["f","g","x"], EAp (EVar "f")
                      (EAp (EVar "g") (EVar "x"))),
        ("twice", ["f"], EAp (EAp (EVar "compose") (EVar "f")) (EVar "f")) ]
-}

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


flatExpr :: [Expr a] -> Expr a
flatExpr [x] = x
flatExpr (x:xs) = EAp x (flatExpr xs)

--
-- Test:  parseTest evar (scanner "x")
--


eSymbol :: Tok -> MyParser Bool
eSymbol symbol = mytoken (\tok -> if (tok == symbol) then 
                                      Just True
                                  else 
                                      Nothing)


eModule :: MyParser Bool
eModule = mytoken (\tok -> case tok of 
                         TokenModule -> Just True
                         other       -> Nothing)

ename :: MyParser Name
ename = mytoken (\tok -> case tok of 
                         TokenVar name -> Just name
                         other         -> Nothing)

evar :: MyParser (Expr a)
evar = mytoken (\tok -> case tok of 
                         TokenVar name -> Just (EVar name)
                         other         -> Nothing)

eComma :: MyParser Bool
eComma = eSymbol TokenComma

eSemi :: MyParser Bool
eSemi = eSymbol TokenSemi

{-
eSemi :: MyParser Bool
eSemi = mytoken (\tok -> case tok of 
                         TokenSemi  -> Just True
                         other      -> Nothing)
-}

rightArrow :: MyParser Bool
rightArrow = mytoken (\tok -> case tok of 
                         TokenRightArrow  -> Just True
                         other            -> Nothing)

eOP :: MyParser Bool
eOP = mytoken (\tok -> case tok of 
                         TokenOP  -> Just True
                         other    -> Nothing)
eCP :: MyParser Bool
eCP = mytoken (\tok -> case tok of 
                         TokenCP  -> Just True
                         other    -> Nothing)

eOB :: MyParser Bool
eOB = mytoken (\tok -> case tok of 
                         TokenOB  -> Just True
                         other    -> Nothing)
eCB :: MyParser Bool
eCB = mytoken (\tok -> case tok of 
                         TokenCB  -> Just True
                         other    -> Nothing)

eCons :: MyParser Bool
eCons = mytoken (\tok -> case tok of 
                         TokenCons  -> Just True
                         other      -> Nothing)

eLet :: MyParser Bool
eLet = mytoken (\tok -> case tok of 
                         TokenLet  -> Just True
                         other     -> Nothing)

eLetRec :: MyParser Bool
eLetRec = mytoken (\tok -> case tok of 
                         TokenLetRec  -> Just True
                         other        -> Nothing)
eLambda :: MyParser Bool
eLambda = mytoken (\tok -> case tok of 
                         TokenLambda  -> Just True
                         other        -> Nothing)
eIn :: MyParser Bool
eIn = mytoken (\tok -> case tok of 
                         TokenIn  -> Just True
                         other    -> Nothing)

eCase :: MyParser Bool
eCase = mytoken (\tok -> case tok of 
                         TokenCase  -> Just True
                         other      -> Nothing)

eIf :: MyParser Bool
eIf = mytoken (\tok -> case tok of 
                         TokenIf  -> Just True
                         other    -> Nothing)

eThen :: MyParser Bool
eThen = mytoken (\tok -> case tok of 
                         TokenThen  -> Just True
                         other      -> Nothing)

eElse :: MyParser Bool
eElse = mytoken (\tok -> case tok of 
                         TokenElse  -> Just True
                         other      -> Nothing)

eOf :: MyParser Bool
eOf = mytoken (\tok -> case tok of 
                         TokenOf  -> Just True
                         other    -> Nothing)
eWhere :: MyParser Bool
eWhere = mytoken (\tok -> case tok of 
                         TokenWhere  -> Just True
                         other       -> Nothing)

enum :: MyParser (Expr a)
enum = mytoken (\tok -> case tok of 
                         TokenInt name -> Just (ENum name)
                         other         -> Nothing)
eint :: MyParser Integer
eint = mytoken (\tok -> case tok of 
                         TokenInt name -> Just name
                         other         -> Nothing)

eqOp :: MyParser (Expr a)
eqOp = mytoken (\tok -> case tok of 
                         TokenEq -> Just (EVar "=")
                         other   -> Nothing)

multOp :: MyParser (Expr a)
multOp = mytoken (\tok -> case tok of 
                         TokenTimes -> Just (EVar "*")
                         other      -> Nothing)

divOp :: MyParser (Expr a)
divOp = mytoken (\tok -> case tok of 
                         TokenDiv -> Just (EVar "/")
                         other    -> Nothing)

plusOp :: MyParser (Expr a)
plusOp = mytoken (\tok -> case tok of 
                         TokenPlus -> Just (EVar "+")
                         other     -> Nothing)

minusOp :: MyParser (Expr a)
minusOp = mytoken (\tok -> case tok of 
                         TokenMinus -> Just (EVar "-")
                         other      -> Nothing)

arithOp :: MyParser (Expr a)
arithOp = mytoken (\tok -> case tok of 
                           TokenPlus  -> Just (EVar "+")
                           TokenMinus -> Just (EVar "-")
                           TokenTimes -> Just (EVar "*")
                           TokenDiv   -> Just (EVar "/")
                           other      -> Nothing)

relOp :: MyParser (Expr a)
relOp = mytoken (\tok -> case tok of 
                           TokenGt    -> Just (EVar ">")
                           TokenLt    -> Just (EVar "<")
                           TokenGe    -> Just (EVar ">=")
                           TokenLe    -> Just (EVar "<=")
                           TokenEqual -> Just (EVar "==")
                           other      -> Nothing)

boolOp :: MyParser (Expr a)
boolOp = mytoken (\tok -> case tok of 
                           TokenAnd -> Just (EVar "&")
                           TokenOr  -> Just (EVar "|")
                           other   -> Nothing)

binOp :: MyParser (Expr a)
binOp = do { b <- arithOp
           ; return b
           }
        <|>
        do { b <- relOp
           ; return b
           }
        <|>
        do { b <- boolOp
           ; return b
           }

aexpr :: MyParser (Expr a)
aexpr = do { a <- evar
           ; return a 
           }
        <|> 
        do { a <- enum
           ; return a 
           }
        <|> 
        do { eOP 
           ; e <- expr
           ; eCP
           ; return e
           }

aexprN = do { a <- many1 aexpr
            ; return $ flatExpr a
            }

expr6 :: MyParser (Expr a)
expr6 =  do { e <- aexprN
            ; return e
            }

expr5 :: MyParser (Expr a)
expr5 =  try (expr6 >>= \e1 ->
              multOp >>= \op ->
              expr6 >>= \e2 ->
              return $ EAp (EAp op e1) e2)
         <|>
         try (expr6 >>= \e1 ->
              divOp >>= \op ->
              expr6 >>= \e2 ->
              return $ EAp (EAp op e1) e2)
         <|>
         (expr6 >>= \e ->
          return $ e)

expr4 :: MyParser (Expr a)
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

expr3 :: MyParser (Expr a)
expr3 =  try (expr4 >>= \e1 ->
              relOp >>= \op ->
              expr4 >>= \e2 -> 
              return $ EAp (EAp op e1) e2)
         <|>
         expr4 

-- parseTest expr2 (scanner "x + y * z > c & x > 4")
expr2 :: MyParser (Expr a)
expr2 =  try (expr3 >>= \e1 ->
              boolOp >>= \op ->
              expr2 >>= \e2 -> 
              return $ EAp (EAp op e1) e2)
         <|>
         expr3

expr1 :: MyParser (Expr a)
expr1 = do { e <- many1 expr2
           ; return $ flatExpr e
           }

defn :: MyParser (Name, Expr a)
defn = do { v <- ename
          ; eqOp
          ; e <- expr
          ; return $ (v,e)
          }

--
-- parseTest defns (scanner "x = 3 * 4 ; y = 5 + 3").
--
defns :: MyParser [(Name, Expr a)]
defns = do { d <- sepEndBy1 defn eSemi
           ; return $ d
           }

--
-- parseTest alt (scanner "1 n -> 0").
--
alt :: MyParser CoreAlt
alt = do { n <- eint
         ; v <- many ename 
         ; rightArrow
         ; e <- expr
         ; return $ (n,v,e)
         }

--alts :: MyParser [Alter a]
alts :: MyParser [CoreAlt]
alts = do { d <- sepEndBy1 alt eSemi
          ; return $ d
          }

-- parseTest expr (scanner "let x = 3 * 4 in x * 4;")
expr :: MyParser (Expr a)
expr = do { eLet 
          ; d <- defns
          ; eIn
          ; e <- expr
          ; return $ ELet False d e
          }
       <|>
       do { eLetRec 
          ; d <- defns
          ; eIn
          ; e <- expr
          ; return $ ELet True d e
          }
       <|>
       do { eCase
          ; e <- expr
          ; eOf
          ; a <- alts
          ; return $ ECase e a
          }
       <|>
       do { eIf
          ; t <- expr
          ; eThen
          ; e1 <- expr
          ; eElse
          ; e2 <- expr
          ; return $ EIf t e1 e2
          }
       <|>
       do { eLambda
          ; vars <- many1 ename
          ; rightArrow
          ; e <- expr
          ; return $ ELam vars e
          }
       <|>  
       do { e <- expr1
          ; return e
          }

--
-- parseTest sc (scanner "add x y = x + y")
--
{-
sc :: MyParser CoreScDefn
sc = do { (EVar name) <- evar 
        ; vars <- many evar
        ; eqOp
        ; e <- expr
        ; return $ (name, map (\(EVar v) -> v) vars, e)
        }
-}

--
-- parseTest sc (scanner "add (x:xs) (y:ys) = x + y")
--
sc :: MyParser CoreScDefn
sc = do { (name,apats) <- funlhs
        ; eqOp
        ; e <- expr
        ; return $ (name, apats, e)
        }


--
-- parseTest funlhs (scanner "sum (x:xs)")
--
funlhs :: MyParser (Name,[APat])
funlhs = do { (EVar name) <- evar
            ; apats <- many1 apat
	    ; return $ (name, apats)
	    }

--
-- parseTest apat (scanner "(x:xs)")
-- parseTest apat (scanner "[]")      only allow an empty list for now!
-- parseTest apat (scanner "[x:xs,y]")
-- parseTest apat (scanner "[x:(z:xs),y]")
--
apat :: MyParser APat
apat = try (do { eOP
               ; p <- pat
               ; eCP
       	       ; return $ p
	       })
       <|> 
       try (do { eOB
               ; a <- apatList
               ; eCB
       	       ; return $ a
	       })
       <|> 
       try (do { (EVar var) <- evar
       	       ; return $ EPvar var
	       })

apatList :: MyParser APat
apatList = try (do { p1 <- pat
                   ; eComma
                   ; p2 <- apatList
       	           ; return $ ECons p1 p2
	           })
           <|> 
           try (do { p1 <- pat
       	           ; return $ ECons p1 ENil
	           })
           <|> 
           return ENil

pat :: MyParser APat
pat = try (do { p1 <- apat
              ; eCons
              ; p2 <- apat
       	      ; return $ ECons p1 p2
	      })
      <|> 
      try (do { (EVar var) <- evar
       	      ; return $ EPvar var
	      })


--
-- parseTest program (scanner "add x y = x + y; mul x z = z*x;")
--
program :: MyParser CoreProgram
program = do { p <- sepEndBy1 sc eSemi
             ; return $ p
             }


--
-- parseTest corePgm (scanner "module add where add (x:xs) (y:ys) = x + y; mul x z = z*x;")
-- parseTest corePgm (scanner "module abc where add (x:xs) (y:ys) = x + y; sum [] = 0; sum (x:xs) = x + sum xs; mul x z = z*x;")
--
corePgm :: MyParser CoreModule
corePgm = do { eModule
             ; modName <- ename
             ; eWhere
             ; pgm <- program
             ; return $ (modName, (groupBy sortByClause pgm))
             }

sortByClause :: CoreScDefn -> CoreScDefn -> Bool
sortByClause (name1,_,_) (name2,_,_) | name1 == name2 = True
sortByClause _ _ = False
