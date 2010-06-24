--
-- Created: 9 Jul 2007 by tobbe@tornkvist.org
-- Desc: Transform the parser output into the form that Epretty.hs expects.
--

module Ecore3 where

import Data.Char
import Text.ParserCombinators.Parsec
import Ebif2
import Bkeep2
import Parser3

type EAtom = String                       -- Atom

type EVars = [String]                     -- List of variable names

type Fname = (String,Arity)               -- Function name

data EFname = EFname Fname                -- Function name
    deriving (Show)

data EMod                                 -- Module definition
    = EMod EAtom [Fname] [EFdef]
    deriving (Show)

data EFdef                                -- Named Function definition
    = EFdef EFname EFun
    deriving (Show)

data EFun                                 -- Local Function definition
    = EFun EVars EXpr                     -- FIXME should it be [EXpr] ?
    deriving (Show)

data EClause                              -- Clause definition
    = EClause EPats [EXpr] [EXpr]
    deriving (Show)

type EClauses = [EClause]                 -- List of clauses

type EPats = [EPat]                       -- List of patterns

data EXpr                                 -- Expression
    = EXnil
    | EXvar String
    | EXnum Integer
    | EXpat EPat
    | EXatom EAtom
    | EXcons EXpr EXpr
    | EXtuple [EXpr]
    | EXfun EFun
    | EXfname Fname
    | EXfatbar String
    | EXfatbar2 String
    | EXcase EVars EClauses
    | EXlet EVars [EXpr] EXpr
    | EXletRec EVars [EXpr] EXpr
    | EXapply EXpr [EXpr]
    | EXprimop EAtom [EXpr]
    | EXcall EXpr EXpr [EXpr]
    deriving (Show)


--
-- Transform the TNT module into an Erlang Core module.
--
tm :: CoreModule -> EMod
tm (m,cps) =
    let cps' = myFilter cps
        cps'' = cps' ++ [(fatbar), (fatbar2), (fatbar3)]     -- FIXME only add if guards are existing
        fnames = [ (cpFname cp, cpArity cp) | cp <- cps'']
        cfuns = concatMap mkCurryFuns fnames
        bk = mkSymtab (bkNew) cps''                 -- FIXME add cfuns functions to symtab ?
        fdefs = (map (td bk) cps'') ++ cfuns
    in
      EMod (lcase m) fnames fdefs


myFilter []                 = []
myFilter ([Tsign]:xs)       = myFilter xs
myFilter ([ETcon _ _ _]:xs) = myFilter xs
myFilter (x:xs)             = x:(myFilter xs)

--
-- Generate curried versions of the original function.
--
mkCurryFuns :: (String,Arity) -> [EFdef]
mkCurryFuns (_,0) = []
mkCurryFuns (fname,arity+1) = map (mkCurryFun fname) [0..arity]

mkCurryFun :: String -> Int -> EFdef
mkCurryFun fname arity =
    let (vs1,bk) = mkCoreVars arity ([],(bkNew))
        (vs2,bk') = mkCoreVars 1 ([],bk)
        vs3 =  vs1 ++ vs2
    in
      EFdef 
      (EFname (fname,arity))
      (EFun vs1
       (EXfun 
        (EFun vs2 
         (EXapply 
          (EXfname (fname,arity+1)) 
          [EXvar v | v <- vs3]))))
    

--
-- Transform a function definition
--
td :: Bkeep -> CoreProgram -> EFdef
td bk cp =
    let fname = cpFname cp
        arity = cpArity cp
        fundef = tf bk arity cp
    in
      EFdef (EFname (fname,arity)) fundef


--
-- Transform each clause of the function def.
--
tf :: Bkeep -> Int -> CoreProgram -> EFun
tf bk arity cp | isUsingGuards cp = 
    let (vars,bk') = mkCoreVars arity ([],bk)
        fbody = tclsG bk' arity vars cp
    in
      EFun vars fbody
tf bk arity cp = 
    let (vars,bk') = mkCoreVars arity ([],bk)
        clauses = map (tcl bk') cp
    in
      EFun vars (EXcase vars clauses)

isUsingGuards :: CoreProgram -> Bool
isUsingGuards []                        = False
isUsingGuards ((Decl (_, _, [], _)):xs) = isUsingGuards xs
isUsingGuards _                         = True

--
-- Transform a clause of a function def. (for funcs. without any guards)
--
tcl :: Bkeep -> TopDecl -> EClause
tcl bk (Decl (_, pats, _, expr)) =
    let ps = [epatUcase bk p | HPat p <- pats]
        body = te bk expr
    in
      EClause ps [EXatom "true"] [body]

--
-- Transform a function definition containing guard expressions.
--
--   For each clause, check if the argument pattern matches
--   and if the guard evaluates to 'true'. For the first clause
--   that fulfills these conditions, evaluate the body of
--   that clause. 
--
tclsG :: Bkeep -> Int -> EVars -> CoreProgram -> EXpr
--tclsG bk arity vs cp = EXnil              -- FIXME
tclsG bk arity vs cp = 
    let ([v1,v2,v3],bk') = mkCoreVars 3 ([],bk)
        gs = map (tg bk' arity) cp
        gs' = foldr (\e es -> EXcons e es) EXnil gs
        evs = foldr mkCons EPnil [(EPvar w) | w <- vs]
    in (EXlet [v1] 
        [EXlet [v2, v3] [EXpat evs, gs']
         (EXapply (EXvar "'_fatbar'/2") [EXvar v2, EXvar v3])]
        (EXapply (EXvar v1) []))

mkCons e es = (EPcons e es)


--
-- For each clause, check if it matches the argument and that
-- its guards evaluates to True.
--
-- fatbar _ []      = ErrorPrimop;
-- fatbar [] (x:_)  = x;
-- fatbar vs (x:xs) = fatbar2 vs (fatbar3 vs x) xs;
-- 
-- fatbar2 _  _ []         = ErrorPrimop;
-- fatbar2 _ (True,fun) _  = fun;
-- fatbar2 vs False (x:xs) = fatbar2 vs (fatbar3 vs x) xs;
-- 
-- fatbar3 [] x     = x;
-- fatbar3 (v:vs) x = fatbar3 vs (x v);
--
fatbar :: CoreProgram
fatbar =
    [Decl ("_fatbar",[HPat EPdcare,HPat EPnil],[],
                    EAp (EVar "error") (HPat (EPatom "match_fail"))),
     Decl ("_fatbar",[HPat EPnil,HPat (EPcons (EPvar "x") EPdcare)],[],
                    HPat (EPvar "x")),
     Decl ("_fatbar",[HPat (EPvar "vs"),HPat (EPcons (EPvar "x") (EPvar "xs"))],[],
                    EAp (EAp (EAp (HPat (EPvar "_fatbar2")) (HPat (EPvar "vs"))) 
                         (EAp (EAp (HPat (EPvar "_fatbar3")) 
                               (HPat (EPvar "vs"))) (HPat (EPvar "x")))) 
                            (HPat (EPvar "xs")))]

--                         (EPapply "x" ["vs"]))

fatbar2 :: CoreProgram
fatbar2 = 
    [Decl ("_fatbar2",[HPat EPdcare,HPat (EPtuple [EPtcon "True" [],EPvar "fun"]),HPat EPdcare],[],
                     HPat (EPvar "fun")),
     Decl ("_fatbar2",[HPat EPdcare,HPat EPdcare,HPat EPnil],[],
                     EAp (EVar "error") (HPat (EPatom "match_fail"))),
     Decl ("fatbar2",[HPat (EPvar "vs"),HPat (EPtcon "False" []),HPat (EPcons (EPvar "x") (EPvar "xs"))],[],
                    EAp (EAp (EAp (HPat (EPvar "_fatbar2")) (HPat (EPvar "vs"))) 
                         (EAp (EAp (HPat (EPvar "_fatbar3")) (HPat (EPvar "vs"))) (HPat (EPvar "x")))) 
                            (HPat (EPvar "xs")))]

{-
     Decl ("_fatbar2",[HPat (EPvar "vs"),HPat (EPtcon "False"),HPat (EPcons (EPvar "x") (EPvar "xs"))],[],
                     EAp (EAp (EAp (HPat (EPvar "_fatbar2")) (HPat (EPvar "vs")))
                          (EAp (EAp (HPat (EPvar "_fatbar3")) (HPat (EPvar "vs"))) (HPat (EPvar "x")))) 
                             (HPat (EPvar "xs")))]
-}

--                                  (EPapply "x" ["vs"]))

fatbar3 :: CoreProgram
fatbar3 = 
    [Decl ("_fatbar3",[HPat EPnil,HPat (EPvar "x")],[],
                     HPat (EPvar "x")),
     Decl ("_fatbar3",[HPat (EPcons (EPvar "v") (EPvar "vs")),HPat (EPvar "x")],[],
                     EAp (EAp (HPat (EPvar "_fatbar3")) (HPat (EPvar "vs"))) 
                             (EAp (HPat (EPvar "x")) (HPat (EPvar "v"))))]

copies _ 0 = []
copies e n = e:(copies e (n - 1))



-- Generate a function that can test if the pattern matches and 
-- if the guard evaluates to true. 
--
tg :: Bkeep -> Int -> TopDecl -> EXpr
tg bk arity (Decl (_, pats, [], expr)) =
    let ps = [epatUcase bk p | HPat p <- pats]
        (vars,bk') = mkCoreVars arity ([],bk)
        (v2,bk'') = mkCoreVars 1 ([],bk')
        xs = te bk'' expr 
        dcares = copies EPdcare arity
        true = (EXatom "true")
        body = (EXcase vars 
                [(EClause ps [true]
                  [(EXtuple [true, (EXfun (EFun [] xs))])]),
                 (EClause dcares [true] 
                  [(EXatom "false")])])
    in
      tgWrap vars body
tg bk arity (Decl (_, pats, [guard], expr)) =
    let ps = [epatUcase bk p | HPat p <- pats]
        (vars,bk') = mkCoreVars arity ([],bk)
        (v2,bk'') = mkCoreVars 1 ([],bk')
        gs = te bk'' guard 
        xs = te bk'' expr 
        true = (EXatom "true")
        body = (EXcase vars 
                [(EClause ps [true]
                  [EXlet v2 [gs]
                   (EXcase v2 
                    [(EClause [(EPatom "true")] [true]
                      [(EXtuple [true, (EXfun (EFun [] xs))])]),
                     (EClause [EPdcare] [true] [(EXatom "false")])])]),
                 (EClause [EPdcare] [true] [(EXatom "false")])])
    in
      tgWrap vars body

tgWrap :: EVars -> EXpr -> EXpr
tgWrap [] body = body
tgWrap (v:vs) body =
    let b = tgWrap vs body
    in
      EXfun (EFun [v] b)
          
{-
      EXfun 
      (EFun vars 
       (EXcase vars 
        [(EClause ps [true]
          [EXlet v2 [gs]
           (EXcase v2 
            [(EClause [(EPatom "true")] [true]
              [(EXtuple [true, (EXfun (EFun [] xs))])]),
             (EClause [EPdcare] [true] [(EXatom "false")])])]),
         (EClause [EPdcare] [true] [(EXatom "false")])]))
-}        


--
-- Transform Expressions
--
te :: Bkeep -> Hxpr -> EXpr
te bk (ENum n) = EXnum n

te bk (ELet False defs expr) =
    let vs = [ucase . fst $ v | v <- defs]
        xs = map (te bk) [snd x | x <- defs]
        ex = te bk expr
    in
      EXlet vs xs ex
te bk (EIVar v) = EXvar v

te bk x@(EVar v) =  tv bk x

te bk x@(HPat p) = toELit bk x

te bk (ELam vs e) = 
    let expr = te bk e
    in EXfun (EFun (map ucase vs) expr)

te bk (EIf b e1 e2) = 
    let (vars,bk') = mkCoreVars 1 ([],bk)
        bx = te bk' b
        x1 = te bk' e1
        x2 = te bk' e2
        c1 = EClause [EPatom "true"] [EXatom "true"] [x1]
        c2 = EClause [EPdcare] [EXatom "true"] [x2]
    in EXlet vars [bx] (EXcase vars [c1,c2])

te bk (EPrimop name e) = 
    let expr = map (te bk) e
    in EXprimop name expr

te bk (EPapply name vs) = 
    let evs = [EXvar (ucase v) | v <- vs]
    in EXapply (EXvar (ucase name)) evs

te bk (EAp op e2) | isOp op = 
                      let Just (m,f,a) = isBif (var op)
                          (vars,bk') = mkCoreVars a ([],bk)
                          evs = [(EXvar v) | v <- vars]
                          bif = EXcall (EXatom m) (EXatom f) evs
                          fun = mkCurried vars bif
                          ([v1,v2],bk'') = mkCoreVars 2 ([],bk')
                          x2 = te bk'' e2
                      in
                        (EXlet [v1,v2] [x2,fun] (EXapply (EXvar v2) [EXvar v1]))

te bk (EAp f e2) | isFun bk f = 
                      let ([v1],bk') = mkCoreVars 1 ([],bk)
                          x2 = te bk' e2
                          Just (Sfa fname _) = bkSyFun (var f) bk
                          fun = EXfname (fname,1)
                      in
                        EXlet [v1] [x2] (EXapply fun [(EXvar v1)])

te bk (EAp v1 v2) | isVar bk v1 && isVar bk v2 = 
                      EXapply (EXvar (maybeUcase bk v1)) 
                                  [(EXvar (maybeUcase bk v2))]

te bk (EAp v e2) | isVar bk v = 
                     let ([v1],bk') = mkCoreVars 1 ([],bk)
                         x2 = te bk' e2
                         var = EXvar (maybeUcase bk v)
                      in
                        EXlet [v1] [x2] (EXapply var [(EXvar v1)])

te bk (EAp e1 e2) = 
    let (vars,bk') = mkCoreVars 1 ([],bk)
        v1 = head vars
        x1 = te bk' e1
        x2 = te bk' (EAp (EIVar v1) e2)
    in
      EXlet vars [x1] x2


--
-- Transform an EVar token
--
tv :: Bkeep -> (Hxpr) -> EXpr
tv bk x@(EVar v) | isFun bk x = 
                     EXapply (EXfname (v,0)) [] 
tv bk x@(EVar v) | isOp x =  
                     let (vars,bk') = mkCoreVars 2 ([],bk)
                         evs = [(EXvar w) | w <- vars]
                         (mod,fun) = mf v
                         bif = EXcall (EXatom mod) (EXatom fun) evs
                     in
                       mkCurried vars bif
tv bk (EVar v) =  
    EXvar $ ucase $ v  


mf :: String -> (String,String)
mf v = case (isBif v) of
         Just (m,f,a) -> (m,f)
         Nothing      -> ("erlang",v)


mkCurried :: [String] -> EXpr -> EXpr
mkCurried [] fun = fun
mkCurried (v:vs) fun = EXfun (EFun [v] (mkCurried vs fun))
    

var :: (Hxpr) -> String
var (EVar v)         = v
var (HPat (EPvar v)) = v
    
  
--
-- How to generate code for a BIF
--
-- EAp "+" E2
-- 
-- let a = E2
-- z = \x -> \y -> erlang:'+'(x,y)
-- apply z a
--
-- z = \x -> \y -> erlang:'+'(x,y)
-- let z = EXfun (EFun ["x"] (EXfun (EFun ["y"] (EXcall 'erlang' '+' ["x","y"]))))
--
--
-- How to generate code for a local function
--
-- EAp f E2
-- 
-- let a = E2
-- apply f a

toELit :: Bkeep -> (Hxpr) -> EXpr
toELit _ (EVar v)  = EXvar v
toELit _ (ENum n)  = EXnum n
toELit bk (HPat (EPvar v)) = tv bk (EVar v) 
toELit bk (HPat p) = EXpat (epatUcase bk p)


epatUcase :: Bkeep -> EPat -> EPat
epatUcase _ EPnil           = EPnil
epatUcase bk (EPvar v)      = EPvar (if (isFun bk (EVar v)) then v else ucase $ v)
epatUcase _ x@(EPnum _)     = x
epatUcase _ x@(EPatom _)    = x
epatUcase _ EPdcare         = EPdcare
epatUcase bk (EPtcon c eps) = EPtcon c (map (epatUcase bk) eps)
epatUcase bk (EPtuple eps)  = EPtuple (map (epatUcase bk) eps)
epatUcase bk (EPcons x xs)  = EPcons (epatUcase bk x) (epatUcase bk xs) 

               
-- FIXME lookup if the function is known in the symtab instead!!         
isFun :: Bkeep -> (Hxpr) -> Bool
isFun bk h | isVar2 h  = 
               let v = getVar h
               in
                 case (bkSyFun v bk) of
                   Nothing -> False
                   other   -> True
isFun _ _ = False

getVar :: (Hxpr) -> String
getVar (EVar v)         = v
getVar (EIVar v)        = v
getVar (HPat (EPvar v)) = v

maybeUcase :: Bkeep -> (Hxpr) -> String
maybeUcase bk x@(EVar v) = if (isFun bk x) then v else ucase $ v
maybeUcase bk x@(HPat (EPvar v)) = if (isFun bk x) then v else ucase $ v
maybeUcase _ (EIVar v)   = v

isVar :: Bkeep -> (Hxpr) -> Bool
isVar bk x = isVar2 x && (not (isFun bk x))

isVar2 :: Hxpr -> Bool
isVar2 (EVar _)          = True
isVar2 (EIVar _)         = True
isVar2 (HPat (EPvar _))  = True
isVar2 _                 = False

isLit :: Hxpr -> Bool
isLit (EVar _) = True
isLit (ENum _) = True
isLit (HPat _) = True
isLit _        = False

primOps :: [String]
primOps = ["+","-","*","/",">","<",">=","<=","==","=/=","=:=","/=","&&","||","error"]

isOp :: Hxpr -> Bool
isOp (EVar op) = elem op primOps
isOp _ = False


mkCoreVars :: Int -> ([String],Bkeep) -> ([String],Bkeep)
mkCoreVars 0 (vars,bk) = (reverse vars,bk)
mkCoreVars arity (vars,bk) | arity > 0 =  
    let (i,bk') = bkBump bk
        var = "_cor" ++ (show i)
    in
      mkCoreVars (arity - 1) (var:vars, bk')


cpFname :: CoreProgram -> String 
cpFname cp = let (Decl p) = head cp
             in fst3 p


cpArity :: CoreProgram -> Int 
cpArity cp = let (Decl p) = head cp
             in length . snd3 $ p


fst3 :: (a,b,c,d) -> a
fst3 (x,_,_,_) = x


snd3 :: (a,b,c,d) -> b
snd3 (_,x,_,_) = x

ucase :: String -> String
ucase = map toUpper

lcase :: String -> String
lcase = map toLower


--
-- Setup the Symbol Table with the info of all locally defined (F/N) functions.
--
mkSymtab :: Bkeep -> [CoreProgram] -> Bkeep
mkSymtab bk [] = bk
mkSymtab bk (cp:cps) =
    let bk' = mkSymtab2 bk cp
    in mkSymtab bk' cps

mkSymtab2 :: Bkeep -> CoreProgram -> Bkeep
mkSymtab2 bk [] = bk
mkSymtab2 bk ((Decl (name,vs,_,_)):xs) = 
    mkSymtab2 (bkSyAdd (Sfa name (length vs)) bk) xs


--
-- To test: parser "x"
--
parser input = let scanres = scanner input
                   pgmres = runParser corePgm () ""
               in
                 case (pgmres scanres) of
                   Left _  -> []
                   Right r -> [r]


--
-- Test cases
--

tcAll = map tm [iSingle, iSum, iZip, iZip2, iSingle2, iSum2, isNull, 
                iSafeHead, iMap, iTsign, iTsign2, iOp, iOp2, iLast,
                iLast2]

c1 = tm iSingle
c2 = tm iSum
c3 = tm iZip
c4 = tm iZip2
c5 = tm iSingle2
c6 = tm iSum2
c7 = tm isNull
c8 = tm iSafeHead
c9 = tm iMap
c10 = tm iOp
c11 = tm iOp2
c12 = tm iGuard
c13 = tm iGuard2
c14 = tm iTuple
c15 = tm iFatbar
c16 = tm iGuard3
c17 = tm iGuard4
c18 = tm iTwo
c19 = tm iError
c20 = tm iFatbar2

b1 = dumpSymtab iSingle2
b11 = dumpSymtab iOp2

dumpSymtab :: CoreModule -> Bkeep
dumpSymtab (_,cps) = mkSymtab (bkNew) cps


--
-- parseTest corePgm (scanner "module zip where zip [] [] = []; zip (x:xs) (y:ys) = let z = x+y;zs = zip xs ys in (z:zs)")
--
iZip = head . parser $ "module Zip where zip [] [] = []; zip (x:xs) (y:ys) = let z = x+y;zs = zip xs ys in (z:zs);"

iZip2 = head . parser $ "module Zip where zip _ [] = []; zip [] _ = []; zip (x:xs) (y:ys) = let z = x+y;zs = zip xs ys in (z:zs);"

iSum = head . parser $ "module Sum where sum [] = 0;sum (x:xs) = let z = sum xs in x + z;"

iSum2 = head . parser $ "module Sum where sum [] = 0;sum (x:xs) = x + sum xs;"

iBool = head . parser $ "module Bool where data Bool = True | False; add x y = x + y;"

iSingle = head . parser $ "module Single where one = 1;"

iSingle2 = head . parser $ "module Single where one = 1; two = one + one;"

iLam = head . parser $ "module Lam where id = \\x -> x; plus1 x = (\\y -> y +1) x;"

iGt =  head . parser $ "module Gt where gt x y = if (x > y) then 1 else 0;"

isNull = head . parser $ "module Null where null [] = True; null _ = False;"

isMaybe = head . parser $ "module Maybe where data Maybe a = Nothing | Just a;"

iSafeHead = head . parser $ "module SafeHead where safehead [] = Nothing; safehead (x:_) = Just x;"

iMap = head . parser $ "module Map where map f [] = []; map f (x:xs) = let z = f x; zs = map f xs in (z:zs);"

iTsign = head . parser $ "module Tsign where head :: [a] -> a;"

iTsign2 = head . parser $ "module X where head :: [a] -> a;head (x:_) = x;"

iOp = head . parser $ "module X where sum xs = foldl (+) 0 xs;"

iOp2 = head . parser $ "module X where plus = (+);"

iLast = head . parser $ "module X where last [x] = x;last (_:xs) = xs;"

iLast2 = head . parser $ "module X where last [x] = x;"

iGuard = head . parser $ "module X where procString xs | isString xs = True;"

iGuard2 = head . parser $ "module X where isString (x:xs) | x >= 0 && x <= 255 = isString xs;"

iGuard3 = head . parser $ "module X where isZero x | x == 0 = True; isZero _ = False;"

iGuard4 = head . parser $ "module X where gt x y | x > y = True; gt _ _         = False;"

iTuple =  head . parser $ "module X where mkTuple x y = (x,y);"
 
iApply =  head . parser $ "module X where eee x y z = fff x y z;"

iApply2 =  head . parser $ "module X where xx = 1; yy = 2; analyze = fatbar 34 xx;"

iFatbar =  head . parser $ "module Fatbar where fatbar _ [] = Primop;fatbar [] (x:_)  = x; fatbar vs (x:xs) = fatbar2 vs (fatbar3 vs x) xs;fatbar2 _  _ []  = Primop; fatbar2 _ (True,fun) _   = fun;fatbar2 vs  False (x:xs) = fatbar2 vs (fatbar3 vs x) xs;fatbar3 [] x = x; fatbar3 (v:vs) x = fatbar3 vs (x v);"

iFatbar2 = head . parser $ "module X where fatbar2 vs False (x:xs) = fatbar2 vs (fatbar3 vs x) xs;"

iList =   head . parser $ "module X where xx = [1,2,3];"

iTwo = head . parser $ "module X where mul x y = x * y; double x = mul x x;"

iError = head . parser $ "module X where eee x = error x;"



