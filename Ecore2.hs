--
-- Created: 9 Jul 2007 by tobbe@tornkvist.org
--

module Ecore2 where

import Text.ParserCombinators.Parsec
--import Data.Char (toUpper) 
import Lexer2
import Parser2
import Ebif2
import Bkeep2

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
-- Test: pgm (parser "module add where add x y = x + y")
--  pgm (parser "module add where gt x y = if (x > y) then 1 else 0")
--  pgm (parser "module sum where sum xs = if (length(xs) == 0) then 0 else (1 + sum (tl xs))

{-

--- Test Cases ---

 parser "add x y = x+y"
 [("add",["x","y"],EAp (EAp (EVar "+") (EVar "x")) (EVar "y"))]

 depth t = case t of 1 n -> 0 ; 2 t1 t2 -> 1 + max (depth t1) (depth t2)

 quad x = let twice = x+x in twice + twice

-}

type St a = [(Expr a)]
type Stack = St Name

type Args = [String]

data Erl
    = MFA Mod Fun Args
    | FA Fun Args
    | LAM String
    | Bonkers
      deriving (Show)

--
-- Test: pgm (parser "module add where add x y = x + y")
--
pgm :: [CoreModule] -> String
pgm [(modName,cp)] 
    = let cp2 = map head cp
          bk = mkSymtab cp2 bkNew
      in
        let mhead = implode ((concatMap mkMhead cp2) ++ ["'module_info'/0","'module_info'/1"]) ","
            mdefs = concat (map (\d -> genSc d bk) cp)
            minfo = modInfo modName
        in
          "module '"++modName++"' ["++mhead++"]\nattributes []\n"++mdefs++"\n"++minfo++"\nend\n"


--
-- Setup the Symbol Table with the info of all locally defined (F/N) functions.
--
mkSymtab :: CoreProgram -> Bkeep -> Bkeep
mkSymtab [] bk = bk
mkSymtab ((name,vs,_):xs) bk = 
    mkSymtab xs (bkSyAdd (Sfa name (length vs)) bk)


--
-- Create the list of exported functions.
--
mkMhead :: CoreScDefn -> [String]
mkMhead (name,vars,_) = mkMhead1 name vars

mkMhead1 :: String -> [APat] -> [String]
mkMhead1 name [] = ["'"++name++"'/0"]
mkMhead1 name args = 
    let x = "'"++name++"'/"++(show $ length args)
        xs = mkMhead1 name (tail args)
    in 
      x:xs
    

--
-- Generate the Ecore for each function definition.
--
genSc :: [CoreScDefn] -> Bkeep -> String
genSc d@((name,aps,_):_) bk 
    = let vlen = length aps
          cfuns = mkCurry name (vlen - 1) (bkNew)
          invars = ["_cor"++(show j) | j <- [1..vlen]]
          cgen = scGen d bk invars []
      in
        cfuns++"\n\n"++cgen

-- Test: scGen ("add",["x","y"],EAp (EAp (EVar "+") (EVar "x")) (EVar "y")) []
scGen :: [CoreScDefn] -> Bkeep -> [String] -> Stack -> String
scGen d@((name,_,_):_) bk invars st 
    = let fhead = mkHead name invars
          fbody = scClause d bk st
          ftail = mkTail bk
      in
        fhead++"\n"++fbody++"\n"++ftail++"\n"

scClause :: [CoreScDefn] -> Bkeep -> Stack -> String
scClause ((name,_,_):xs) bk st
    = let x = "TO BE DONE"
      in 
        -- 
        -- HERE WE MUST CENERATE THE PATTERN IN THE CLAUSE HEAD 
        -- BEFORE CALLING (see zip.core):
        -- exprGen expr st bk
        "tbd"

--
-- Make the head of the function. 
--
mkHead :: String -> [String] -> String
mkHead name invars  
    = let vs = implode invars ","
      in
        "'"++name++"'/"++(show $ length invars)++" = "++
               "fun("++vs++") ->\n"++
               "  case <"++vs++"> of \n"

--
-- Make the end of the function, i.e the case expression.
--
mkTail :: Bkeep -> String
mkTail bk
    = let (i,bk2) = bkBump bk
          v = "_v" ++ (show i)
      in 
        "<"++v++"> when 'true' ->\n" ++
               "  primop 'match_fail'\n" ++
	       "    ({'function_clause',"++v++"})\n" ++
               "end\n\n"
                                                     
                            
mkUvars = map ("_"++)

--
-- exprGen (EAp (EAp (EVar "+") (EVar "x")) (EVar "y")) []
-- " call 'erlang':'+'(X,Y)"
--
exprGen :: (Expr Name) -> Stack -> Bkeep -> String
exprGen (ENum n) st bk = (show n)
exprGen (EVar v) st bk = "_"++v
exprGen (EAp e1 e2) st bk = 
    case (buildAp e1 (e2:st) bk) of
      (MFA m f a) ->
          let args = implode a ","
          in " call '"++m++"':'"++f++"'("++args++")"
      (FA f a) ->
          case (bkSyFun f bk) of
            Just (Sfa f arity) ->
                let args = implode a ","
                in " apply '"++f++"'/"++(show arity)++"("++args++")"
            Nothing ->
                "what do we do here ?"
      (LAM lam) ->
          let free = findFree e2
              exp2 = exprGen e2 [] bk
          in 
            " let "++free++" = \n "++lam++" \n in apply "++free++
                       "\n("++exp2++")\n"
exprGen (EIf t e1 e2) st bk = 
    let test = ifTestGen t [] bk
        x1   = exprGen e1 [] bk
        x2   = exprGen e2 [] bk
    in
      "case <> of\n <> when\n"++test++x1++"\n<> when 'true' ->\n"++x2++"\nend\n"
exprGen (ELet recP defs body) st bk = 
    let lets = letGens recP defs bk
        exps = exprGen body [] bk
    in
      lets ++ exps
exprGen (ELam vars expr) st bk = 
    let uvars = mkUvars vars
        body = exprGen expr [] bk
    in
      " fun ("++(implode uvars ",")++") ->\n"++body
exprGen e st bk = 
    "not implemented"

letGens :: Bool -> [(Name, Expr Name)] -> Bkeep -> String
letGens recP [] bk = ""
letGens recP ((n,e):defs) bk =
    let expr = exprGen e [] bk
        xlet = whichLet recP
    in 
      xlet++" _"++n++" = "++expr++" in "++(letGens recP defs bk)

whichLet :: Bool -> String      
whichLet False = "let"
whichLet True  = "letrec"

  
ifTestGen :: (Expr Name) -> Stack -> Bkeep -> String
ifTestGen expr st bk =
    "try\n" ++ (exprGen expr st bk) ++ "\n" ++
    " of <Try> ->\n Try\n catch <T,R> ->\n 'false' ->\n"

-- 
-- buildAp (EAp (EAp (EVar "+") (EVar "x")) (EVar "y")) []
--
buildAp :: (Expr Name) -> Stack -> Bkeep -> Erl
buildAp (EAp e1 e2) st bk = buildAp e1 (e2:st) bk
buildAp expr st bk = 
    case expr of
      (EVar func) ->
          case isBif func of
            Just (m,f,r) ->
                let vs = map (\x -> exprGen x [] bk) (take r st)
                in
                  MFA m f vs
            Nothing ->
                case (bkSyFun func bk) of
                  Just (Sfa func r) ->
                      let vs = map (\x -> exprGen x [] bk) (take r st)
                      in
                        FA func vs
                  Nothing ->
                      Bonkers   -- FIXME !?
      (ELam vars expr) ->
          let lam = exprGen (ELam vars expr) [] bk
          in 
            LAM lam


findFree :: (Expr a) -> String
findFree e = "_tnt0"   -- FIXME find free variable in expression


implode :: [String] -> String -> String
implode ws sep = concat $ interleave ws sep
 
interleave :: [String] -> String -> [String]
interleave [] _     = []
interleave [x] _    = [x]
interleave (x:xs) s = x:s:(interleave xs s)


--
-- Turn a function declaration into a 'curried' version
-- For example: "add x y" ,should become (in pseudo Erlang):
--
--   add() -> \x -> add(x)
--   add(x) -> \y -> add(x,y)
--
-- Test:  mkCurry "add" 2 (bkNew)
--
mkCurry :: String -> Int -> Bkeep -> String
mkCurry _ n _ | n < 0 = []
mkCurry name n bk =
    let (i,bk2) = bkBump bk
        v = "_cor"++(show i)
        invars = ["_v"++(show j) | j <- [1..n]]
        v1 = implode invars ","
        v2 = implode (invars ++ [v]) ","
        h = "'"++name++"'/"++(show n)++" = fun ("++v1++") -> "++
            "fun (_"++v++") -> apply '"++name++"'/"++(show (n+1))++" ("++v2++")\n"
        t = mkCurry name (n-1) bk2 
    in
      h ++ t


-- 
-- Implode a list of variable names: ["x","y"] => "x,y"
--
mkVars :: [String] -> Integer -> String
mkVars  _ 0      = []
mkVars  (x:_) 1  = "_"++x
mkVars  (x:xs) n = "_"++x++","++(mkVars xs (n-1))



modInfo :: String -> String
modInfo m 
    = "'module_info'/0 = fun () -> call 'erlang':'get_module_info' ('"++m++"')\n"++
      "'module_info'/1 = fun (_cor0) ->	call 'erlang':'get_module_info' ('"++m++"', _cor0)"


      