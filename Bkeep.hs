--
-- Created: 14 Aug 2007 by tobbe@tornkvist.org
--
--   ADT + Ops. to maintain various kind of information.
--

module Bkeep where

import Data.Bits

type Name = String
type Mod = String
type Fun = Name
type Arity = Int

--
-- Variable counter. For creating internal vaiables.
--
type Vcnt = Int

--
-- Simple symbol table
--
data Stab = Sfa Fun Arity
            deriving (Show)
type Symtab = [Stab]

--
-- String (space) prefix when generating code
--
type Prefix = String

--
-- Bit flags field.
--
type Flags = Int

--
-- Data structure for bookeeping
--
data Bkeep = Bkeep Vcnt Symtab Prefix Flags
             deriving (Show)


bkNew :: Bkeep
bkNew = Bkeep 0 [] "" 0

--
-- Bump the variable counter
--
bkBump :: Bkeep -> (Int, Bkeep)
bkBump (Bkeep i s p f) =
    let j = i + 1
    in (j, Bkeep j s p f)

--
-- Increase the prefix
--
bkInc :: Bkeep -> Bkeep
bkInc (Bkeep i s p f) = Bkeep i s ("  " ++ p) f

--
-- Decrease the prefix
--
bkDec :: Bkeep -> Bkeep
bkDec (Bkeep i s p f) | length p < 2 = Bkeep i s p f
bkDec (Bkeep i s (_:_:p) f) = Bkeep i s p f

--
-- Return current prefix
--
bkPix :: Bkeep -> String
bkPix (Bkeep _ _ p _) = p

--
-- Remove current prefix
--
bkPix0 :: Bkeep -> Bkeep
bkPix0 (Bkeep i s _ f) = Bkeep i s "" f

--
-- Add entry to Symbol Table
--
bkSyAdd :: Stab -> Bkeep -> Bkeep
bkSyAdd x (Bkeep i syb p f) = Bkeep i (x:syb) p f

--
-- Get entry from Symbol Table
--
bkSyFun :: String -> Bkeep -> Maybe Stab
bkSyFun name (Bkeep _ [] _ _) =
    Nothing
bkSyFun name (Bkeep _ ((Sfa n1 arity):xs) _ _) | name == n1 =
    Just $ Sfa n1 arity
bkSyFun name (Bkeep i (_:xs) p f) =
    bkSyFun name (Bkeep i xs p f)

--
-- Set bit flags
--

fInsideGuard :: Int
fInsideGuard = 1

bkSetInsideGuard :: Bkeep -> Bkeep
bkSetInsideGuard (Bkeep i syb p f) = Bkeep i syb p (f `setBit` fInsideGuard)

bkClrInsideGuard :: Bkeep -> Bkeep
bkClrInsideGuard (Bkeep i syb p f) = Bkeep i syb p (f `clearBit` fInsideGuard)

bkIsInsideGuard :: Bkeep -> Bool
bkIsInsideGuard (Bkeep i syb p f) = f `testBit` fInsideGuard
