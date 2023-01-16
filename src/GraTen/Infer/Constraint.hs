{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GraTen.Infer.Constraint
  ( Subtype
  , Constr (..)
  , VC
  , Constraints (..)
  , emptyC
  , appendC
  , addVC
  , addSS
  , addSubtype
  , concatC
  ) where

import Data.List          (intercalate)

import GraTen.Syntax
import GraTen.Infer.Subst
import GraTen.Pretty

type Subtype = ([ID], Context, Type, Type, Pos)

data Constr a = Impl [ID] [a] [a] [a]
  deriving (Eq, Functor, Foldable)

-- Verification Condition
type VC = Constr Predicate

instance Substitutable VC where
  subst [] vc = vc
  subst ss (Impl ids ctx p q) =
    Impl (map (substID ss) ids) (map (subst ss) ctx)
         (map (subst ss) p) (map (subst ss) q)

instance Pretty Subtype where
  pretty (vars, ctx, t1, t2, pos) =
    let f sep xs = if null xs then "[]" else "[ " ++ intercalate sep xs ++ " ]"
        varsInScope = f ", " (map pretty vars)
        context = f "\n  , " (map pretty (filter (/= SBool True) ctx))
     in '@' : varsInScope ++ "\n  " ++ context ++ " |- " ++ pretty t1 ++ " <: " ++ pretty t2 ++ " @" ++ pretty pos

instance Pretty VC where
  pretty (Impl vars ctx p q) =
    let f sep xs = if null xs then "[]" else "[ " ++ intercalate sep xs ++ " ]"
        varsInScope = f ", " (map pretty vars)
        context = f ", " (map pretty ctx)
        lhs = f ", " (map pretty p)
     in if length context > 100 || length lhs > 100
           then '@' : varsInScope ++ "\n  " ++ context ++ "\n  => " ++ lhs ++ "\n  => " ++ f "\n     , " (map pretty q)
           else if length (context ++ lhs) > 100
           then '@' : varsInScope ++ "\n  " ++ context ++ " => " ++ lhs ++ " =>\n    " ++ f "\n     , " (map pretty q)
           else '@' : varsInScope ++ "\n  " ++ context ++ " => " ++ lhs ++ " => " ++
             f ('\n' : replicate (8 + length (context ++ lhs)) ' ' ++ ", ") (map pretty q)

data Constraints = Con
  { unsolved     :: [VC]
  , subtype      :: [Subtype]
  , substitution :: Subst
  }

instance Pretty Constraints where
  pretty (Con us sub s) = unlines ["unsolved:", pretty us, "subtype:", pretty sub, pretty s]

emptyC :: Constraints
emptyC = Con [] [] emptyS

appendC :: Constraints -> Constraints -> Constraints
appendC c1 c2 =
  Con (unsolved c1 ++ unsolved c2)
      (subtype c1 ++ subtype c2)
      (substitution c1 `compose` substitution c2)

addVC :: [VC] -> Constraints -> Constraints
addVC vc c = c { unsolved = vc ++ unsolved c }

addSS :: Subst -> Constraints -> Constraints
addSS ss c = c { substitution = substitution c `compose` ss }

addSubtype :: [Subtype] -> Constraints -> Constraints
addSubtype sub c = c { subtype = sub ++ subtype c }

concatC :: [Constraints] -> Constraints
concatC cs =
  Con (concatMap unsolved cs) (concatMap subtype cs) (concatS (map substitution cs))
