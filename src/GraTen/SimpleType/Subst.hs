module GraTen.SimpleType.Subst
  ( Constraint
  , Subst
  , subst
  , compose
  ) where

import           Data.Maybe         (isNothing)
import qualified Data.Tuple.Extra   as T

import GraTen.Syntax.Literal
import GraTen.SimpleType.Syntax


type Constraint = [(SimpleType, SimpleType, Pos)]

type Subst = [(Int, SimpleType)]

subst :: Subst -> SimpleType -> SimpleType
subst sigma (STyFun ml x t1 t2) = STyFun ml x (subst sigma t1) (subst sigma t2)
subst sigma (STyOptFun l t1 t2) = STyOptFun l (subst sigma t1) (subst sigma t2)
subst sigma (STyTuple ts)       = STyTuple (map (subst sigma) ts)
subst sigma (STyOption t)       = STyOption (subst sigma t)
subst sigma (STyList t)         = STyList (subst sigma t)
subst sigma (STyVar x)          = case lookup x sigma of
                                    Nothing -> STyVar x
                                    Just ty -> ty
subst _     t                   = t

compose :: Subst -> Subst -> Subst
compose s1 s2 =
  map (T.second (subst s1)) s2 ++ filter (\(x, _) -> isNothing (lookup x s2)) s1
