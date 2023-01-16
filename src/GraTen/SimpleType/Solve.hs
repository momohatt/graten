{-# LANGUAGE TupleSections #-}

module GraTen.SimpleType.Solve
  ( solve
  ) where

import GraTen.Infer.InferM
import GraTen.SimpleType.Subst
import GraTen.SimpleType.Syntax
import GraTen.Pretty


solve :: Constraint -> InferM Subst
solve [] = return []
solve ((STyUnit, STyUnit, _) : xs) = solve xs
solve ((STyInt,  STyInt, _)  : xs) = solve xs
solve ((STyBool, STyBool, _) : xs) = solve xs
solve ((STyFun _ _ t1 t2, STyFun _ _ t3 t4, pos) : xs) =
  solve ((t1, t3, pos) : (t2, t4, pos) : xs)
solve ((STyOptFun _ t1 t2, STyOptFun _ t3 t4, pos) : xs) =
  solve ((t1, t3, pos) : (t2, t4, pos) : xs)
solve ((STyTuple ts1, STyTuple ts2, pos) : xs) | length ts1 == length ts2 =
  solve (zipWith (,,pos) ts1 ts2 ++ xs)
solve ((STyOption t1, STyOption t2, pos) : xs) = solve ((t1, t2, pos) : xs)
solve ((STyList t1, STyList t2, pos) : xs) = solve ((t1, t2, pos) : xs)
solve ((STyUser xx, STyUser yy, _) : xs) | xx == yy = solve xs
solve ((STyTensor, STyTensor, _) : xs) = solve xs
solve ((STyVar x, STyVar y, _) : xs) | x == y = solve xs
solve ((STyVar x, t, _) : xs) | not (occur x t) =
  (`compose` [(x, t)]) <$> solve (map (\(t1, t2, pos) -> (subst [(x, t)] t1, subst [(x, t)] t2, pos)) xs)
solve ((t, STyVar x, _) : xs) | not (occur x t) =
  (`compose` [(x, t)]) <$> solve (map (\(t1, t2, pos) -> (subst [(x, t)] t1, subst [(x, t)] t2, pos)) xs)
solve ((t1, t2, pos) : _) =
  throwError ("Cannot unify " ++ pretty t1 ++ " and " ++ pretty t2 ++ " at " ++ show pos)

occur :: Int -> SimpleType -> Bool
occur x (STyVar y)          = x == y
occur x (STyFun _ _ t1 t2)  = occur x t1 || occur x t2
occur x (STyOptFun _ t1 t2) = occur x t1 || occur x t2
occur x (STyTuple ts)       = any (occur x) ts
occur x (STyList t)         = occur x t
occur _ _                   = False
