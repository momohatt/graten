{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module GraTen.Infer.Subtype
  ( subtypeToVC
  , extractCtx
  , extractCtx'
  ) where

import           GraTen.Infer.Constraint
import           GraTen.Infer.InferM
import           GraTen.Infer.Subst
import           GraTen.Pretty
import           GraTen.Syntax.Literal   (showPos)
import           GraTen.Syntax.Type


subtypeToVC :: [Subtype] -> CheckM [VC]
subtypeToVC [] = return []
subtypeToVC ((vars, ctx, t1, t2, pos) : xs) =
  case (t1, t2) of
    (TyRef t1 p1, TyRef t2 p2) -> do
      xs' <- case (t1, t2) of
               (TyInt,  TyInt)  -> return []
               (TyBool, TyBool) -> return []
               (TyTensor, TyTensor) -> return []
               (TyList t1', TyList t2') -> return [(vars, ctx, t1', t2', pos)]
               (TyUser xx, TyUser yy) | xx == yy -> return []
               _ -> throwError "Error: base type mismatch"
      (Impl vars ctx [p1] [p2] :) <$> subtypeToVC (xs' ++ xs)

    (TyFun l1 (Just x) t1' t2', TyFun l2 (Just y) t3' t4') | l1 == l2 ->
      (Impl vars [] [] [SEq (SName y []) (SName x [])] :) <$> -- trick for recursion type annotation
        subtypeToVC ((vars, ctx, t3', t1', pos) : (y : vars, ctx ++ extractCtx [(y, t3')], t2', t4', pos) : xs)
    (TyFun _ _ t1' t2', TyFun _ (Just y) t3' t4') ->
      subtypeToVC ((vars, ctx, t3', t1', pos) : (y : vars, ctx ++ extractCtx [(y, t3')], t2', t4', pos) : xs)
    (TyFun _ (Just x) t1' t2', TyFun _ _ t3' t4') ->
      subtypeToVC ((vars, ctx, t3', t1', pos) : (x : vars, ctx ++ extractCtx [(x, t3')], t2', t4', pos) : xs)
    (TyFun _ _ t1' t2', TyFun _ _ t3' t4') ->
      subtypeToVC ((vars, ctx, t3', t1', pos) : (vars, ctx, t2', t4', pos) : xs)
    (TyOptionFun l1 t1' (Just s1) t2', TyOptionFun l2 t3' (Just s2) t4') | l1 == l2 ->
      subtypeToVC ((vars, ctx, t3', t1', pos) : (vars, ctx, s2, s1, pos) : (l2 : vars, ctx ++ extractCtx [(l2, t3')], t2', t4', pos) : xs)
    (TyOptionFun l1 t1' _ t2', TyOptionFun l2 t3' _ t4') | l1 == l2 ->
      subtypeToVC ((vars, ctx, t3', t1', pos) : (l2 : vars, ctx ++ extractCtx [(l2, t3')], t2', t4', pos) : xs)

    (TyUnit, TyUnit) -> subtypeToVC xs
    (TyTuple ts1, TyTuple ts2) ->
      subtypeToVC (zipWith (\t1 t2 -> (vars, ctx, t1, t2, pos)) ts1 ts2 ++ xs)
    (TyOption t1, TyOption t2) -> subtypeToVC ((vars, ctx, t1, t2, pos) : xs)

    _ -> throwError ("Error: cannot induce " ++ pretty t1 ++ " <: " ++ pretty t2 ++ "\n  at " ++ showPos pos)

extractCtx :: [(ID, Type)] -> Context
extractCtx [] = []
extractCtx ((x, TyRef _ p) : env)  = rename Self x p : extractCtx env
extractCtx ((x, TyTuple ts) : env) = concat (zipWith f [1..] ts) ++ extractCtx env
    where
      f :: Int -> Type -> Context
      f i (TyRef _ p) = [subst [(N Self [], SName x ['_' : show i])] p]
      f _ _           = []
extractCtx (_ : env) = extractCtx env

extractCtx' :: TyEnv -> Context
extractCtx' = extractCtx . concatMap (\case { VarDef x (_, ts) -> [(x, ts)]; _ -> [] })
