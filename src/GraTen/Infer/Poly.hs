{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module GraTen.Infer.Poly
  ( generalize
  , instantiate
  , instantiateTy
  ) where

import Control.Monad.State
import Data.List             (nub)

import GraTen.Syntax.Type
import GraTen.Infer.InferM
import GraTen.Infer.Subst

-- Generalize a type to a type scheme.
generalize :: [(ID, TyScheme)] -> Type -> TyScheme
generalize tyenv t =
  let freeSVar = filter f (nub (fsv t))
      -- Since |ys| of |SVar x ys| is not compared on the substitution, |sigma|
      -- substitutes any SVars of the form |SVar b _| with |SBool True|.
      sigma = map (\b -> (T b [], SBool True)) freeSVar
   in ([], subst sigma t)
 where
  f x = x `notElem` concatMap (\(_, (_, ty)) -> fsv ty) tyenv

-- | Instantiates a type scheme.
instantiate :: Monad m
            => [ID]     -- ^ Variables in scope
            -> TyScheme
            -> StateT Int m Type
instantiate vars (fvs, ty) = do
  env <- mapM (\x -> (x,) <$> newInt) fvs
  instantiateTy vars env ty

-- | Instantiates a type.
instantiateTy :: Monad m
              => [ID]        -- ^ Variables in scope
              -> [(ID, Int)] -- ^ Mapping from old IDs to new IDs
              -> Type
              -> StateT Int m Type
instantiateTy vars env (TyRef t p) =
  TyRef <$> mapM (instantiateTy vars env) t <*> pure (instantiateS vars env p)
instantiateTy vars env (TyFun l Nothing t1 t2) =
  TyFun l Nothing <$> instantiateTy vars env t1 <*> instantiateTy vars env t2
instantiateTy vars env (TyFun l (Just x) t1 t2) = do
  n <- newInt
  TyFun l (Just (fst x, n)) <$> instantiateTy vars env t1
                            <*> instantiateTy vars ((x, n) : env) t2
instantiateTy vars env (TyOptionFun x t1 s t2) = do
  n <- newInt
  TyOptionFun (fst x, n) <$> instantiateTy vars env t1 <*> mapM (instantiateTy vars env) s
                         <*> instantiateTy vars ((x, n) : env) t2
instantiateTy vars env (TyOption t) = TyOption <$> instantiateTy vars env t
instantiateTy vars env (TyTuple ts) = TyTuple <$> mapM (instantiateTy vars env) ts
instantiateTy _ _ t                 = return t

-- | Instantiates a refinement predicate.
instantiateS :: [ID]        -- ^ Variables in scope
             -> [(ID, Int)] -- ^ Mapping from old IDs to new IDs
             -> Predicate
             -> Predicate
instantiateS vars env = mapPred go
  where
    go (SVar x ys)  = case lookup x env of
                        Just n  -> SVar (fst x, n) (map f ys ++ vars)
                        Nothing -> SVar x (map f ys)
    go (SName x n)  = SName (f x) n
    go s            = s

    f y = case lookup y env of
            Just n -> (fst y, n)
            Nothing -> y
