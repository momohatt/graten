{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module GraTen.Module
  ( addPath
  , addPathS
  ) where

import           GraTen.Syntax.Type
import           GraTen.SimpleType.Syntax


-- addPath add addPathS assumes that TyEnv is in the bottom-to-head order e.g.,
-- the newest definition in the TyEnv is the leftmost element of the list.
addPath :: [String] -> TyEnv -> TyEnv
addPath path env = addPath' path ps env
  where
    ps = concatMap (\case { Module x _ -> [(x, [])]; _ -> [] }) env

addPath' :: [String] -> [(String, [String])] -> TyEnv -> TyEnv
addPath' path ps = f
 where
  f [] = []
  f (VarDef x (fv, t) : defs) = VarDef x (fv, g (collectTyName defs) t) : f defs
  f (TyDef xx td : defs) =
    TyDef (xx { path = reverse path }) (g (xx : collectTyName defs) <$> td) : f defs
  f (Module x tyenv : defs) =
    let ps' = concatMap (\case { Module y _ -> [(y, x : path)]; _ -> [] }) tyenv
     in Module x (addPath' (x : path) (ps' ++ ps) tyenv) : f defs
  g scope (TyRef t p)             = TyRef (h scope t) p
  g scope (TyFun l n t1 t2)       = TyFun l n (g scope t1) (g scope t2)
  g scope (TyOptionFun n t1 s t2) = TyOptionFun n (g scope t1) (g scope <$> s) (g scope t2)
  g scope (TyTuple ts)            = TyTuple (map (g scope) ts)
  g scope (TyOption t)            = TyOption (g scope t)
  g _     t                       = t
  h scope (TyUser xx@(TyName [] y)) = if xx `elem` scope
                                         then TyUser (TyName (reverse path) y)
                                         else TyUser xx
  h _     (TyUser (TyName path y))  = case lookup (head path) ps of
                                        Nothing -> TyUser (TyName path y)
                                        Just par -> TyUser (TyName (reverse par ++ path) y)
  h scope (TyList t) = TyList (g scope t)
  h _     t          = t

addPathS :: [String] -> STyEnv -> STyEnv
addPathS path env = addPathS' path ps env
  where
    ps = concatMap (\case { Module x _ -> [(x, [])]; _ -> [] }) env

addPathS' :: [String] -> [(String, [String])] -> STyEnv -> STyEnv
addPathS' path ps = f
 where
  f [] = []
  f (VarDef x t : defs) = VarDef x (g (collectTyName defs) t) : f defs
  f (TyDef xx td : defs) =
    TyDef (xx { path = reverse path }) (g (xx : collectTyName defs) <$> td) : f defs
  f (Module x tyenv : defs) =
    let ps' = concatMap (\case { Module y _ -> [(y, x : path)]; _ -> [] }) tyenv
     in Module x (addPathS' (x : path) (ps' ++ ps) tyenv) : f defs
  g scope (STyUser xx@(TyName [] y)) = if xx `elem` scope
                                          then STyUser (TyName (reverse path) y)
                                          else STyUser xx
  g _     (STyUser (TyName path y)) = case lookup (head path) ps of
                                        Nothing -> STyUser (TyName path y)
                                        Just par -> STyUser (TyName (reverse par ++ path) y)
  g scope (STyFun l n t1 t2)  = STyFun l n (g scope t1) (g scope t2)
  g scope (STyOptFun n t1 t2) = STyOptFun n (g scope t1) (g scope t2)
  g scope (STyTuple ts)       = STyTuple (map (g scope) ts)
  g scope (STyOption t)       = STyOption (g scope t)
  g _     t                   = t

collectTyName :: [Def tyscheme ty] -> [TyName]
collectTyName []                  = []
collectTyName (TyDef xx _ : defs) = xx : collectTyName defs
collectTyName (_ : defs)          = collectTyName defs
