{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module GraTen.Alpha
  ( alpha
  ) where

import qualified Data.Tuple.Extra    as T

import           GraTen.Syntax       hiding (Def (..))
import           GraTen.Infer.InferM

data VarOrModule
  = Var Int
  | Module Table

type Table = [(String, VarOrModule)]

alpha :: Monad m => IProgram String ty -> StateT Int m (IProgram ID ty)
alpha prog = fst <$> alphaProg [] prog

alphaProg :: Monad m => Table -> IProgram String ty -> StateT Int m (IProgram ID ty, Table)
alphaProg env [] = return ([], env)
alphaProg env (d:prog) = do
  (d', env') <- alphaD env d
  T.first (d':) <$> alphaProg env' prog

alphaD :: Monad m => Table -> IDecl String ty -> StateT Int m (IDecl ID ty, Table)
alphaD env (ILet p e pos) = do
  e' <- alphaE env e
  (p', env') <- alphaP env p
  return (ILet p' e' pos, env' ++ env)
alphaD env (ILetRec f t ty e pos) = do
  n <- newInt
  t' <- mapM (alphaT env) t
  e' <- alphaE ((f, Var n) : env) e
  return (ILetRec (f, n) t' ty e' pos, (f, Var n) : env)
alphaD env (IOpen m) =
  case lookup m env of
    Just (Module env') -> return (IOpen m, env' ++ env)
    _ -> return (IOpen m, env)
alphaD env (IModule x ds) = do
  (ds', env') <- alphaProg env ds
  return (IModule x ds', (x, Module env') : env)
alphaD env (ILetModule x path) =
  case f env [] path of
    []   -> return (ILetModule x path, env)
    env' -> return (ILetModule x path, (x, Module env') : env)
 where
  f _   r []     = r
  f env _ (x:xs) = case lookup x env of
                   Just (Module env') -> f env' env' xs
                   _ -> []
alphaD env (ITypeDef xx td) = do
  td <- mapM (alphaT env) td
  return $ (ITypeDef xx td, env)

alphaE :: Monad m => Table -> IExpr String ty -> StateT Int m (IExpr ID ty)
alphaE _   IUnit      = return IUnit
alphaE _   (IConst c) = return (IConst c)
alphaE env (IVar x t) =
  case lookup x env of
    Just (Var n) -> return (IVar (x, n) t)
    _ -> return (IVar (x, 0) t)
alphaE env (ICtor c me t)  = ICtor c <$> mapM (T.secondM (alphaE env)) me <*> pure t
alphaE env (ITag c me)     = ITag c <$> mapM (alphaE env) me
alphaE env (ITuple es)     = ITuple <$> mapM (alphaE env) es
alphaE _   (INil t)        = return (INil t)
alphaE env (ICons p e1 e2) = ICons p <$> alphaE env e1 <*> alphaE env e2
alphaE env (IRecord p es)  = IRecord p <$> mapM (T.secondM (alphaE env)) es
alphaE env (IField p e x)  = IField p <$> alphaE env e <*> pure x
alphaE env (IIf p x e1 e2 t) =
  IIf p <$> alphaE env x <*> alphaE env e1 <*> alphaE env e2 <*> pure t
alphaE env (IFor p i e1 e2 e3) = do
  n <- newInt
  IFor p (i, n) <$> alphaE env e1 <*> alphaE env e2 <*> alphaE ((i, Var n) : env) e3
alphaE env (ILetIn d e) = do
  (d', env') <- alphaD env d
  ILetIn d' <$> alphaE env' e
alphaE env (IFun pos ml p t e) = do
  (p', env') <- alphaP env p
  IFun pos ml p' t <$> alphaE (env' ++ env) e
alphaE env (IOptFun x s t e) = do
  n <- newInt
  s' <- mapM (alphaE env) s
  IOptFun (x, n) s' t <$> alphaE ((x, Var n) : env) e
alphaE env (IApp mx p e1 e2) = IApp mx p <$> alphaE env e1 <*> alphaE env e2
alphaE env (IAppEnd e) = IAppEnd <$> alphaE env e
alphaE env (IMatch p e cls t) = IMatch p <$> alphaE env e <*> mapM (alphaC env) cls <*> pure t
  where
    alphaC env (p, e) = do
      (p', env') <- alphaP env p
      e' <- alphaE (env' ++ env) e
      return (p', e')
alphaE env (ISeq p e1 e2) = ISeq p <$> alphaE env e1 <*> alphaE env e2
alphaE env (IType p e t)  = IType p <$> alphaE env e <*> alphaT env t
alphaE _ ICast{} = undefined
alphaE _ IAssert{} = undefined

alphaT :: Monad m => Table -> Type -> StateT Int m Type
alphaT env (TyRef t p)    = TyRef <$> mapM (alphaT env) t <*> pure (alphaS env p)
alphaT env (TyFun ml (Just (x, _)) t1 t2) = do
  n <- newInt
  TyFun ml (Just (x, n)) <$> alphaT env t1 <*> alphaT ((x, Var n) : env) t2
alphaT env (TyFun ml Nothing t1 t2) =
  TyFun ml Nothing <$> alphaT env t1 <*> alphaT env t2
alphaT env (TyOptionFun (l, _) t1 s t2) = do
  n <- newInt
  TyOptionFun (l, n) <$> alphaT env t1 <*> mapM (alphaT env) s
                     <*> alphaT ((l, Var n) : env) t2
alphaT env (TyTuple ts) = TyTuple <$> mapM (alphaT env) ts
alphaT env (TyOption t) = TyOption <$> alphaT env t
alphaT _   t            = return t

alphaS :: Table -> Predicate -> Predicate
alphaS env = mapPred (\case
    SVar x ys  -> SVar x (map maybeRenameID ys)
    SName x fs -> SName (maybeRenameID x) fs
    s          -> s)
  where
    maybeRenameID x = case lookup (fst x) env of
                        Just (Var n) -> (fst x, n)
                        _            -> x

alphaP :: Monad m => Table -> Pattern String -> StateT Int m (Pattern ID, Table)
alphaP _ PWildcard  = return (PWildcard, [])
alphaP _ (PConst c) = return (PConst c, [])
alphaP _ PUnit      = return (PUnit, [])
alphaP _ (PTag s)   = return (PTag s, [])
alphaP _ PNil       = return (PNil, [])
alphaP _ (PVar x) = do
  n <- newInt
  return (PVar (x, n), [(x, Var n)])
alphaP env (PTuple ps) = do
  (ps', envs') <- unzip <$> mapM (alphaP env) ps
  return (PTuple ps', concat envs')
alphaP env (PRecord path fs) = do
  (fs', envs') <- unzip <$> mapM (\(x, p) -> T.first (x,) <$> alphaP env p) fs
  return (PRecord path fs', concat envs')
alphaP _   (PConstr s Nothing) = return (PConstr s Nothing, [])
alphaP env (PConstr s (Just p)) = T.first (PConstr s . Just) <$> alphaP env p
alphaP env (PCons p1 p2) = do
  (p1', env1') <- alphaP env p1
  (p2', env2') <- alphaP env p2
  return (PCons p1' p2', env1' ++ env2')
alphaP env (PType x t) = do
  n <- newInt
  t' <- alphaT env t
  return (PType (x, n) t', [(x, Var n)])
