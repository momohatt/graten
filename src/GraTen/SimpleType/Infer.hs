{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module GraTen.SimpleType.Infer
  ( typecheck
  ) where

import           Control.Monad.State
import           Data.List                (find, intercalate)
import qualified Data.Tuple.Extra         as T

import           GraTen.Infer.InferM
import           GraTen.Module
import           GraTen.Pretty
import           GraTen.SimpleType.Solve
import           GraTen.SimpleType.Subst
import           GraTen.SimpleType.Syntax
import           GraTen.Syntax.Expr
import           GraTen.Syntax.IExpr
import           GraTen.Syntax.Literal    (Pos)
import           GraTen.Syntax.Type       (ID, Def (..), TypeDef (..))
import qualified GraTen.Syntax.Type       as R


typecheck :: R.TyEnv -> IProgram ID () -> InferM (IProgram ID SimpleType)
typecheck env prog = do
  (_, c, prog') <- inferProg (tyenvToStyenv env) prog
  sigma <- solve c
  return $ map (subst sigma <$>) prog'

newTyVar :: InferM SimpleType
newTyVar = STyVar <$> newInt

inferProg :: STyEnv -> IProgram ID () -> InferM (STyEnv, Constraint, IProgram ID SimpleType)
inferProg env [] = return (env, [], [])
inferProg env (d : ds) = do
  (env', c1, d') <- inferDecl env d
  (env'', c2, ds') <- inferProg env' ds
  return (env'', c1 ++ c2, d' : ds')

inferDecl :: STyEnv -> IDecl ID () -> InferM (STyEnv, Constraint, IDecl ID SimpleType)
inferDecl env (ILet p e pos) = do
  (t, c1, e') <- inferExpr env e
  (env', c2) <- matchPattern env p t
  let c = c1 ++ map (\(t1, t2) -> (t1, t2, pos)) c2
  -- Resolve constraints at every let binding so that optional/labeled function
  -- arguments can be supported
  sigma <- solve c
  return (map (T.second (subst sigma)) env' `R.addMapping` env, c, ILet p e' pos)
inferDecl env (ILetRec f mt _ e pos) = do
  t <- case mt of
         Nothing -> newTyVar
         Just t' -> return (tyToSimpleTy t')
  (t', c, e') <- inferExpr (VarDef f t : env) e
  let c' = (t, t', pos) : c
  sigma <- solve c'
  return (R.updateMapping (subst sigma) (VarDef f t : env), c', ILetRec f mt t e' pos)
inferDecl env (IOpen x) =
  case R.lookupModule x env of
    Just m -> return (m ++ env, [], IOpen x)
    _ -> do
      liftIO $ putStrLn ("\ESC[33mWarning: Unknown module " ++ x ++ "\ESC[39m")
      return (env, [], IOpen x)
inferDecl env (IModule x ds) = do
  (env', c, ds') <- inferProg env ds
  let env'' = addPathS [x] env'
  return (R.Module x env'' : env, c, IModule x ds')
inferDecl env (ILetModule x path) =
  case R.findPath env path of
    Just m  -> return (R.Module x m : env, [], ILetModule x path)
    Nothing -> return (env, [], ILetModule x path)
inferDecl env (ITypeDef xx td) =
  return (TyDef xx (tyToSimpleTy <$> td) : env, [], ITypeDef xx td)

inferExpr :: STyEnv -> IExpr ID () -> InferM (SimpleType, Constraint, IExpr ID SimpleType)
inferExpr _ IUnit = return (STyUnit, [], IUnit)
inferExpr _ (IConst c) = return (typeOfConst c, [], IConst c)
inferExpr env (IVar x _) =
  case R.lookupVar x env of
    Just ty -> return (ty, [], IVar x ty)
    Nothing -> do
      liftIO $ putStrLn ("\ESC[33mWarning: Unknown variable " ++ fst x ++ "\ESC[39m")
      ty <- newTyVar
      return (ty, [], IVar x ty)
inferExpr env (ICtor x me _) =
  case (findCtor x env, me) of
    (Nothing, Nothing) -> do
      ty <- newTyVar
      return (ty, [], ICtor x Nothing ty)
    (Nothing, Just (pos, e)) -> do
      ty <- newTyVar
      (_, c, e') <- inferExpr env e
      return (ty, c, ICtor x (Just (pos, e')) ty)
    (Just (xx, Nothing), Nothing) -> return (STyUser xx, [], ICtor x Nothing (STyUser xx))
    (Just (xx, Just t), Just (pos, e)) -> do
      (c, e') <- checkExpr env e t pos
      return (STyUser xx, c, ICtor x (Just (pos, e')) (STyUser xx))
    (Just (_, Nothing), Just _) ->
      throwError ("Constructor " ++ x ++ " does not take an argument")
    (Just (_, Just _), Nothing) ->
      throwError ("Constructor " ++ x ++ " needs an argument")
inferExpr _   (ITag x  Nothing) = return (STyString, [], ITag x Nothing)
inferExpr env (ITag x (Just e)) = do
  (_, c, e') <- inferExpr env e
  return (STyString, c, ITag x (Just e'))
inferExpr env (ITuple es) = do
  (ts, cs, es') <- unzip3 <$> mapM (inferExpr env) es
  return (STyTuple ts, concat cs, ITuple es')
inferExpr _ (INil ()) = do
  t <- newTyVar
  return (STyList t, [], INil t)
inferExpr env (ICons pos e1 e2) = do
  (t1, c1, e1') <- inferExpr env e1
  (t2, c2, e2') <- inferExpr env e2
  return (t2, (STyList t1, t2, pos) : c1 ++ c2, ICons pos e1' e2')
inferExpr env (IRecord pos es) = do
  (xs, rs) <- unzip <$> mapM (\(x, e) -> (x,) <$> inferExpr env e) es
  let (ts, cs, es') = unzip3 rs
  case find (\case { TyDef _ (Record _ fs) -> map fst fs == xs; _ -> False }) env of
    Just (TyDef x (Record _ ts')) -> do
      let c = zipWith (,,pos) ts (map snd ts')
      return (STyUser x, c ++ concat cs, IRecord pos (zip xs es'))
    _ -> throwError ("Unknown fields: " ++ intercalate ", " xs)
inferExpr env (IField pos e x) = do
  (t, c, e') <- inferExpr env e
  case findField x env of
    Just (xx, t') -> return (t', (STyUser xx, t, pos) : c, IField pos e' x)
    Nothing       -> throwError ("Unknown field: " ++ x)
inferExpr env (IIf pos x e1 e2 _) = do
  (t0, c0, x')  <- inferExpr env x
  (t1, c1, e1') <- inferExpr env e1
  (t2, c2, e2') <- inferExpr env e2
  return (t1, [(t0, STyBool, pos), (t1, t2, pos)] ++ c0 ++ c1 ++ c2, IIf pos x' e1' e2' t1)
inferExpr env (IFor pos x e1 e2 e3) = do
  (t1, c1, e1') <- inferExpr env e1
  (t2, c2, e2') <- inferExpr env e2
  (t3, c3, e3') <- inferExpr (VarDef x STyInt : env) e3
  return (STyUnit, [(t1, STyInt, pos), (t2, STyInt, pos), (t3, STyUnit, pos)] ++ c1 ++ c2 ++ c3,
          IFor pos x e1' e2' e3')
inferExpr env (ILetIn d e) = do
  (env', c1, d') <- inferDecl env d
  (t, c2, e') <- inferExpr env' e
  return (t, c1 ++ c2, ILetIn d' e')
inferExpr env (IFun pos ml p _ e) = do
  t1 <- newTyVar
  (env', c') <- matchPattern env p t1
  (t2, c, e') <- inferExpr (env' `R.addMapping` env) e
  let mx = case p of { PVar x -> Just x; PType x _ -> Just x; _ -> Nothing }
  return (STyFun ml mx t1 t2, c ++ map (\(t1, t2) -> (t1, t2, pos)) c', IFun pos ml p t1 e')
inferExpr env (IOptFun l (Just x) _ e) = do
  (t1, c1, x') <- inferExpr env x
  (t2, c2, e') <- inferExpr (VarDef l t1 : env) e
  return (STyOptFun l t1 t2, c1 ++ c2, IOptFun l (Just x') t1 e')
inferExpr env (IOptFun l Nothing _ e) = do
  t1 <- newTyVar
  (t2, c2, e') <- inferExpr (VarDef l (STyOption t1) : env) e
  return (STyOptFun l t1 t2, c2, IOptFun l Nothing t1 e')
inferExpr env (IApp ml pos e1 e2) = do
  (t1, c1, e1') <- inferExpr env e1
  (tyA, tyR, c3) <- loop ml t1
  (c2, e2') <- checkExpr env e2 tyA pos
  return (tyR, c1 ++ c2 ++ c3, IApp ml pos e1' e2')
 where
  loop Nothing  (STyFun Nothing _ t1 t2) = return (t1, t2, [])
  loop (Just x) (STyFun (Just l) _ t1 t2) | x == l = return (t1, t2, [])
  loop mx       (STyFun ml x t1 t2) = T.second3 (STyFun ml x t1) <$> loop mx t2
  loop (Just x) (STyOptFun l t1 t2) | x == fst l = return (t1, t2, [])
  loop mx       (STyOptFun l t1 t2) =
    T.second3 (STyOptFun l t1) <$> loop mx t2
  loop mx t = do
    tyA <- newTyVar
    tyR <- newTyVar
    return (tyA, tyR, [(t, STyFun mx Nothing tyA tyR, pos)])
inferExpr env (IAppEnd e) = do
  (t, c, e') <- inferExpr env e
  return (removeOpt t, c, IAppEnd e')
 where
  removeOpt (STyOptFun _ _ t) = removeOpt t
  removeOpt t                 = t
inferExpr env (IMatch pos e1 clauses _) = do
  (t1, c1, e1') <- inferExpr env e1
  ty <- newTyVar
  (cs, clauses') <- unzip <$> forM clauses (\(p, e2) -> do
    (env', c2) <- T.second (map (\(t1, t2) -> (t1, t2, pos))) <$> matchPattern env p t1
    (t2, c3, e2') <- inferExpr (env' `R.addMapping` env) e2
    return ((ty, t2, pos) : c2 ++ c3, (p, e2')))
  return (ty, c1 ++ concat cs, IMatch pos e1' clauses' ty)
inferExpr env (ISeq pos e1 e2) = do
  (t1, c1, e1') <- inferExpr env e1
  (t2, c2, e2') <- inferExpr env e2
  return (t2, (t1, STyUnit, pos) : c1 ++ c2, ISeq pos e1' e2')
inferExpr env (IType pos e t) = do
  (t1, c, e') <- inferExpr env e
  return (t1, (t1, tyToSimpleTy t, pos) : c, IType pos e' t)
inferExpr _ ICast{} = undefined
inferExpr _ IAssert{} = undefined

checkExpr :: STyEnv -> IExpr ID () -> SimpleType -> Pos -> InferM (Constraint, IExpr ID SimpleType)
checkExpr env (ICtor x me _) ty@(STyUser tyname) _ = do
  case R.findPath env (R.path tyname) >>= R.lookupType tyname of
    Just (Variant tags) ->
      case (lookup x tags, me) of
        (Nothing, Nothing) -> ([],) . ICtor x Nothing <$> newTyVar
        (Nothing, Just (pos, e)) -> do
          ty <- newTyVar
          (_, c, e') <- inferExpr env e
          return (c, ICtor x (Just (pos, e')) ty)
        (Just Nothing, Nothing) -> return ([], ICtor x Nothing ty)
        (Just (Just tyA), Just (pos, e)) -> do
          (c, e') <- checkExpr env e tyA pos
          return (c, ICtor x (Just (pos, e')) ty)
        (Just Nothing, Just _) -> throwError ("Constructor " ++ x ++ " does not take an argument")
        (Just Just{}, Nothing) -> throwError ("Constructor " ++ x ++ " needs an argument")
    _ -> throwError ("Unknown variant type: " ++ pretty tyname)
checkExpr env (ITuple es) (STyTuple ts) pos = do
  (cs, es') <- unzip <$> zipWithM (\e t -> checkExpr env e t pos) es ts
  return (concat cs, ITuple es')
checkExpr env e ty pos = do
  (t, c, e') <- inferExpr env e
  return ((t, ty, pos) : c, e')


findFields :: String -> STyEnv -> Maybe (R.TyName, [(String, SimpleType)])
findFields _ []                                                = Nothing
findFields x (TyDef xx (Record _ fs@(lookup x -> Just{})) : _) = Just (xx, fs)
findFields x (_ : xs)                                          = findFields x xs

findField :: String -> STyEnv -> Maybe (R.TyName, SimpleType)
findField _ []                                             = Nothing
findField x (TyDef xx (Record _ (lookup x -> Just t)) : _) = Just (xx, t)
findField x (_ : xs)                                       = findField x xs

findCtor :: String -> STyEnv -> Maybe (R.TyName, Maybe SimpleType)
findCtor _ []                                               = Nothing
findCtor x (TyDef xx (Variant (lookup x -> Just marg)) : _) = Just (xx, marg)
findCtor x (_ : xs)                                         = findCtor x xs

matchPattern :: STyEnv -> Pattern ID -> SimpleType -> InferM ([(ID, SimpleType)], [(SimpleType, SimpleType)])
matchPattern _ PWildcard  _  = return ([], [])
matchPattern _ (PConst c) ty = return ([], [(typeOfConst c, ty)])
matchPattern _ (PVar x)   ty = return ([(x, ty)], [])
matchPattern _ PUnit      ty = return ([], [(STyUnit, ty)])
matchPattern _ (PTag _)   ty = return ([], [(STyString, ty)])
matchPattern env (PTuple ps) ty = do
  ts <- mapM (\_ -> newTyVar) ps
  (envs, cs) <- unzip <$> zipWithM (matchPattern env) ps ts
  return (concat envs, (STyTuple ts, ty) : concat cs)
matchPattern _   (PRecord _ []) _ = throwError "Empty record pattern"
matchPattern env (PRecord [] fs@((x, _):_)) ty =
  case findFields x env of
    Nothing -> throwError ("Unknown record field: " ++ x)
    Just (xx, fs') -> do
      (envs, cs) <- unzip <$> mapM findType fs
      return (concat envs, (STyUser xx, ty) : concat cs)
     where
      findType (x, p) =
        case lookup x fs' of
          Nothing -> throwError ("Unknown record field: " ++ x)
          Just t -> matchPattern env p t
matchPattern env (PRecord (p:path) fs@(_:_)) ty =
  case R.lookupModule p env of
    Just env' -> matchPattern env' (PRecord path fs) ty
    _ -> throwError ("Unknown module: " ++ p)
matchPattern _ (PType x t) ty = do
  let t' = tyToSimpleTy t
  return ([(x, t')], [(t', ty)])
matchPattern _ PNil ty = do
  t <- newTyVar
  return ([], [(ty, STyList t)])
matchPattern env (PCons p1 p2) ty = do
  t1 <- newTyVar
  t2 <- newTyVar
  (env1, c1) <- matchPattern env p1 t1
  (env2, c2) <- matchPattern env p2 t2
  return (env1 ++ env2, (STyList t1, t2) : (ty, t2) : c1 ++ c2)
matchPattern _ (PConstr "None" Nothing) ty = do
  t <- newTyVar
  return ([], [(STyOption t, ty)])
matchPattern env (PConstr "Some" (Just p)) ty = do
  t <- newTyVar
  T.second ((ty, STyOption t) :) <$> matchPattern env p t
matchPattern env (PConstr x mp) ty = do
  case (findCtor x env, mp) of
    (Nothing, _) -> throwError ("Unknown pattern constructor: " ++ x)
    (Just (xx, Nothing), Nothing) -> return ([], [(STyUser xx, ty)])
    (Just (xx, Just t), Just p) ->
      T.second ((STyUser xx, ty) :) <$> matchPattern env p t
    (Just (_, Just _), Nothing) ->
      throwError ("Constructor " ++ x ++ " needs an argument")
    _ -> throwError ("Invalid argument to constructor " ++ x)
