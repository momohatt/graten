{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module GraTen.Infer
  ( module GraTen.Infer.InferM
  , typecheck
  ) where

import           Data.List                (find, intercalate, nub, (\\))
import qualified Data.Tuple.Extra         as T

import           GraTen.Infer.Constraint
import           GraTen.Infer.InferM
import           GraTen.Infer.Poly
import           GraTen.Infer.Solve
import           GraTen.Infer.Subst
import           GraTen.Infer.Subtype
import           GraTen.Module
import           GraTen.Pretty
import           GraTen.SimpleType.Syntax (SimpleType(..), tyToSimpleTy)
import           GraTen.Syntax


typecheck :: TyEnv -> IProgram ID SimpleType -> InferM (TyEnv, IProgram ID ())
typecheck initEnv prog = do
  (env, _, c, prog') <- inferProg initEnv [] emptyC prog
  tellLn ("Constraints left:\n" ++ pretty c)
  let sigma = substitution c
  let env' = updateMapping (subst sigma) (take (length env - length initEnv) env)
  return (env', map (updateCast sigma) prog')
 where
  updateCast ss (ILet p e pos)         = ILet p (updateCast' ss e) pos
  updateCast ss (ILetRec x mt t e pos) = ILetRec x mt t (updateCast' ss e) pos
  updateCast ss (IModule x ds)         = IModule x (map (updateCast ss) ds)
  updateCast _  d                      = d

  updateCast' ss = f
    where
      f (ICast e _ _ (TyRef _ (SBool True)) _) = f e
      f (ICast e ctx t1 t2 p) =
        ICast (f e) (filter (/= SBool True) (map (g . subst ss) ctx))
              (mapPredInType g (subst ss t1)) (mapPredInType g (subst ss t2)) p
      f (ITuple es)         = ITuple (map f es)
      f (ICons p e1 e2)     = ICons p (f e1) (f e2)
      f (IField p e x)      = IField p (f e) x
      f (IRecord p es)      = IRecord p (map (T.second f) es)
      f (IIf p x e1 e2 t)   = IIf p (f x) (f e1) (f e2) t
      f (IFor p i e1 e2 e3) = IFor p i (f e1) (f e2) (f e3)
      f (ILetIn d e)        = ILetIn (updateCast ss d) (f e)
      f (IFun p l x t e)    = IFun p l x t (f e)
      f (IOptFun x e1 t e2) = IOptFun x (f <$> e1) t (f e2)
      f (IApp x p e1 e2)    = IApp x p (f e1) (f e2)
      f (IAppEnd e)         = IAppEnd (f e)
      f (IMatch p e cls t)  = IMatch p (f e) (map (T.second f) cls) t
      f (ISeq p e1 e2)      = ISeq p (f e1) (f e2)
      f e                   = e
      g = mapPred (\case { SVar{} -> SBool True; s -> s })

genTypeBase :: TyEnv -> ([ID] -> InferM Predicate) -> SimpleType -> InferM Type
genTypeBase _   _ STyUnit       = return TyUnit
genTypeBase env f STyInt        = TyRef TyInt <$> f (depId env)
genTypeBase env f STyBool       = TyRef TyBool <$> f (depId env)
genTypeBase env f STyTensor     = TyRef TyTensor <$> f (depId env)
genTypeBase env f (STyUser xx)  = TyRef (TyUser xx) <$> f (depId env)
genTypeBase env f (STyTuple ts) = TyTuple <$> mapM (genTypeBase env f) ts
genTypeBase env f (STyOption t) = TyOption <$> genTypeBase env f t
genTypeBase env f (STyList t)   = TyRef <$> (TyList <$> genTypeBase env f t) <*> f (depId env)
genTypeBase env f (STyFun l x t1 t2) = do
  t1' <- genTypeBase env f t1
  t2' <- case x of
           Nothing -> genTypeBase env f t2
           Just x' -> genTypeBase (VarDef x' ([], t1') : env) f t2
  return $ TyFun l x t1' t2'
genTypeBase env f (STyOptFun l t1 t2) = do
  t1' <- genTypeBase env f t1
  s' <- genTypeBase env f t1
  t2' <- genTypeBase (VarDef l ([], t1') : env) f t2
  return $ TyOptionFun l t1' (Just s') t2'
-- If the simple type has not been decided, assume it's unit
genTypeBase _  _ (STyVar _)    = return TyUnit

genType :: TyEnv -> SimpleType -> InferM Type
genType env = genTypeBase env (\xs -> SVar <$> newID "_b" <*> pure (Self:xs))

genDynType :: TyEnv -> SimpleType -> InferM Type
genDynType env = genTypeBase env (\_ -> return (SBool True))

decomposeConstraints :: Constraints -> InferM (Subst, [VC])
decomposeConstraints c@(Con c_sh subty sigma) = do
  tellLn ("decomposeConstraints:\n" ++ pretty c)
  -- 1. decompose subtype constraints
  let subtype = map (\(vars, fs, t1, t2, pos) -> (map (substID sigma) vars, map (subst sigma) fs, f t1, f t2, pos)) subty
                  where f = subst sigma
  c_subtype <- lift $ subtypeToVC subtype
  -- 2. solve type and shape constraints
  (sigmaU, c_left) <- solve (map (subst sigma) (c_sh ++ c_subtype))
  return (sigmaU `compose` sigma, c_left)

inferProg
  :: TyEnv
  -> Context
  -> Constraints
  -> IProgram ID SimpleType
  -> InferM (TyEnv, Context, Constraints, IProgram ID ())
inferProg env ctx c []     = return (env, ctx, c, [])
inferProg env ctx c (d:ds) = do
  d <- mapTypeM (pure . subst (substitution c)) d
  (env', ctx', c', d') <- inferDecl env ctx d
  -- Substitute global constants.
  let (sigma, _) = solveName (extractCtx' env')
  let env'' = updateMapping (subst sigma) env'
  (env, ctx, c, ds') <- inferProg env'' ctx' (c' `appendC` c) ds
  return (env, ctx, c, d' : ds')

inferDecl
  :: TyEnv
  -> Context
  -> IDecl ID SimpleType
  -> InferM (TyEnv, Context, Constraints, IDecl ID ())
inferDecl env ctx (ILet pattern expr pos) = do
  (ty, c, e') <- inferExpr env ctx expr
  (env_new, tyC_bind) <- matchPattern env pattern ty
  let tyC_bind' = map (\(t1, t2) -> (depId env, ctx, t1, t2, pos)) tyC_bind
  (sigma, c_leftover) <- decomposeConstraints (tyC_bind' `addSubtype` c)

  let env' = updateMapping (subst sigma) env
  let env_new' = map (\(x, t) -> VarDef x (generalize (extractMapping env') (simplifyPredInType (depId env') (subst sigma t)))) env_new
  let p = extractCtx' env_new'
  let c = Con c_leftover [] sigma
  return (env_new' ++ env', p ++ map (subst sigma) ctx, c, ILet pattern e' pos)

inferDecl env ctx (ILetRec f mt t expr pos) = do
  (fv, ty) <- case mt of
                Nothing -> ([],) <$> genType env t
                Just t' -> return (nub (fsv t'), t')
  tellLn ("type at ILetRec: " ++ pretty ty)
  (c, e') <- checkExpr (VarDef f (fv, ty) : env) ctx expr ty pos
  (sigma, c_leftover) <- decomposeConstraints c
  let c = Con c_leftover [] sigma
  let env' = updateMapping (subst sigma) env
  let ty' = generalize (extractMapping env') (simplifyPredInType (depId env') (subst sigma ty))
  return (VarDef f ty' : env', map (subst sigma) ctx, c, ILetRec f mt () e' pos)

inferDecl env ctx (IOpen name) = do
  case lookupModule name env of
    Just m -> do
      let p = extractCtx' m
      return (m ++ env, p ++ ctx, emptyC, IOpen name)
    _ -> do
      liftIO $ putStrLn ("\ESC[33mWarning: Unknown module " ++ name ++ "\ESC[39m")
      return (env, ctx, emptyC, IOpen name)

inferDecl env ctx (IModule x ds) = do
  (env', ctx', c, ds') <- inferProg env ctx emptyC ds
  let envDiff = take (length env' - length env) env'
  return (Module x (addPath [x] envDiff) : env, ctx', c, IModule x ds')
inferDecl env ctx (ILetModule x path) =
  case findPath env path of
    Just m  -> return (Module x m : env, ctx, emptyC, ILetModule x path)
    Nothing -> return (env, ctx, emptyC, ILetModule x path)
inferDecl env ctx (ITypeDef xx td) =
  return (TyDef xx td : env, ctx, emptyC, ITypeDef xx td)

inferExpr
  :: TyEnv
  -> Context
  -> IExpr ID SimpleType
  -> InferM (Type, Constraints, IExpr ID ())
inferExpr _ _ IUnit = return (TyUnit, emptyC, IUnit)
inferExpr _ _ (IConst c) = return (typeOfConst c, emptyC, IConst c)
inferExpr env _ (IVar x t) = do
  ty <- case lookupVar x env of
          Just ty -> do ty <- instantiate (depId env) ty
                        tellLn ("instantiate " ++ pretty x ++ ": " ++ pretty ty)
                        return ty
          Nothing -> genDynType env t
  let ty' = case ty of
              TyRef TyTensor _ -> TyRef TyTensor (SEq (SShape Self) (SShape x))
              TyRef (TyUser xx) _ ->
                case findPath env (path xx) >>= lookupType xx of
                  Just (Record xs _) ->
                    TyRef (TyUser xx)
                          (mkAnd (map (\f -> SEq (SName Self [f]) (SName x [f])) xs))
                  _ -> TyRef (TyUser xx) (SBool True) -- float, etc.
              TyRef b _ -> TyRef b (SEq (SName Self []) (SName x []))
              _ -> ty
  return (ty', emptyC, IVar x ())
inferExpr env _   (ICtor x Nothing t) =
  (, emptyC, ICtor x Nothing ()) <$> genDynType env t
inferExpr env ctx (ICtor x (Just (pos, e)) t) = do
  ty <- genDynType env t
  (_, c, e') <- inferExpr env ctx e
  return (ty, c, ICtor x (Just (pos, e')) ())
inferExpr _   _   (ITag x Nothing) = return (TyString, emptyC, ITag x Nothing)
inferExpr env ctx (ITag x (Just e)) = do
  (_, c, e') <- inferExpr env ctx e
  return (TyString, c, ITag x (Just e'))
inferExpr env ctx (ITuple es) = do
  (ts, cs, es') <- unzip3 <$> mapM (inferExpr env ctx) es
  return (TyTuple ts, concatC cs, ITuple es')
inferExpr env _ (INil t) = do
  ty <- genType env (STyList t)
  let ty' = case ty of
              TyRef t@(TyList (TyRef TyInt _)) _ -> TyRef t (SEq (SName Self []) (SList []))
              _ -> ty
  return (ty', emptyC, INil ())
inferExpr env ctx (ICons pos e1 e2) = do
  (t1, c1, e1') <- inferExpr env ctx e1
  (t2, c2, e2') <- inferExpr env ctx e2
  let e' = ICons pos e1' e2'
  let vars = depId env
  case (t1, t2) of
    (TyRef TyInt p, TyRef (TyList t) q) -> do
      x <- newID "x"
      y <- newID "y"
      let t3 = TyRef (TyList t) (mkAnd [SEq (SName Self []) (SFun "cons" [SName x [], SName y []])])
      let vc = [Impl vars [] [] [rename Self x p, rename Self y q]]
      return (t3, Con vc [(vars, ctx, t1, t, pos)] emptyS `appendC` c2 `appendC` c1, e')
    (_, TyRef (TyList t) _) ->
      return (t2, [(vars, ctx, t1, t, pos)] `addSubtype` c2 `appendC` c1, e')
    _ -> throwError "Invalid type for ICons"
inferExpr env ctx (IRecord pos es) = do
  (xs, rs) <- unzip <$> mapM (\(x, e) -> (x,) <$> inferExpr env ctx e) es
  let (ts, cs, es') = unzip3 rs
  case find (\case { TyDef _ (Record _ fs) -> map fst fs == xs; _ -> False }) env of
    Just (TyDef xx (Record ys ts')) -> do
      ys' <- mapM newID ys
      ts' <- mapM (instantiateTy [] (map (\(y, n) -> ((y, 0), n)) ys') . snd) ts'
      let subty = zipWith (depId env,ctx,,,pos) ts ts'
      let ps = zipWith (\x f -> SEq (SName Self [f]) (SName x [])) ys' ys
      return (TyRef (TyUser xx) (mkAnd ps),
              subty `addSubtype` concatC cs, IRecord pos (zip xs es'))
    _ -> throwError ("Unknown fields: " ++ intercalate ", " xs)
inferExpr env ctx (IField pos e x) = do
  (t, c, e') <- inferExpr env ctx e
  case t of
    TyRef (TyUser xx) _ ->
      case lookupType xx env of
        Just (Record params (lookup x -> Just ft)) -> do
          var <- newID "r"
          let t' = TyRef (TyUser xx) (mkAnd (map (\f -> SEq (SName var [f]) (SName Self [f])) params))
          let ft' = subst (map (\f -> (N (f, 0) [], SName var [f])) params) ft
          let subty = [(depId env, ctx, t, t', (0, 0))]
          return (ft', subty `addSubtype` c, IField pos e' x)
        _ -> throwError ("Failed to find type " ++ pretty xx ++ " that has field " ++ x)
    _ -> undefined
inferExpr env ctx e@(IIf pos _ _ _ t) = do
  ty <- genType env t
  tellLn ("genType at IIf: " ++ pretty ty)
  (c, e') <- checkExpr env ctx e ty pos
  return (ty, c, e')
inferExpr env ctx (IFor pos x e1 e2 e3) = do
  (c1, e1') <- checkExpr env ctx e1 (TyRef TyInt (SBool True)) pos
  (c2, e2') <- checkExpr env ctx e2 (TyRef TyInt (SBool True)) pos
  t <- genType env STyInt
  (c3, e3') <- checkExpr (VarDef x ([], t) : env) ctx e3 TyUnit pos
  return (TyUnit, c3 `appendC` c2 `appendC` c1, IFor pos x e1' e2' e3')
inferExpr env ctx (ILetIn decl e) = do
  (env', ctx', c1, d') <- inferDecl env ctx decl
  (t, c2, e') <- inferExpr env' ctx' e
  if null (baseTypeId env' \\ baseTypeId env)
     then return (t, c2 `appendC` c1, ILetIn d' e')
     else do
       t' <- genType env (tyToSimpleTy t)
       tellLn ("genType at ILetIn: " ++ pretty t')
       return (t', [(depId env, ctx', t, t', (0, 0))] `addSubtype` c2 `appendC` c1, ILetIn d' (ICast e' ctx' t t' (0, 0)))
inferExpr env ctx (IFun pos ml p t e) = do
  t1 <- genType env t
  tellLn ("genType at IFun: " ++ pretty t1)
  (env', tyC_bind) <- matchPattern env p t1
  let tyC_bind' = map (\(t1, t2) -> (depId env, ctx, t1, t2, pos)) tyC_bind
  let ctx' = extractCtx env'
  (t2, c2, e') <- inferExpr (map (T.second ([],)) env' `addMapping` env) (ctx' ++ ctx) e
  let mx = case p of { PVar x -> Just x; PType x _ -> Just x; _ -> Nothing }
  return (TyFun ml mx t1 t2, tyC_bind' `addSubtype` c2, IFun pos ml p () e')
inferExpr env ctx (IOptFun x Nothing t e) = do
  t1 <- genType env t
  (t2, c2, e') <- inferExpr (VarDef x ([], TyOption t1) : env) ctx e
  return (TyOptionFun x t1 Nothing t2, c2, IOptFun x Nothing () e')
inferExpr env ctx (IOptFun x (Just s) t e) = do
  (t1', c1, s') <- inferExpr env ctx s
  t1 <- genType env t
  let ctx' = extractCtx [(x, t1)]
  (t2, c2, e') <- inferExpr (VarDef x ([], t1) : env) (ctx' ++ ctx) e
  return (TyOptionFun x t1 (Just t1') t2, c2 `appendC` c1, IOptFun x (Just s') () e')

inferExpr env ctx (IApp label pos e1 e2) = do
  (t1, c1, e1') <- inferExpr env ctx e1
  (x, vars, tyA, tyR, c3) <- loop label (depId env) t1
  (t2, c2, e2') <- inferExpr env ctx e2
  return ( tyR
         , apply x vars tyA t2 `appendC` c3 `appendC` c2 `appendC` c1
         , IApp label pos e1' (ICast e2' ctx t2 tyA pos) )
 where
  loop :: Maybe String -> [ID] -> Type
       -> InferM (Maybe ID, [ID], Type, Type, Constraints)
  loop Nothing  vars (TyFun Nothing mx t1 t2) = return (mx, vars, t1, t2, emptyC)
  loop (Just y) vars (TyFun (Just l) mx t1 t2) | l == y = return (mx, vars, t1, t2, emptyC)
  loop label    vars (TyFun ml mx t1 t2) = do
    (y, vars', tyA, tyR, c) <- loop label vars t2
    return (y, vars', tyA, TyFun ml mx t1 tyR, c)
  loop (Just y) vars (TyOptionFun x t1 _ t2) | fst x == y =
    return (Just x, vars, t1, t2, emptyC)
  loop label    vars (TyOptionFun x t1 s t2) = do
    (y, vars', tyA, tyR, c) <- loop label vars t2
    return (y, vars', tyA, TyOptionFun x t1 s tyR, c)
  loop _ _ _ = throwError "loop"

  apply Nothing  vars tyA t2 = Con [] [(vars, ctx, t2, tyA, pos)] emptyS
  apply (Just x) vars tyA t2 =
    Con [Impl vars [] [] (extractCtx [(x, t2)])] [(vars, ctx, t2, tyA, pos)] emptyS

inferExpr env ctx (IAppEnd e) = do
  (t, c1, e') <- inferExpr env ctx e
  let (c2, t') = removeOpt t
  return (t', c2 `appendC` c1, IAppEnd e')
 where
  removeOpt (TyOptionFun _ _ Nothing t2) = removeOpt t2
  removeOpt (TyOptionFun x t1 (Just s) t2) =
    let vars = depId env
        c = Con [Impl vars [] [] (extractCtx [(x, s)])] [(vars, ctx, s, t1, (0, 0))] emptyS
     in T.first (c `appendC`) (removeOpt t2)
  removeOpt t = (emptyC, t)
inferExpr env ctx (IMatch pos e1 [(p, e2)] _) = do
  (t1, c1, e1') <- inferExpr env ctx e1
  (env', c2) <- matchPattern env p t1
  let c2' = map (\(t1, t2) -> (depId env, ctx, t1, t2, pos)) c2
  (t2, c3, e2') <- inferExpr (map (T.second ([],)) env' `addMapping` env) (extractCtx env' ++ ctx) e2
  return (t2, c2' `addSubtype` c3 `appendC` c1, IMatch pos e1' [(p, e2')] ())
inferExpr env ctx e@(IMatch pos _ _ t) = do
  ty <- genType env t
  (c, e') <- checkExpr env ctx e ty pos
  return (ty, c, e')
inferExpr env ctx (ISeq pos e1 e2) = do
  (c1, e1') <- checkExpr env ctx e1 TyUnit pos
  (t2, c2, e2') <- inferExpr env ctx e2
  return (t2, c2 `appendC` c1, ISeq pos e1' e2')
inferExpr env ctx (IType pos e t) = do
  (c, e') <- checkExpr env ctx e t pos
  return (t, c, e')
inferExpr _ _ ICast{} = undefined
inferExpr _ _ IAssert{} = undefined


checkExpr
  :: TyEnv
  -> Context
  -> IExpr ID SimpleType
  -> Type
  -> Pos
  -> InferM (Constraints, IExpr ID ())
-- ETuple
checkExpr env ctx (IIf pos1 x e1 e2 _) ty pos2 = do
  (c1, x') <- checkExpr env ctx x (TyRef TyBool (SBool True)) pos1
  let (ctx1, ctx2) = case x of
                       IVar y _ -> (SName y [] : ctx, SNot (SName y []) : ctx)
                       _ -> (ctx, ctx)
  (c2, e1') <- checkExpr env ctx1 e1 ty pos2
  (c3, e2') <- checkExpr env ctx2 e2 ty pos2
  return (c3 `appendC` c2 `appendC` c1, IIf pos1 x' e1' e2' ())
checkExpr env ctx (ILetIn decl e) ty pos = do
  (env', ctx', c1, d') <- inferDecl env ctx decl
  (c2, e') <- checkExpr env' ctx' e ty pos
  return (c2 `appendC` c1, ILetIn d' e')
checkExpr env ctx (IFun pos1 ml p _ e) (TyFun _ my t1 t2) pos = do
  let mx = case p of { PVar x -> Just x; PType x _ -> Just x; _ -> Nothing }
  let vc = case (mx, my) of
             (Just x, Just y) -> [Impl (depId env) [] [] [SEq (SName y []) (SName x [])]]
             _ -> []
  (env', tyC) <- matchPattern env p t1
  let c' = map (\(t1, t2) -> (depId env, ctx, t1, t2, pos)) tyC
  (c, e') <- checkExpr (map (T.second ([],)) env' `addMapping` env) (extractCtx env' ++ ctx) e t2 pos
  return (c' `addSubtype` (vc `addVC` c), IFun pos1 ml p () e')
checkExpr env ctx (IOptFun x Nothing _ e) (TyOptionFun y t1 Nothing t2) pos = do
  (c1, e') <- checkExpr (VarDef x ([], t1) : env) (extractCtx [(x, t1)] ++ ctx) e t2 pos
  return ( [Impl (depId env) [] [] [SEq (SName y []) (SName x [])]] `addVC` c1
         , IOptFun x Nothing () e')
checkExpr env ctx (IOptFun x (Just s) _ e) (TyOptionFun y t1 (Just ts) t2) pos = do
  (c1, e') <- checkExpr (VarDef x ([], t1) : env) (extractCtx [(x, t1)] ++ ctx) e t2 pos
  (c2, s') <- checkExpr env ctx s ts pos
  return ( [Impl (depId env) [] [] [SEq (SName y []) (SName x [])]] `addVC` (c1 `appendC` c2)
         , IOptFun x (Just s') () e')
checkExpr env ctx (IMatch pos1 e1 clauses _) ty pos2 = do
  (t1, c1, e1') <- inferExpr env ctx e1
  (cs, clauses') <- unzip <$> forM clauses (\(p, e2) -> do
    (env', c2) <- matchPattern env p t1
    let c2' = map (\(t1, t2) -> (depId env, ctx, t1, t2, pos1)) c2
    (c3, e2') <- checkExpr (map (T.second ([],)) env' `addMapping` env) (extractCtx env' ++ ctx) e2 ty pos2
    return (c2' `addSubtype` c3, (p, e2')))
  return (concatC cs `appendC` c1, IMatch pos1 e1' clauses' ())
checkExpr env ctx (ISeq pos1 e1 e2) ty pos2 = do
  (c1, e1') <- checkExpr env ctx e1 TyUnit pos1
  (c2, e2') <- checkExpr env ctx e2 ty pos2
  return (c2 `appendC` c1, ISeq pos1 e1' e2')
checkExpr env ctx e ty pos = do
  (ty', c, e') <- inferExpr env ctx e
  return ([(depId env, ctx, ty', ty, pos)] `addSubtype` c, ICast e' ctx ty' ty pos)


matchPattern :: TyEnv -> Pattern ID -> Type -> InferM ([(ID, Type)], [(Type, Type)])
matchPattern _ PWildcard     _  = return ([], [])
matchPattern _ (PConst expr) ty = return ([], [(typeOfConst expr, ty)])
matchPattern _ (PVar x)      ty = return ([(x, ty)], [])
matchPattern _ PUnit         ty = return ([], [(ty, TyUnit)])
matchPattern _ (PTag _)      ty = return ([], [(ty, TyString)])
matchPattern env (PTuple ps) (TyTuple ts) = do
  (envs, cs) <- unzip <$> zipWithM (matchPattern env) ps ts
  return (concat envs, concat cs)
matchPattern env (PRecord [] fs) t@(TyRef (TyUser xx) _) =
  case lookupType xx env of
    Just r@Record{} -> do
      var <- newID "r"
      let t' = TyRef (TyUser xx) (mkAnd (map (\f -> SEq (SName var [f]) (SName Self [f])) (params r)))
      (envs, cs) <- unzip <$> mapM (findType r var) fs
      return (concat envs, (t, t') : concat cs)
    _ -> throwError ("Unknown record type in matchPattern: " ++ pretty xx)
 where
  findType r var (y, pat) =
    case lookup y (fields r) of
      Nothing -> throwError ("Unknown record field in matchPattern: " ++ y)
      Just (TyRef bt p) ->
        matchPattern env pat (TyRef bt (subst (map (\f -> (N (f, 0) [], SName var [f])) (params r)) p))
      Just t -> matchPattern env pat t
matchPattern env (PRecord (p:path) fs) ty =
  case lookupModule p env of
    Just env' -> matchPattern env' (PRecord path fs) ty
    _ -> throwError ("Unknown module: " ++ p)
matchPattern _ (PType x t1) t2 = return ([(x, t1)], [(t2, t1)])
matchPattern _ PNil (TyRef TyList{} _) = return ([], [])
matchPattern env (PCons p1 p2) (TyRef (TyList t) _) = do
  (env1, c1) <- matchPattern env p1 t
  (env2, c2) <- matchPattern env p2 (TyRef (TyList t) (SBool True))
  return (env2 ++ env1, c2 ++ c1)
matchPattern _   (PConstr "None" Nothing)  (TyOption _) = return ([], [])
matchPattern env (PConstr "Some" (Just p)) (TyOption t) = matchPattern env p t
matchPattern env (PConstr x mp) (TyRef (TyUser xx) _) =
  case lookupType xx env of
    Just (Variant tags) ->
      case (lookup x tags, mp) of
        (Just Nothing, Nothing) -> return ([], [])
        (Just (Just t), Just p) -> matchPattern env p t
        _ -> throwError "Unreachable in matchPattern (Infer.hs)"
    _ -> throwError ("Unknown variant type in matchPattern: " ++ pretty xx)
matchPattern _ _ _ = throwError "Type mismatch in pattern"

baseTypeId :: TyEnv -> [ID]
baseTypeId []                              = []
baseTypeId (VarDef x (_, TyRef{}) : env)   = x : baseTypeId env
baseTypeId (VarDef x (_, TyTuple{}) : env) = x : baseTypeId env
baseTypeId (_ : env)                       = baseTypeId env

depId :: TyEnv -> [ID]
depId []                                                          = []
depId (VarDef _ (_, TyRef _ (SEq (SName Self []) SInt{}))  : env) = depId env
depId (VarDef _ (_, TyRef _ (SEq (SName Self []) SBool{})) : env) = depId env
depId (VarDef x (_, TyRef{}) : env)                               = x : depId env
depId (VarDef x (_, TyTuple{}) : env)                             = x : depId env
depId (_ : env)                                                   = depId env

extractMapping :: TyEnv -> [(ID, TyScheme)]
extractMapping []                  = []
extractMapping (VarDef x ts : env) = (x, ts) : extractMapping env
extractMapping (_ : env)           = extractMapping env
