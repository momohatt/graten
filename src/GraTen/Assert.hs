{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module GraTen.Assert
  ( insertAssert
  ) where

import           Data.List               (intercalate)
import           Data.Maybe              (isNothing)
import qualified Data.Tuple.Extra        as T

import           GraTen.CmdOptions
import           GraTen.Infer.Constraint
import           GraTen.Infer.InferM
import           GraTen.Infer.Solve
import qualified GraTen.Infer.Subst      as S
import           GraTen.Infer.Subtype
import           GraTen.Infer.Z3
import           GraTen.Pretty
import           GraTen.Syntax

-- | Converts program with ICast to program with assertions.
insertAssert :: IProgram ID () -> InferM (IProgram ID ())
insertAssert = mapM icDecl

icDecl :: IDecl ID () -> InferM (IDecl ID ())
icDecl (ILet pattern e p)   = ILet pattern <$> icExpr e <*> pure p
icDecl (ILetRec f mt _ e p) = ILetRec f mt () <$> icExpr e <*> pure p
icDecl (IModule x ds)       = IModule x <$> mapM icDecl ds
icDecl d                    = return d

icExpr :: IExpr ID () -> InferM (IExpr ID ())
icExpr (ICtor x me _)  = ICtor x <$> mapM (T.secondM icExpr) me <*> pure ()
icExpr (ITag x me)     = ITag x <$> mapM icExpr me
icExpr (ITuple es)     = ITuple <$> mapM icExpr es
icExpr (IRecord p es)  = IRecord p <$> mapM (T.secondM icExpr) es
icExpr (IField p e x)  = IField p <$> icExpr e <*> pure x
icExpr (ICons p e1 e2) = ICons p <$> icExpr e1 <*> icExpr e2
icExpr (IIf p e0 e1 e2 _) =
  IIf p <$> icExpr e0 <*> icExpr e1 <*> icExpr e2 <*> pure ()
icExpr (IFor p i e0 e1 e2) =
  IFor p i <$> icExpr e0 <*> icExpr e1 <*> icExpr e2
icExpr (ILetIn d e) = ILetIn <$> icDecl d <*> icExpr e
icExpr (IFun p ml x _ e) = IFun p ml x () <$> icExpr e
icExpr (IOptFun x s _ e) = IOptFun x <$> mapM icExpr s <*> pure () <*> icExpr e
icExpr (IApp ml p e1 e2) = IApp ml p <$> icExpr e1 <*> icExpr e2
icExpr (IAppEnd e) = IAppEnd <$> icExpr e
icExpr (IMatch p e cls _) =
  IMatch p <$> icExpr e <*> mapM (T.secondM icExpr) cls <*> pure ()
icExpr (ISeq p e1 e2) = ISeq p <$> icExpr e1 <*> icExpr e2
icExpr (IType p e ty) = IType p <$> icExpr e <*> pure ty
icExpr (ICast e ctx t1 t2 p) = do
  e <- icExpr e
  n <- genAssert ctx t1 t2 p
  case n of
    Nothing -> return e
    Just n  -> do
      tellLn ("[" ++ intercalate ", " (map pretty ctx) ++ "] |- " ++
        pretty t1 ++ " <: " ++ pretty t2 ++ " " ++ pretty p)
      return (app n e)
icExpr e = return e


genAssert :: Context -> Type -> Type -> (Int, Int) -> InferM (Maybe (IExpr ID ()))
genAssert ctx (TyRef _ p1) (TyRef _ p2) _ = do
  let vc = normalizeImpl [Impl [] ctx [p1] [p2]]
  useZ3 <- lift . lift $ asks optUseZ3
  -- TODO: Use pos information for error message
  b <- if useZ3 then checkSat vc else pure (null vc)
  if b
     then return Nothing
     else return . Just $ lambda Self (assert (snd (solveName [p2])) Self)
genAssert ctx (TyFun _ mx t1 t2) (TyFun _ _ t3 t4) pos = do
  n1 <- genAssert ctx t3 t1 pos
  -- The labels of the two function types must be identical
  n2 <- case mx of
          Just x  -> genAssert (ctx ++ extractCtx [(x, t1), (x, t3)]) t2 t4 pos
          Nothing -> genAssert ctx t2 t4 pos
  case (n1, n2) of
    (Nothing, Nothing) -> return Nothing
    (Nothing, Just n2) -> do
      (f, x) <- (,) <$> newID "~f" <*> newID "~x"
      return . Just $ lambda f (lambda x (app n2 (app (var f) (var x))))
    (Just n1, Nothing) -> do
      (f, x) <- (,) <$> newID "~f" <*> newID "~x"
      return . Just $ lambda f (lambda x (app (var f) (app n1 (var x))))
    (Just n1, Just n2) -> do
      (f, x) <- (,) <$> newID "~f" <*> newID "~x"
      return . Just $ lambda f (lambda x (app n2 (app (var f) (app n1 (var x)))))
genAssert ctx (TyOptionFun x t1 _ t2) (TyOptionFun _ t3 _ t4) pos = do
  n1 <- genAssert ctx t3 t1 pos
  -- The labels of the two function types must be identical
  n2 <- genAssert (ctx ++ extractCtx [(x, t1), (x, t3)]) t2 t4 pos
  case (n1, n2) of
    (Nothing, Nothing) -> return Nothing
    (Nothing, Just n2) -> do
      (f, x) <- (,) <$> newID "~f" <*> newID "~x"
      return . Just $ lambda f (lambda x (app n2 (app (var f) (var x))))
    (Just n1, Nothing) -> do
      (f, x) <- (,) <$> newID "~f" <*> newID "~x"
      return . Just $ lambda f (lambda x (app (var f) (app n1 (var x))))
    (Just n1, Just n2) -> do
      (f, x) <- (,) <$> newID "~f" <*> newID "~x"
      return . Just $ lambda f (lambda x (app n2 (app (var f) (app n1 (var x)))))
genAssert ctx (TyTuple ts1) (TyTuple ts2) pos = do
  ns <- zipWithM (\t1 t2 -> genAssert ctx t1 t2 pos) ts1 ts2
  if all isNothing ns
     then return Nothing
     else
     let xs = map (\i -> ("~x" ++ show i, 0)) [0 .. length ns - 1]
         es = map (\case (Nothing, x) -> var x
                         (Just n, x) -> app n (var x)) (zip ns xs)
      in return . Just $ IFun (0, 0) Nothing (PTuple (map PVar xs)) () (ITuple es)
genAssert ctx (TyOption t1) (TyOption t2) pos = do
  n <- genAssert ctx t1 t2 pos
  case n of
    Nothing -> return Nothing
    Just n' -> do
      x <- newID "~x"
      return . Just $ lambda x (IMatch (0, 0) (var x)
        [(PConstr "None" Nothing, (var x)), (PConstr "Some" (Just (PVar x)), (app n' (var x)))] ())
genAssert _ TyUnit TyUnit _ = return Nothing
genAssert _ _ _ _ = undefined

var :: ID -> IExpr ID ()
var x = IVar x ()

app :: IExpr ID () -> IExpr ID () -> IExpr ID ()
app (IFun _ _ (PVar x) _ e) (IVar y _) = rename x y e
app e1 e2                              = IApp Nothing (0, 0) e1 e2

lambda :: ID -> IExpr ID () -> IExpr ID ()
lambda x e = IFun (0, 0) Nothing (PVar x) () e

assert :: [Predicate] -> ID -> IExpr ID ()
assert p x = ISeq (0, 0) (IAssert (mkAnd p)) (var x)

rename :: ID -> ID -> IExpr ID () -> IExpr ID ()
rename x y (IVar z _) = if z == x then IVar y () else IVar z ()
rename x y (ITuple es) = ITuple (map (rename x y) es)
rename x y (IApp ml pos e1 e2) = IApp ml pos (rename x y e1) (rename x y e2)
rename x _ e@(IFun _ _ (PVar z) _ _) | z == x = e
rename x y (IFun pos ml p _ e) = IFun pos ml p () (rename x y e)
rename x y (ISeq pos e1 e2)    = ISeq pos (rename x y e1) (rename x y e2)
rename x y (IAssert p) = IAssert (S.rename x y p)
rename _ _ e           = e -- Other constructs are not used in assertions
