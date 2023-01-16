{-# LANGUAGE TupleSections #-}

module GraTen.Desugar
  ( desugar
  , undesugar
  ) where

import qualified Data.Tuple.Extra         as T

import           GraTen.Syntax

desugar :: Program String -> IProgram String ()
desugar = map desugarD

desugarD :: Decl String -> IDecl String ()
desugarD (DLet p e pos)      = ILet p (desugarE e) pos
desugarD (DLetRec x t e pos) = ILetRec x t () (desugarE e) pos
desugarD (DOpen x)           = IOpen x
desugarD (DModule x ds)      = IModule x (desugar ds)
desugarD (DLetModule x path) = ILetModule x path
desugarD (DTypeDef xx td)    = ITypeDef xx td

desugarE :: Expr String -> IExpr String ()
desugarE EUnit         = IUnit
desugarE (EConst c)    = IConst c
desugarE (EVar x)      = IVar x ()
desugarE (ECtor x)     = ICtor x Nothing ()
desugarE (ETag x)      = ITag x Nothing
desugarE (ETuple es)   = ITuple (map desugarE es)
desugarE ENil          = INil ()
desugarE (ECons p e1 e2) = ICons p (desugarE e1) (desugarE e2)
desugarE (ERecord p es)  = IRecord p (map (\(x, e) -> (x, desugarE e)) es)
desugarE (EField p e x)  = IField p (desugarE e) x
-- temporarily using lists as arrays
desugarE (EArray p es)   = desugarE (foldr (ECons p) ENil es)
desugarE (EGet p e i) =
  desugarE (EApp Nothing p (EApp Nothing p (EVar "__get__") e) i)
desugarE (EPrefix "-" _ (EConst (LInt n))) = IConst (LInt (-n))
desugarE (EPrefix "-" p e)  = desugarE (EApp Nothing p (EVar "__neg__") e)
desugarE (EPrefix "-." p e) = desugarE (EApp Nothing p (EVar "__fneg__") e)
desugarE (EPrefix op p e) = desugarE (EApp Nothing p (EVar op) e)
desugarE (EInfix "|>" p e1 e2) = desugarE (EAppEnd (EApp Nothing p e2 e1))
desugarE (EInfix op p e1 e2) =
  desugarE (EApp Nothing p (EApp Nothing p (EVar op) e1) e2)
desugarE (EIf p e1@EVar{} e2 e3) =
  IIf p (desugarE e1) (desugarE e2) (desugarE e3) ()
desugarE (EIf p e1 e2 e3) =
  ILetIn (ILet (PVar "~tmp") (desugarE e1) p)
         (IIf p (IVar "~tmp" ()) (desugarE e2) (desugarE e3) ())
desugarE (EFor p i e1 e2 e3) = IFor p i (desugarE e1) (desugarE e2) (desugarE e3)
desugarE (ELetIn d e) = ILetIn (desugarD d) (desugarE e)
desugarE (EFun pos (Required ml p) e) = IFun pos ml p () (desugarE e)
desugarE (EFun _ (Optional x e1) e2) =
  IOptFun x (desugarE <$> e1) () (desugarE e2)
desugarE (EAppEnd (EApp _ p (ECtor x) e)) = ICtor x (Just (p, desugarE e)) ()
desugarE (EAppEnd (EApp _ _ (ETag x) e))  = ITag x (Just (desugarE e))
desugarE (EApp ml pos e1 e2) =
  case desugarE e2 of
    e2'@IConst{} -> IApp ml pos (desugarE e1) e2'
    e2'@IVar{}   -> IApp ml pos (desugarE e1) e2'
    e2'@INil{}   -> IApp ml pos (desugarE e1) e2'
    e2'@ICons{}  -> IApp ml pos (desugarE e1) e2'
    e2'@IFun{}   -> IApp ml pos (desugarE e1) e2'
    e2'@ITag{}   -> IApp ml pos (desugarE e1) e2'
    e2'@ICtor{}  -> IApp ml pos (desugarE e1) e2'
    e2'@ITuple{} -> IApp ml pos (desugarE e1) e2'
    e2' -> ILetIn (ILet (PVar "~tmp") e2' pos) (IApp ml pos (desugarE e1) (IVar "~tmp" ()))
desugarE (EAppEnd e) = f (desugarE e)
  where
    f (ILetIn d e) = ILetIn d (f e)
    f e            = IAppEnd e
desugarE (EMatch p e clauses) =
  IMatch p (desugarE e) (map (\(p, e) -> (p, desugarE e)) clauses) ()
desugarE (ESeq p e1 e2) = ISeq p (desugarE e1) (desugarE e2)
desugarE (EType pos e t)  = IType pos (desugarE e) t

-- Remove unnecessary definition of variables introduced by desugar.
undesugar :: IDecl ID () -> IDecl ID ()
undesugar (ILet pattern e p)   = ILet pattern (undesugarE e) p
undesugar (ILetRec f mt _ e p) = ILetRec f mt () (undesugarE e) p
undesugar (IModule x ds)       = IModule x (map undesugar ds)
undesugar d                    = d

undesugarE :: IExpr ID () -> IExpr ID ()
undesugarE (ICtor x me _)  = ICtor x (T.second undesugarE <$> me) ()
undesugarE (ITag x me)     = ITag x (undesugarE <$> me)
undesugarE (ITuple es)     = ITuple (map undesugarE es)
undesugarE (IIf p e0 e1 e2 _) =
  IIf p (undesugarE e0) (undesugarE e1) (undesugarE e2) ()
undesugarE (IFor p i e0 e1 e2) =
  IFor p i (undesugarE e0) (undesugarE e1) (undesugarE e2)
undesugarE (ILetIn (ILet (PVar x@("~tmp", _)) e1 _) (IApp ml pos e2 (IVar y _)))
  | x == y =
    undesugarE (IApp ml pos e2 e1)
undesugarE (ILetIn (ILet (PVar x@("~tmp", _)) e1 _) (IIf p (IVar y _) e2 e3 _))
  | x == y =
    undesugarE (IIf p e1 e2 e3 ())
undesugarE (ILetIn d e) = ILetIn (undesugar d) (undesugarE e)
undesugarE (IFun p ml x _ e) = IFun p ml x () (undesugarE e)
undesugarE (IOptFun x s _ e) = IOptFun x (undesugarE <$> s) () (undesugarE e)
undesugarE (IApp ml p e1 e2) = IApp ml p (undesugarE e1) (undesugarE e2)
undesugarE (IAppEnd e) = IAppEnd (undesugarE e)
undesugarE (IMatch p e cls _) =
  IMatch p (undesugarE e) (map (\(p, e) -> (p, undesugarE e)) cls) ()
undesugarE (ISeq p e1 e2) = ISeq p (undesugarE e1) (undesugarE e2)
undesugarE (IType p e ty) = IType p (undesugarE e) ty
undesugarE e = e
