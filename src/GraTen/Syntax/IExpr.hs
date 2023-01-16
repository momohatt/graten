{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}

module GraTen.Syntax.IExpr
  ( IProgram
  , IDecl (..)
  , IExpr (..)
  , mapTypeM
  ) where

import qualified Data.Tuple.Extra         as T

import           GraTen.Syntax.Literal
import           GraTen.Syntax.Type
import           GraTen.Syntax.Expr    (Pattern (..), mapTypeP)

type IProgram a ty = [IDecl a ty]

data IDecl a ty
  = ILet (Pattern a) (IExpr a ty) Pos
  | ILetRec a (Maybe Type) ty (IExpr a ty) Pos
  | IOpen String
  | IModule String (IProgram a ty)
  | ILetModule String [String]
  | ITypeDef TyName (TypeDef Type)
  deriving (Show, Functor)

data IExpr a ty
  = IUnit
  | IConst ConstantLiteral
  | IVar a ty
  | ICtor String (Maybe (Pos, IExpr a ty)) ty
  | ITag String (Maybe (IExpr a ty))
  | ITuple [IExpr a ty]
  | INil ty
  | ICons Pos (IExpr a ty) (IExpr a ty)
  | IRecord Pos [(String, IExpr a ty)]
  | IField Pos (IExpr a ty) String
  | IIf Pos (IExpr a ty) (IExpr a ty) (IExpr a ty) ty
  | IFor Pos a (IExpr a ty) (IExpr a ty) (IExpr a ty)
  | ILetIn (IDecl a ty) (IExpr a ty)
  | IFun Pos (Maybe String) (Pattern a) ty (IExpr a ty)
  | IOptFun a (Maybe (IExpr a ty)) ty (IExpr a ty)
  | IApp (Maybe String) Pos (IExpr a ty) (IExpr a ty)
  | IAppEnd (IExpr a ty)
  | IMatch Pos (IExpr a ty) [(Pattern a, IExpr a ty)] ty
  | ISeq Pos (IExpr a ty) (IExpr a ty)
  | IType Pos (IExpr a ty) Type
  -- | Internal expression for the 'subsumption marker' in the paper.
  | ICast (IExpr a ty) Context Type Type Pos
  | IAssert Predicate
  deriving (Show, Functor)

mapTypeM :: Monad m => (Type -> m Type) -> IDecl a ty -> m (IDecl a ty)
mapTypeM f (ILet p e pos)         = ILet <$> mapTypeP f p <*> mapType' f e <*> pure pos
mapTypeM f (ILetRec x mt t e pos) = ILetRec x mt t <$> mapType' f e <*> pure pos
mapTypeM f (IModule x ds)         = IModule x <$> mapM (mapTypeM f) ds
mapTypeM _ d                      = return d

mapType' :: Monad m => (Type -> m Type) -> IExpr a ty -> m (IExpr a ty)
mapType' f (ICtor x me t)      = ICtor x <$> mapM (T.secondM (mapType' f)) me <*> pure t
mapType' f (ITag x me)         = ITag x <$> mapM (mapType' f) me
mapType' f (ITuple es)         = ITuple <$> mapM (mapType' f) es
mapType' f (ICons p e1 e2)     = ICons p <$> mapType' f e1 <*> mapType' f e2
mapType' f (IRecord p es)      = IRecord p <$> mapM (T.secondM (mapType' f)) es
mapType' f (IField p e x)      = IField p <$> mapType' f e <*> pure x
mapType' f (IIf p x e1 e2 t)   = IIf p <$> mapType' f x <*> mapType' f e1 <*> mapType' f e2 <*> pure t
mapType' f (IFor p i e1 e2 e3) = IFor p i <$> mapType' f e1 <*> mapType' f e2 <*> mapType' f e3
mapType' f (ILetIn d e)        = ILetIn <$> mapTypeM f d <*> mapType' f e
mapType' f (IFun p l x t e)    = IFun p l x t <$> mapType' f e
mapType' f (IOptFun x e1 t e2) = IOptFun x <$> mapM (mapType' f) e1 <*> pure t <*> mapType' f e2
mapType' f (IApp x p e1 e2)    = IApp x p <$> mapType' f e1 <*> mapType' f e2
mapType' f (IAppEnd e)         = IAppEnd <$> mapType' f e
mapType' f (IMatch p e cls t)  = IMatch p <$> mapType' f e <*> mapM (\(p, e) -> (,) <$> mapTypeP f p <*> mapType' f e) cls <*> pure t
mapType' f (ISeq p e1 e2)      = ISeq p <$> mapType' f e1 <*> mapType' f e2
mapType' f (IType p e t)       = IType p <$> mapType' f e <*> f t
mapType' _ e                   = return e
