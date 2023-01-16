{-# LANGUAGE TupleSections #-}

module GraTen.Syntax.Expr
  ( Program
  , Decl(..)
  , Expr(..)
  , Arg(..)
  , Pattern(..)
  , mapTypeP
  ) where

import GraTen.Syntax.Literal
import GraTen.Syntax.Type

type Program a = [Decl a]

data Decl a
  = DLet (Pattern a) (Expr a) Pos
  | DLetRec a (Maybe Type) (Expr a) Pos
  | DOpen String
  | DModule String (Program a) -- module XX = struct ... end
  | DLetModule String [String] -- module XX = YY.ZZ.WW
  -- Type declaration without its implementation (e.g., 'type t') is expressed
  -- as (DTypeDef xx (TypeDef (Variant []))
  | DTypeDef TyName (TypeDef Type)
  deriving (Show)

data Expr a
  = EUnit
  | EConst ConstantLiteral
  | EVar a
  -- | Value constructor (without argument)
  | ECtor String
  -- | Polymorphic Variants (ex. `A)
  | ETag String
  | ETuple [Expr a]
  | ERecord Pos [(String, Expr a)]
  | EField Pos (Expr a) String
  | ENil
  | ECons Pos (Expr a) (Expr a)
  | EArray Pos [Expr a]
  | EGet Pos (Expr a) (Expr a)
  | EPrefix String Pos (Expr a)
  | EInfix String Pos (Expr a) (Expr a)
  | EIf Pos (Expr a) (Expr a) (Expr a)
  | EFor Pos a (Expr a) (Expr a) (Expr a)
  | ELetIn (Decl a) (Expr a)
  | EFun Pos (Arg a) (Expr a)
  | EApp (Maybe String) Pos (Expr a) (Expr a)
  -- | Marks the end of the argument sequence in the function applications.
  --   e.g., (f e1) e2 -> EApp (EAppEnd (EApp f e1)) e2
  --         f e1 e2   -> EApp (EApp f e1) e2
  --   The optional arguments that is not provided until EAppEnd is reduced.
  | EAppEnd (Expr a)
  | EMatch Pos (Expr a) [(Pattern a, Expr a)]
  | ESeq Pos (Expr a) (Expr a)
  | EType Pos (Expr a) Type
  deriving (Show)

data Arg a
  = Required (Maybe String) (Pattern a)
  | Optional a (Maybe (Expr a))
  deriving (Show)

data Pattern a
  = PWildcard
  | PConst ConstantLiteral
  | PVar a
  | PUnit
  | PTag String
  | PConstr String (Maybe (Pattern a))
  | PTuple [Pattern a]
  | PRecord [String]              -- path of the first field
            [(String, Pattern a)] -- fields
  | PNil
  | PCons (Pattern a) (Pattern a)
  | PType a Type
  deriving (Show)


mapTypeP :: Monad m => (Type -> m Type) -> Pattern a -> m (Pattern a)
mapTypeP f (PTuple ps)       = PTuple <$> mapM (mapTypeP f) ps
mapTypeP f (PRecord path fs) = PRecord path <$> mapM (\(x, p) -> (x,) <$> mapTypeP f p) fs
mapTypeP f (PCons p1 p2)     = PCons <$> mapTypeP f p1 <*> mapTypeP f p2
mapTypeP f (PType x ty)      = PType x <$> f ty
mapTypeP _ p                 = return p
