{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}

module GraTen.SimpleType.Syntax
  ( SimpleType (..)
  , pattern STyString
  , STyEnv
  , typeOfConst
  , tyToSimpleTy
  , tyenvToStyenv
  ) where

import           GraTen.Syntax.Literal (ConstantLiteral (..))
import           GraTen.Syntax.Type    (ID, Type (..), BaseType_ (..))
import qualified GraTen.Syntax.Type    as R

data SimpleType
  = STyUnit
  | STyInt
  | STyBool
  | STyFun (Maybe String) (Maybe ID) SimpleType SimpleType
  | STyOptFun ID SimpleType SimpleType
  | STyTuple [SimpleType]
  | STyOption SimpleType
  | STyList SimpleType
  | STyVar Int
  | STyUser R.TyName
  | STyTensor
  deriving (Show)

pattern STyString :: SimpleType
pattern STyString = STyUser (R.TyName [] "string")

type STyEnv = [R.Def SimpleType SimpleType]

typeOfConst :: ConstantLiteral -> SimpleType
typeOfConst (LInt _)    = STyInt
typeOfConst (LBool _)   = STyBool
typeOfConst (LFloat _)  = STyUser (R.TyName [] "float")
typeOfConst (LChar _)   = STyUser (R.TyName [] "char")
typeOfConst (LString _) = STyString

tyToSimpleTy :: Type -> SimpleType
tyToSimpleTy (TyRef TyInt _)         = STyInt
tyToSimpleTy (TyRef TyBool _)        = STyBool
tyToSimpleTy (TyRef TyTensor _)      = STyTensor
tyToSimpleTy (TyRef (TyList t) _)    = STyList (tyToSimpleTy t)
tyToSimpleTy (TyRef (TyUser xx) _)   = STyUser xx
tyToSimpleTy (TyFun ml mx t1 t2)     = STyFun ml mx (tyToSimpleTy t1) (tyToSimpleTy t2)
tyToSimpleTy (TyOptionFun l t1 _ t2) = STyOptFun l (tyToSimpleTy t1) (tyToSimpleTy t2)
tyToSimpleTy TyUnit         = STyUnit
tyToSimpleTy (TyTuple ts)   = STyTuple (map tyToSimpleTy ts)
tyToSimpleTy (TyOption t)   = STyOption (tyToSimpleTy t)

tyenvToStyenv :: R.TyEnv -> STyEnv
tyenvToStyenv = map (\case
    R.VarDef x (_, t) -> R.VarDef x (tyToSimpleTy t)
    R.TyDef xx td     -> R.TyDef xx (tyToSimpleTy <$> td)
    R.Module x env    -> R.Module x (tyenvToStyenv env))
