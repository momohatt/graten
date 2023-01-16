{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module GraTen.Pretty
  ( Pretty (..)
  ) where

import Data.List                (intercalate)

import GraTen.SimpleType.Syntax
import GraTen.Syntax

class Pretty a where
  pretty :: a -> String

class Complex a where
  isAtom :: a -> Bool

pretty' :: (Pretty a, Complex a) => a -> String
pretty' a | isAtom a = pretty a
pretty' a            = "(" ++ pretty a ++ ")"

instance {-# OVERLAPPABLE #-} Pretty a => Pretty [a] where
  pretty [] = "[]"
  pretty [x] = "[ " ++ pretty x ++ " ]"
  pretty (x:xs) = unlines (("[ " ++ pretty x) : map ((", " ++) . pretty) xs) ++ "]"

instance {-# OVERLAPPABLE #-} (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (x, y) = "(" ++ pretty x ++ ", " ++ pretty y ++ ")"

instance {-# OVERLAPPABLE #-} (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
  pretty (x, y, z) = "(" ++ pretty x ++ ", " ++ pretty y ++ ", " ++ pretty z ++ ")"

instance {-# OVERLAPPABLE #-} (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (a, b, c, d) where
  pretty (x, y, z, w) = "(" ++ pretty x ++ ", " ++ pretty y ++ ", " ++ pretty z ++ ", " ++ pretty w ++ ")"

instance {-# OVERLAPPABLE #-} Show a => Pretty a where
  pretty = show

instance Pretty ConstantLiteral where
  pretty (LInt x)      = show x
  pretty (LBool True)  = "true"
  pretty (LBool False) = "false"
  pretty (LFloat x)    = show x
  pretty (LChar x)     = show x
  pretty (LString x)   = show x

instance Pretty ID where
  pretty (x, 0) = x
  pretty (x, t) = x ++ "_" ++ show t

instance Pretty SimpleType where
  pretty STyUnit   = "unit"
  pretty STyInt    = "int"
  pretty STyBool   = "bool"
  pretty STyTensor = "tensor"
  pretty (STyFun (Just l) _ t1 t2) = "~" ++ l ++ ":" ++ pretty' t1 ++ " -> " ++ pretty t2
  pretty (STyFun _ Nothing t1 t2)  = pretty' t1 ++ " -> " ++ pretty t2
  pretty (STyFun _ (Just x) t1 t2) = pretty x ++ ":" ++ pretty' t1 ++ " -> " ++ pretty t2
  pretty (STyOptFun l t1 t2)       = "?" ++ pretty l ++ ":" ++ pretty' t1 ++ " -> " ++ pretty t2
  pretty (STyTuple ts)   = intercalate " * " (map pretty' ts)
  pretty (STyOption t)   = "option " ++ pretty' t
  pretty (STyList t)     = pretty' t ++ " list"
  pretty (STyVar n)      = "a" ++ show n
  pretty (STyUser name)  = pretty name

instance Pretty Type where
  pretty (TyRef t (SBool True))   = pretty t
  pretty (TyRef t f)              = "{ v:" ++ pretty t ++ " | " ++ pretty f ++ " }"
  pretty (TyFun Just{} (Just x) t1 t2) = "~" ++ pretty x ++ ":" ++ pretty' t1 ++ " -> " ++ pretty t2
  pretty (TyFun _ Nothing t1 t2)  = pretty' t1 ++ " -> " ++ pretty t2
  pretty (TyFun _ (Just x) t1 t2) = pretty x ++ ":" ++ pretty' t1 ++ " -> " ++ pretty t2
  pretty (TyOptionFun x t1 _ t2)  = "?" ++ pretty x ++ ":" ++ pretty' t1 ++ " -> " ++ pretty t2
  pretty TyUnit                   = "unit"
  pretty (TyTuple ts)             = intercalate " * " (map pretty' ts)
  pretty (TyOption t)             = "option " ++ pretty' t

instance Pretty BaseType where
  pretty TyInt        = "int"
  pretty TyBool       = "bool"
  pretty TyTensor     = "tensor"
  pretty (TyList t)   = pretty' t ++ " list"
  pretty (TyUser xx)  = pretty xx

instance Pretty TyName where
  pretty (TyName path name) = intercalate "." (path ++ [name])

instance Complex SimpleType where
  isAtom STyFun{}    = False
  isAtom STyOptFun{} = False
  isAtom STyTuple{}  = False
  isAtom _           = True

instance Complex Type where
  isAtom TyFun{}       = False
  isAtom TyOptionFun{} = False
  isAtom TyTuple{}     = False
  isAtom _             = True

instance Pretty Predicate where
  pretty (SInt n)     = pretty n
  pretty (SBool b)    = if b then "true" else "false"
  pretty (SName x fs) = pretty x ++ concatMap ('.' :) fs
  pretty (SVar x ys)  = '\'' : pretty x ++ "#{" ++ intercalate "," (map (show . snd) ys) ++ "}"
  pretty (SNot s)     = "!(" ++ pretty s ++ ")"
  pretty (SNeg s)     = '-' : pretty s
  pretty (SBi Add s1 (SNeg s2)) = pretty s1 ++ " - "  ++ prettyAt 6 s2
  pretty (SBi Add s (SInt n)) | n < 0 = pretty s ++ " - "  ++ show (-n)
  pretty (SBi Add s1 s2)      = prettyAt 6 s1 ++ " + "  ++ prettyAt 6 s2
  pretty (SBi Mul s1 s2)      = prettyAt 7 s1 ++ " * "  ++ prettyAt 7 s2
  pretty (SBi FloorDiv s1 s2) = prettyAt 7 s1 ++ " // " ++ prettyAt 7 s2
  pretty (SFun p ss) = unwords (p : map pretty' ss)
  pretty (SList ss)  = "[" ++ intercalate "; " (map pretty ss) ++ "]"
  pretty (SBi Eq s1 s2) = prettyAt 4 s1 ++ " = " ++ prettyAt 4 s2
  pretty (SBi Gt s1 s2) = prettyAt 4 s1 ++ " > " ++ prettyAt 4 s2
  pretty (SBi Ge s1 s2) = prettyAt 4 s1 ++ " >= " ++ prettyAt 4 s2
  pretty (SBi And s1 s2)  = prettyAt 3 s1 ++ " && " ++ prettyAt 3 s2
  pretty (SBi Or s1 s2)   = prettyAt 2 s1 ++ " || " ++ prettyAt 2 s2

prettyAt :: Int -> Predicate -> String
prettyAt n s | priority s > n = pretty s
prettyAt _ s                  = "(" ++ pretty s ++ ")"

priority :: Predicate -> Int
priority (SBi Or _ _)       = 2
priority (SBi And _ _)      = 3
priority (SBi Eq _ _)       = 4
priority (SBi Gt _ _)       = 4
priority (SBi Ge _ _)       = 4
priority (SBi Add _ _)      = 6
priority (SBi Mul _ _)      = 7
priority (SBi FloorDiv _ _) = 7
priority _                  = 100

instance Complex Predicate where
  isAtom SBi{}  = False
  isAtom SFun{} = False
  isAtom _      = True

instance Pretty TyScheme where
  pretty ([], ty) = pretty ty
  pretty (xs, ty) = "forall " ++ unwords (map pretty xs) ++ ". " ++ pretty ty

instance Pretty (Def TyScheme Type) where
  pretty (VarDef x t) = pretty x ++ " : " ++ pretty t
  pretty (TyDef xx _) = pretty xx ++ " : type"
  pretty (Module x _) = x ++ " : module"

instance Pretty TyEnv where
  pretty tyenv = unlines (map pretty (reverse tyenv))
