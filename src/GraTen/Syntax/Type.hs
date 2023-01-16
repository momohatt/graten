{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE PatternSynonyms   #-}

module GraTen.Syntax.Type
  -- Types
  ( ID
  , IDF
  , BaseType_ (..)
  , BaseType
  , Type (..)
  , Predicate (..)
  , Op (..)
  , pattern Self
  , pattern TyString
  , pattern SEq
  , pattern SShape
  , mkAnd
  , typeOfConst
  , mapPred
  , mapPredInType
  , foldPred
  , namesP
  , fsvP
  , names
  , fsv
  -- Type environments and module systems
  , TyScheme
  , Context
  , Def (..)
  , TyName (..)
  , TypeDef (..)
  , TyEnv
  , lookupVar
  , lookupType
  , lookupModule
  , findPath
  , addMapping
  , updateMapping
  ) where

import GraTen.Syntax.Literal (ConstantLiteral (..))

-- | Name and unique ID
type ID = (String, Int)

-- | ID with fields.
type IDF = (ID, [String])

data BaseType_ a
  = TyInt
  | TyBool
  | TyTensor
  | TyList a
  -- | User-defined types.
  | TyUser TyName
  deriving (Show, Functor, Foldable, Traversable)

type BaseType = BaseType_ Type

data Type
  -- | Base type with a refinement (not the 'ref' type!).
  = TyRef BaseType Predicate
  -- | The standard function types.
  | TyFun
      (Maybe String) -- ^ label for labeled arguments
      (Maybe ID)     -- ^ identifier for the argument
      Type           -- ^ argument type
      Type           -- ^ return type
  -- | Type for the optional argument functions.
  --   For a type `TyOptionFun x t1 s t2`, the argument label which is used to
  --   pass the argument is `fst x`.
  | TyOptionFun
      ID             -- ^ identifier for the argument
      Type           -- ^ argument type
      (Maybe Type)   -- ^ type of the default value when the argument is omitted
      Type           -- ^ return type
  | TyUnit
  | TyOption Type
  -- TODO: Make this dependent sum.
  | TyTuple [Type]
  deriving (Show)

-- | Refinement predicates.
data Predicate
  = SBool Bool
  | SInt Integer
  -- | Program variables and fields. 'SName x [y, z]' corresponds to x.y.z.
  | SName ID [String]
  -- | Shape unification variables, which are used only internally.
  | SVar ID [ID]
  -- | Logical negation.
  | SNot Predicate
  -- | Negation of integers.
  | SNeg Predicate
  -- | Various binary operations (defined below).
  | SBi Op Predicate Predicate
  -- | Various functions (e.g. swap, replace, ...) with arguments
  | SFun String [Predicate]
  | SList [Predicate]
  deriving (Show)

data Op
  = Add
  | Mul
  | FloorDiv
  | Eq
  | Gt
  | Ge
  | And
  | Or
  deriving (Eq, Show)

-- | The special identifier that is used in refinement predicates to refer to
--   the value of 'itself'.
pattern Self :: ID
pattern Self = ("v", 0)

pattern TyString :: Type
pattern TyString = TyRef (TyUser (TyName [] "string")) (SBool True)

instance Eq Predicate where
  SBool b1   == SBool b2   = b1 == b2
  SInt n1    == SInt n2    = n1 == n2
  SName x n  == SName y m  = x == y && n == m
  SVar x xs  == SVar y ys  = x == y && xs == ys
  SNot p     == SNot q     = p == q
  SNeg p     == SNeg q     = p == q
  SBi op1 p1 p2 == SBi op2 q1 q2 | op1 == op2 =
    if op1 `elem` [Add, Mul, Eq, And, Or]
       then p1 == q1 && p2 == q2 || p1 == q2 && p2 == q1
       else p1 == q1 && p2 == q2
  SFun f ps  == SFun g qs  = f == g && ps == qs
  SList ps   == SList qs   = ps == qs
  _          == _          = False

pattern SEq :: Predicate -> Predicate -> Predicate
pattern SEq s1 s2 = SBi Eq s1 s2

pattern SShape :: ID -> Predicate
pattern SShape x = SName x ["shape"]

mkAnd :: [Predicate] -> Predicate
mkAnd []     = SBool True
mkAnd (s:ss) = f s ss
  where
    f acc []                 = acc
    f acc (SBool True  : ss) = f acc ss
    f _   (SBool False : _)  = SBool False
    f acc (s : ss)           = f (SBi And acc s) ss

typeOfConst :: ConstantLiteral -> Type
typeOfConst (LInt n)  = TyRef TyInt (SEq (SName Self []) (SInt n))
typeOfConst (LBool b) = TyRef TyBool (SEq (SName Self []) (SBool b))
typeOfConst LFloat{}  = TyRef (TyUser (TyName [] "float")) (SBool True)
typeOfConst LChar{}   = TyRef (TyUser (TyName [] "char")) (SBool True)
typeOfConst LString{} = TyString

mapPred :: (Predicate -> Predicate) -> Predicate -> Predicate
mapPred = g
  where
    g f (SNot s)       = SNot (g f s)
    g f (SNeg s)       = SNeg (g f s)
    g f (SBi op s1 s2) = SBi op (g f s1) (g f s2)
    g f (SFun p ss)    = SFun p (map (g f) ss)
    g f (SList ss)     = SList (map (g f) ss)
    g f s              = f s

mapPredInType :: (Predicate -> Predicate) -> Type -> Type
mapPredInType = g
  where
    g f (TyRef t p)             = TyRef (g f <$> t) (f p)
    g f (TyFun l n t1 t2)       = TyFun l n (g f t1) (g f t2)
    g f (TyOptionFun n t1 s t2) = TyOptionFun n (g f t1) (g f <$> s) (g f t2)
    g f (TyTuple ts)            = TyTuple (map (g f) ts)
    g f (TyOption t)            = TyOption (g f t)
    g _ t                       = t

foldPred :: (a -> Predicate -> a) -> a -> Predicate -> a
foldPred = g
  where
    g f acc (SNot s)      = g f acc s
    g f acc (SNeg s)      = g f acc s
    g f acc (SBi _ s1 s2) = g f (g f acc s1) s2
    g f acc (SFun _ ss)   = foldl (g f) acc ss
    g f acc (SList ss)    = foldl (g f) acc ss
    g f acc s             = f acc s

namesP :: Predicate -> [ID]
namesP = foldPred go []
  where
    go acc (SName y _)  = y : acc
    go acc _            = acc

fsvP :: Predicate -> [ID]
fsvP = foldPred go []
  where
    go acc (SVar y _) = y : acc
    go acc _          = acc

names :: Type -> [ID]
names (TyRef t p)  = filter (/= Self) (namesP p ++ concatMap names t)
names (TyOption t) = names t
names (TyTuple ts) = concatMap names ts
names (TyFun _ (Just x) t1 t2)       = names t1 ++ filter (/= x) (names t2)
names (TyFun _ Nothing t1 t2)        = names t1 ++ names t2
names (TyOptionFun x t1 (Just s) t2) = names t1 ++ names s ++ filter (/= x) (names t2)
names (TyOptionFun x t1 _ t2)        = names t1 ++ filter (/= x) (names t2)
names _                              = []

fsv :: Type -> [ID]
fsv (TyRef t p)  = fsvP p ++ concatMap fsv t
fsv (TyOption t) = fsv t
fsv (TyTuple ts) = concatMap fsv ts
fsv (TyFun _ _ t1 t2)              = fsv t1 ++ fsv t2
fsv (TyOptionFun x t1 (Just s) t2) = fsv t1 ++ fsv s ++ filter (/= x) (fsv t2)
fsv (TyOptionFun x t1 _ t2)        = fsv t1 ++ filter (/= x) (fsv t2)
fsv _                              = []

--
-- Type Environment
--

type TyScheme = ([ID], Type)
type Context = [Predicate]

data Def tyscheme ty
  = VarDef ID     tyscheme
  | TyDef  TyName (TypeDef ty)
  | Module String [Def tyscheme ty]
  deriving (Show)

-- | Name of types with the absolute path to the module it is defined in.
data TyName = TyName
  { path :: [String]
  , name :: String
  }
  deriving (Eq, Show)

data TypeDef ty
  = Record { params :: [String]
           , fields :: [(String, ty)] -- ^ all IDs in Types are of the form (x, 0)
           }
  -- | Fields of variant types, which is defined as a pair of the Tag and the
  --   argument (can be omitted). For example, type 'int maybe' is expressed as
  --   [("None", Nothing), ("Some", Just (TyRef Int (SBool True)))].
  | Variant [(String, Maybe ty)]
  deriving (Show, Functor, Foldable, Traversable)

-- | Type environment. The leftmost element (head) of the list is assumed to be
--   the newest definition.
type TyEnv = [Def TyScheme Type]

lookupVar :: ID -> [Def tyscheme ty] -> Maybe tyscheme
lookupVar _ []                         = Nothing
lookupVar x (VarDef y ts : _) | x == y = return ts
lookupVar x (_ : defs)                 = lookupVar x defs

lookupType :: TyName -> [Def tyscheme ty] -> Maybe (TypeDef ty)
lookupType _  []                           = Nothing
lookupType xx (TyDef yy td : _) | xx == yy = return td
lookupType xx (_ : defs)                   = lookupType xx defs

lookupModule :: String -> [Def tyscheme ty] -> Maybe [Def tyscheme ty]
lookupModule _ []                        = Nothing
lookupModule x (Module y m : _) | x == y = return m
lookupModule x (_ : defs)                = lookupModule x defs

findPath :: [Def tyscheme ty] -> [String] -> Maybe [Def tyscheme ty]
findPath env path = f env (Just env) path
  where
    f _   r []       = r
    f env _ (p:path) = case lookupModule p env of
                         Just m -> f m (Just m) path
                         _ -> Nothing

addMapping :: [(ID, tyscheme)] -> [Def tyscheme ty] -> [Def tyscheme ty]
addMapping gamma env = map (uncurry VarDef) gamma ++ env

updateMapping :: (tyscheme -> tyscheme) -> [Def tyscheme ty] -> [Def tyscheme ty]
updateMapping _ []                   = []
updateMapping f (VarDef xx ts : env) = VarDef xx (f ts) : updateMapping f env
updateMapping f (def : env)          = def : updateMapping f env
