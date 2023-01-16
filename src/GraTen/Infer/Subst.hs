{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE ViewPatterns      #-}

module GraTen.Infer.Subst
  ( Substitutee (..)
  , Subst
  , emptyS
  , concatS
  , Substitutable (..)
  , substID
  , rename
  , compose
  , normalizePred
  ) where

import           Prelude            hiding (and, or)
import           Data.List          (find)
import           Data.Maybe         (isNothing)
import qualified Data.Tuple.Extra   as T

import           GraTen.Syntax.Type
import           GraTen.Pretty


data Substitutee
  -- A substitution |(N x fs, p)| substitutes any occurrence of |SName x fs| to |p|.
  = N ID [String]
  -- A substitution |(T x xs, p)| substitutes any occurrence of |SVar x ys| to |[ys/xs] p|.
  | T ID [ID]
  deriving (Show)

instance Eq Substitutee where
  N x f1 == N y f2 = x == y && f1 == f2
  -- Compare template variables only by the names.
  T x _  == T y _  = x == y
  _      == _      = False

-- We assume that substitutions for variables of tensor type must be
-- substitutions for shapes (e.g., for any substitution (x, V p), the variable
-- x must not be a tensor type.
type Subst = [(Substitutee, Predicate)]

instance Pretty (Substitutee, Predicate) where
  pretty (N x f,  p) = "(" ++ pretty x ++ concatMap ('.' :) f ++ ", " ++ pretty p ++ ")"
  pretty (T x xs, p) = "(" ++ pretty (SVar x xs) ++ ", " ++ pretty p ++ ")"

emptyS :: Subst
emptyS = []

concatS :: [Subst] -> Subst
concatS = concat

class Substitutable a where
  subst :: Subst -> a -> a

instance Substitutable Type where
  subst [] t = t
  subst ss (TyRef t p) = TyRef (subst ss <$> t) (subst ss p)
  subst ss (TyFun l (Just x) t1 t2) =
    case lookup (N x []) ss of
      Just (SName x' []) -> TyFun l (Just x') (subst ss t1) (subst ss t2)
      _                  -> TyFun l (Just x)  (subst ss t1) (subst ss t2)
  subst ss (TyFun l Nothing t1 t2) =
    TyFun l Nothing (subst ss t1) (subst ss t2)
  subst ss (TyOptionFun l t1 s t2) =
    case lookup (N l []) ss of
      Just (SName l' []) -> TyOptionFun l' (subst ss t1) (subst ss <$> s) (subst ss t2)
      _                  -> TyOptionFun l  (subst ss t1) (subst ss <$> s) (subst ss t2)
  subst ss (TyTuple ts) = TyTuple (map (subst ss) ts)
  subst ss (TyOption t) = TyOption (subst ss t)
  subst _  t            = t

instance Substitutable TyScheme where
  subst [] ts      = ts
  subst ss (xs, t) = (xs, subst (filter f ss) t)
    where f (N x _, _) = x `notElem` xs
          f _          = True

instance Substitutable Predicate where
  subst [] = id
  subst ss = normalizePred . mapPred f
    where
      f :: Predicate -> Predicate
      f (SVar y xs) =
        case find (\(x, _) -> x == T y []) ss of
          Just (T _ ys, p) -> subst ss (foldl (\acc f -> f acc) p (zipWith rename ys xs))
          _ -> SVar y (map (substID ss) xs)
      f s@(SName y f) = case lookup (N y f) ss of
                          Just s -> s
                          Nothing -> case lookup (N y []) ss of
                                       Just (SName x f') -> SName x (f' ++ f)
                                       _ -> s
      f s             = s

substID :: Subst -> ID -> ID
substID ss x = case lookup (N x []) ss of
                 Just (SName y []) -> y
                 _ -> x

rename :: ID -> ID -> Predicate -> Predicate
rename x y = normalizePred . mapPred f
  where
    f (SName z fs) = SName (renameIf z) fs
    f (SVar z ws)  = SVar z (map renameIf ws)
    f s            = s
    renameIf z = if z == x then y else z

compose :: Subst -> Subst -> Subst
compose s1 s2 =
  map (T.second (subst s1)) s2 ++ filter (\(x, _) -> isNothing (lookup x s2)) s1

normalizePred :: Predicate -> Predicate
normalizePred (SNot s) =
  case normalizePred s of
    SBool b       -> SBool (not b)
    SBi Or p1 p2  -> SBi And (snot p1) (snot p2)
    SBi And p1 p2 -> SBi Or (snot p1) (snot p2)
    s'            -> snot s'
normalizePred (SNeg s) =
  case normalizePred s of
    SNeg s'  -> s'
    SInt n   -> SInt (-n)
    s'       -> SNeg s'
normalizePred (SBi op s1 s2) =
  case op of
    Add      -> add      (normalizePred s1) (normalizePred s2)
    Mul      -> mul      (normalizePred s1) (normalizePred s2)
    FloorDiv -> floorDiv (normalizePred s1) (normalizePred s2)
    Eq       -> eq       (normalizePred s1) (normalizePred s2)
    Gt       -> gt       (normalizePred s1) (normalizePred s2)
    Ge       -> ge       (normalizePred s1) (normalizePred s2)
    And      -> and      (normalizePred s1) (normalizePred s2)
    Or       -> or       (normalizePred s1) (normalizePred s2)
normalizePred (SFun f ss) =
  case (f, map normalizePred ss) of
    ("cons", [s1, s2]) -> mkCons s1 s2
    ("append", [s1, s2]) -> mkAppend s1 s2
    ("nth", [s1, s2]) -> mkNth s1 s2
    ("len", [s]) -> mkLen s
    ("insertAt", [s1, s2, s3]) -> mkInsertAt s1 s2 s3
    ("dropAt", [s1, s2]) -> mkDropAt s1 s2
    -- TODO: Support cases where the first argument is not a singleton list.
    ("insertAllAt", [SList [n], x, ss]) -> mkInsertAt n x ss
    ("dropAllAt", [SList [n], ss]) -> mkDropAt n ss
    ("head", [s]) -> mkHead s
    ("tail", [s]) -> mkTail s
    ("init", [s]) -> mkInit s
    ("last", [s]) -> mkLast s
    ("prod", [s]) -> mkProd s
    ("swap", [s1, s2, s3]) -> mkSwap s1 s2 s3
    ("broadcastable", [s1, s2]) -> mkBroadcastable s1 s2
    ("broadcast", [s1, s2]) -> mkBroadcast s1 s2
    ("reshapeable", [s1, s2]) -> mkReshapeable s1 s2
    ("reshape", [s1, s2]) -> mkReshape s1 s2
    ("matmul", [s1, s2]) -> mkMatmul s1 s2
    (_, ss) -> SFun f ss
normalizePred (SList ss) = SList (map normalizePred ss)
normalizePred s = s

eq :: Predicate -> Predicate -> Predicate
eq (SInt n1) (SInt n2) = SBool (n1 == n2)
eq (SNeg s1) (SNeg s2) = eq s1 s2
eq (SBi op1 s1 s2) (SBi op2 s3 s4) | op1 == op2 && s1 == s3 = eq s2 s4
eq (SBi op1 s1 s2) (SBi op2 s3 s4) | op1 == op2 && s2 == s4 = eq s1 s3
eq (SBi Add s1 s2) (SBi Add s3 s4) | s1 == s4 = eq s2 s3
eq (SBi Add s1 s2) (SBi Add s3 s4) | s2 == s3 = eq s1 s4
eq (SBi Mul s1 s2) (SBi Mul s3 s4) | s1 == s4 = eq s2 s3
eq (SBi Mul s1 s2) (SBi Mul s3 s4) | s2 == s3 = eq s1 s4
eq (SInt n1) (SBi Mul (SInt n2) s) | n1 `mod` n2 == 0 = eq s (SInt (div n1 n2))
eq (SBi Add (SInt n2) s) (SInt n1) = eq s (SInt (n1 - n2))
eq (SBi Mul (SInt n2) s) (SInt n1) | n1 `mod` n2 == 0 = eq s (SInt (div n1 n2))
eq (SBi Mul (SInt n1) s1) (SBi Mul (SInt n2) s2) | gcd n1 n2 /= 1 =
  let m = gcd n1 n2
   in eq (SBi Mul (SInt (div n1 m)) s1) (SBi Mul (SInt (div n2 m)) s2)
eq (SList s1) (SList s2) =
  if length s1 == length s2 then mkAnd (zipWith eq s1 s2)
                            else SBool False
eq s1@SList{} s2@SFun{}  = processList s1 s2
eq s1@SFun{}  s2@SList{} = processList s2 s1
eq (SFun "append" [s1, SList s2]) (SFun "append" [s3, SList s4]) =
  case compare (length s2) (length s4) of
    EQ -> mkAnd (eq s1 s3 : zipWith eq s2 s4)
    LT -> mkAnd (eq s1 (mkAppend s3 (SList (drop (length s2) s4))) : zipWith eq s2 s4)
    GT -> mkAnd (eq s3 (mkAppend s1 (SList (drop (length s4) s2))) : zipWith eq s2 s4)
eq (SFun "append" [SList s1, s2]) (SFun "append" [SList s3, s4]) =
  case compare (length s1) (length s3) of
    EQ -> mkAnd (eq s2 s4 : zipWith eq s1 s3)
    LT -> mkAnd (eq s2 (mkAppend (SList (drop (length s1) s3)) s4) : zipWith eq s1 s3)
    GT -> mkAnd (eq s4 (mkAppend (SList (drop (length s3) s1)) s2) : zipWith eq s1 s3)
eq s1 s2 | s1 == s2 = SBool True
eq s1 s2@(SName Self _) = SEq s2 s1
eq s1 s2 = SEq s1 s2

processList :: Predicate -> Predicate -> Predicate
processList (SList ss) (SFun "append" [SList ss1, s]) =
  let (ss1', ss2') = splitAt (length ss1) ss
   in mkAnd (eq (SList ss2') s : zipWith eq ss1 ss1')
processList (SList ss) (SFun "append" [s, SList ss1]) =
  let (ss1', ss2') = splitAt (length ss - length ss1) ss
   in mkAnd (eq (SList ss1') s : zipWith eq ss2' ss1)
processList (SList (x:ss1)) (SFun "cons" [y, ss2]) =
  and (eq x y) (eq (SList ss1) ss2)
processList (SList ss1) (SFun "insertAt" [SInt n, x, ss2]) =
  case splitAt (fromIntegral n) ss1 of
    (_, []) -> SBool False
    (ss, y:ss') -> and (eq (SList (ss ++ ss')) ss2) (eq x y)
processList s1 s2 = SEq s1 s2

add :: Predicate -> Predicate -> Predicate
add (SInt n1) (SInt n2) = SInt (n1 + n2)
add s1        (SInt 0)  = s1
add (SInt 0)  s2        = s2
add s1 (SNeg s2) | s1 == s2 = SInt 0
add (SBi Add s1 (SInt n1)) (SInt n2) = add s1 (SInt (n1 + n2))
add (SBi Add (SInt n1) s1) (SInt n2) = add s1 (SInt (n1 + n2))
add (SBi FloorDiv s1 s2@(SInt n)) (SInt m) = floorDiv (add s1 (SInt (n * m))) s2
add s1 s2 = SBi Add s1 s2

mul :: Predicate -> Predicate -> Predicate
mul (SInt n1)   (SInt n2)   = SInt (n1 * n2)
mul _           (SInt 0)    = SInt 0
mul (SInt 0)    _           = SInt 0
mul s1          (SInt 1)    = s1
mul (SInt 1)    s2          = s2
mul s1          (SInt (-1)) = SNeg s1
mul (SInt (-1)) s2          = SNeg s2
mul s1@SInt{}   (SBi Add s2 s3) = SBi Add (SBi Mul s1 s2) (SBi Mul s1 s3)
mul (SInt n)    (SBi Mul (SInt m) s) = SBi Mul (SInt (n * m)) s
mul s  n@SInt{} = SBi Mul n s
mul s1 s2       = SBi Mul s1 s2

floorDiv :: Predicate -> Predicate -> Predicate
floorDiv (SInt n) (SInt m)                   = SInt (div n m)
floorDiv (SBi FloorDiv s1 (SInt n)) (SInt m) = SBi FloorDiv s1 (SInt (n * m))
floorDiv (SBi Mul s1 s2) s3 | s1 == s3 = s2
floorDiv (SBi Mul s1 s2) s3 | s2 == s3 = s1
floorDiv s1 (SInt 1) = s1
floorDiv s1 s2       = SBi FloorDiv s1 s2

and :: Predicate -> Predicate -> Predicate
and (SBool b1) (SBool b2) = SBool (b1 && b2)
and (SBool False) _ = SBool False
and _ (SBool False) = SBool False
and (SBool True) s = s
and s (SBool True) = s
and s1 s2 | s1 == s2 = s1
and s1 s2            = SBi And s1 s2

or :: Predicate -> Predicate -> Predicate
or (SBool b1) (SBool b2) = SBool (b1 || b2)
or (SBool True) _ = SBool True
or _ (SBool True) = SBool True
or (SBool False) s = s
or s (SBool False) = s
or s1 s2 = SBi Or s1 s2

gt :: Predicate -> Predicate -> Predicate
gt (SInt n1) (SInt n2) = SBool (n1 > n2)
gt s1        s2        = SBi Gt s1 s2

ge :: Predicate -> Predicate -> Predicate
ge (SInt n1) (SInt n2) = SBool (n1 >= n2)
ge s1        s2        = SBi Ge s1 s2

snot :: Predicate -> Predicate
snot (SNot s) = s
snot s        = SNot s

mkCons :: Predicate -> Predicate -> Predicate
mkCons s (SList ss) = SList (s : ss)
mkCons s ss         = SFun "cons" [s, ss]

mkAppend :: Predicate -> Predicate -> Predicate
mkAppend (SList [])  s           = s
mkAppend s           (SList [])  = s
mkAppend (SList ss1) (SList ss2) = SList (ss1 ++ ss2)
mkAppend s1          s2          = SFun "append" [s1, s2]

mkNth :: Predicate -> Predicate -> Predicate
mkNth (SInt n) (SList ss) =
  let m = if n < 0 then length ss - fromIntegral n `mod` length ss
                   else fromIntegral n `mod` length ss
   in ss !! m
mkNth (SInt 0) (SFun "append" [SList (s:_), _]) = s
mkNth (SInt n) (SFun "insertAt" [SInt m, x, _]) | n == m = x
mkNth s1       s2         = SFun "nth" [s1, s2]

mkLen :: Predicate -> Predicate
mkLen (SList ss)                  = SInt (toInteger (length ss))
mkLen (SFun "init" [s])           = add (mkLen s) (SInt (-1))
mkLen (SFun "insertAt" [_, _, s]) = add (mkLen s) (SInt 1)
mkLen (SFun "dropAt" [_, s])      = add (mkLen s) (SInt (-1))
mkLen (SFun "append" [s1, s2])    = add (mkLen s1) (mkLen s2)
mkLen s                           = SFun "len" [s]

mkInsertAt :: Predicate -> Predicate -> Predicate -> Predicate
mkInsertAt (SInt n) x (SList ss) =
  let m = if n < 0 then toInteger (length ss) + 1 + n else n
   in SList (insertAt m x ss)
mkInsertAt s1 s2 s3 = SFun "insertAt" [s1, s2, s3]

mkDropAt :: Predicate -> Predicate -> Predicate
mkDropAt (SInt n) (SList ss) =
  let m = if n < 0 then toInteger (length ss) + n else n
   in SList (dropAt m ss)
mkDropAt (SInt n) (SFun "insertAt" [SInt m, _, s]) | n == m = s
mkDropAt s1 s2 = SFun "dropAt" [s1, s2]

mkHead :: Predicate -> Predicate
mkHead (SList (s:_)) = s
mkHead s             = SFun "head" [s]

mkTail :: Predicate -> Predicate
mkTail (SList (_:s)) = SList s
mkTail s             = SFun "tail" [s]

mkInit :: Predicate -> Predicate
mkInit (SList [])                     = SList []
mkInit (SList ss)                     = SList (init ss)
mkInit (SFun "append" [s, SList [_]]) = s
mkInit s                              = SFun "init" [s]

mkLast :: Predicate -> Predicate
mkLast (SList ss)             = last ss
mkLast (SFun "append" [_, s]) = mkLast s -- s cannot be an empty list.
mkLast s                      = SFun "last" [s]

mkProd :: Predicate -> Predicate
mkProd (SList ss)               = foldr mul (SInt 1) ss
mkProd (SFun "append" [s1, s2]) = mul (mkProd s1) (mkProd s2)
mkProd s                        = SFun "prod" [s]

mkSwap :: Predicate -> Predicate -> Predicate -> Predicate
mkSwap (SInt n) (SInt m) (SList ss) =
  let n' = if n < 0 then toInteger (length ss) + n else n
      m' = if m < 0 then toInteger (length ss) + m else m
      sn = ss !! fromIntegral n'
      sm = ss !! fromIntegral m'
   in SList (insertAt m' sn (dropAt m' (insertAt n' sm (dropAt n' ss))))
mkSwap s1 s2 s3 = SFun "swap" [s1, s2, s3]

mkBroadcastable :: Predicate -> Predicate -> Predicate
mkBroadcastable (SList ss1) (SList ss2) =
  broadcastable (reverse ss1) (reverse ss2)
mkBroadcastable (SList []) _          = SBool True
mkBroadcastable _          (SList []) = SBool True
mkBroadcastable s1 (SFun "insertAt" [i, SInt 1, SFun "dropAt" [j, s2]])
  | i == j && s1 == s2 = SBool True
mkBroadcastable s1 s2 | s1 == s2 = SBool True
mkBroadcastable s1 s2            = SFun "broadcastable" [s1, s2]

mkBroadcast :: Predicate -> Predicate -> Predicate
mkBroadcast (SList ss1) (SList ss2) =
  SList (reverse (broadcast (reverse ss1) (reverse ss2)))
mkBroadcast (SList [])  s           = s
mkBroadcast s           (SList [])  = s
mkBroadcast s1 (SFun "insertAt" [i, SInt 1, SFun "dropAt" [j, s2]])
  | i == j && s1 == s2 = s1
mkBroadcast s1 s2 | s1 == s2 = s1
mkBroadcast s1 s2            = SFun "broadcast" [s1, s2]

mkReshapeable :: Predicate -> Predicate -> Predicate
mkReshapeable s1 (SList s2) = eq (mkProd s1) (mkProd (SList (f s2)))
  where
    -- We assume that the size '-1' is explicitly specified in the program.
    s2' = filter (/= SInt (-1)) s2
    f = map (\case SInt (-1) -> floorDiv (mkProd s1) (foldr mul (SInt 1) s2')
                   x -> x)
mkReshapeable s1 s2 = SFun "reshapeable" [s1, s2]

mkReshape :: Predicate -> Predicate -> Predicate
mkReshape s1 (SList s2) = SList (f s2)
  where
    s2' = filter (/= SInt (-1)) s2
    f = map (\case SInt (-1) -> floorDiv (mkProd s1) (foldr mul (SInt 1) s2')
                   x -> x)
mkReshape s1 s2 = SFun "reshape" [s1, s2]

mkMatmul :: Predicate -> Predicate -> Predicate
mkMatmul (SList ss1) (SList ss2) =
  case matmul ss1 ss2 of
    Just ss3 -> SList ss3
    Nothing -> SFun "matmul" [SList ss1, SList ss2]
mkMatmul s1 s2 = SFun "matmul" [s1, s2]

insertAt :: Integer -> Predicate -> [Predicate] -> [Predicate]
insertAt n x xs | n <= 0  = x : xs
insertAt _ x []           = [x]
insertAt n x (y : xs)     = y : insertAt (n - 1) x xs

dropAt :: Integer -> [Predicate] -> [Predicate]
dropAt _ []     = []
dropAt n (x:xs) = if n <= 0 then xs else x : dropAt (n - 1) xs

broadcastable :: [Predicate] -> [Predicate] -> Predicate
broadcastable []      _       = SBool True
broadcastable _       []      = SBool True
broadcastable (s1:r1) (s2:r2) =
  and (or (eq s1 s2) (or (eq s1 (SInt 1)) (eq s2 (SInt 1)))) (broadcastable r1 r2)

broadcast :: [Predicate] -> [Predicate] -> [Predicate]
broadcast [] s  = s
broadcast s  [] = s
broadcast (s1:r1) (SInt 1:r2) = s1 : broadcast r1 r2
broadcast (SInt 1:r1) (s2:r2) = s2 : broadcast r1 r2
broadcast (s1:r1) (_:r2)      = s1 : broadcast r1 r2

matmul :: [Predicate] -> [Predicate] -> Maybe [Predicate]
matmul []     _      = Nothing
matmul _      []     = Nothing
matmul [a]    [c, d] = if a == c then Just [d]    else Nothing
matmul [a, b] [c]    = if b == c then Just [a]    else Nothing
matmul [a, b] [c, d] = if b == c then Just [a, d] else Nothing
matmul xs ys = matmul (drop (length xs - 2) xs) (drop (length ys - 2) ys)
