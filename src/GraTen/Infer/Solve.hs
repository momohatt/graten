{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

module GraTen.Infer.Solve
  ( solve
  , simplifyPredInType
  , normalizeImpl
  , solveName
  , solveUniqName
  ) where

import           Data.List               (delete, nub, sortOn)
import qualified Data.Tuple.Extra        as T

import           GraTen.Infer.Constraint
import           GraTen.Infer.InferM
import           GraTen.Infer.Subst
import           GraTen.Pretty
import           GraTen.Syntax.Type


-- Solves the constraints manually with heuristics.
solve :: [VC] -> InferM (Subst, [VC])
solve c = loop c emptyS
 where
  loop [] sigma0 = return (sigma0, []) -- ログ短縮
  loop c0 sigma0 = do
    let c1 = sortPremise (normalizeImpl c0)
    tellLn ("const1:\n" ++ pretty c1)
    (sigma2, c2) <- solveVC c1
    tellLn ("subst2:\n" ++ pretty sigma2)
    tellLn ("const2:\n" ++ pretty c2)
    if null sigma2 then return (sigma0, c0)
                   else loop (map (subst sigma2) c2) (sigma2 `compose` sigma0)

normalizeImpl :: [VC] -> [VC]
normalizeImpl = removeValid . flatten . substNameVC . removeValid . flatten
  where
    removeValid :: [VC] -> [VC]
    removeValid [] = []
    removeValid (Impl ids ctx p q : xs) =
      case filter (`notElem` (ctx ++ p)) q of
        [] -> removeValid xs
        q' -> Impl ids ctx p q' : removeValid xs

    substNameVC :: [VC] -> [VC]
    substNameVC [] = []
    substNameVC (Impl ids ctx p q : xs) =
      let (sigma1, ctx') = solveName ctx
          (sigma2, p') = solveName (map (subst sigma1) p)
       in solveNth (Impl ids ctx' p' (map (subst (sigma2 `compose` sigma1)) q)) : substNameVC xs

    flatten :: [VC] -> [VC]
    flatten [] = []
    flatten (Impl ids ctx p q : xs) =
      Impl ids (flattenP ctx) (flattenP p) (flattenP q) : flatten xs


flattenP :: [Predicate] -> [Predicate]
flattenP []                   = []
flattenP (SBool True : xs)    = flattenP xs
flattenP (SBi And p1 p2 : xs) = flattenP (p1 : p2 : xs)
flattenP (x : xs)             = x : flattenP xs


sortPremise :: [VC] -> [VC]
sortPremise = map (\(Impl ids ctx p q) -> Impl ids (sortOn f ctx) (sortOn f p) q)
  where
    f (SVar _ ys) = length ys
    f _           = maxBound


solveVC :: [VC] -> InferM (Subst, [VC])
solveVC c' = do
  let (sigma0, c) = T.second normalizeImpl (removeVar [] c')
  -- tellLn ("sigma0: " ++ pretty sigma0)
  -- tellLn ("const0: " ++ pretty c)
  (sigma1, c) <- return $ solveSup [] c
  -- tellLn ("sigma1: " ++ pretty sigma1)
  -- tellLn ("const1: " ++ pretty c)
  (sigma2, c) <- solveSub [] (normalizeImpl (map (subst sigma1) c))
  return (sigma2 `compose` sigma1 `compose` sigma0, normalizeImpl c)


-- Finds solution from the conclusion side of Impl (q in ctx => p => q).
solveSup :: [VC] -> [VC] -> (Subst, [VC])
solveSup acc [] = (emptyS, reverse acc)
solveSup acc (x@(Impl _ _ p [SVar y ys]) : xs)
  | Self `elem` ys && notOccurLeft y (acc ++ xs) =
    case filter (all (`elem` ys) . names') p of
      [] -> solveSup (x : acc) xs
      p' -> T.first (`compose` [(T y ys, mkAnd p')]) $ solveSup acc xs
 where
  notOccurLeft x = all (\case Impl _ [] [] _ -> True
                              Impl _ _ _ q -> x `notElem` concatMap fsvP q)
solveSup acc (x : xs) = solveSup (x : acc) xs


-- Finds solution from the assumption side of Impl (ctx and p in ctx => p => q).
solveSub :: [VC] -> [VC] -> InferM (Subst, [VC])
solveSub acc [] = return (emptyS, reverse acc)
solveSub acc (Impl ids ctx p q : xs) = do
  let (matches, q') = T.first (groupBySVar []) (matchSVar (p ++ ctx) q)
  sigma <- mapM (\((x, ys), p) -> do
    b <- newID "b"
    return (T x ys, mkAnd (p ++ [SVar b ys]))) matches
  let (Impl ids' ctx' p' _) = subst sigma (Impl ids ctx p [])
  let (acc', xs') = T.both (map (subst sigma)) (acc, xs)
  T.first (`compose` sigma) <$> solveSub (Impl ids' ctx' p' q' : acc') (normalizeImpl xs')
 where
  matchSVar _ []            = ([], [])
  matchSVar p (q@SVar{}:qs) = T.second (q :) (matchSVar p qs)
  matchSVar p (q:qs)        = case findSVar p q of
                                Nothing -> T.second (q :) (matchSVar p qs)
                                Just x -> T.first ((x, q) :) (matchSVar p qs)

  findSVar [] _ = Nothing
  findSVar (SVar x ys : _) q | all (`elem` ys) (names' q) = Just (x, ys)
  findSVar (_ : ps) q = findSVar ps q

  groupBySVar acc [] = acc
  groupBySVar acc ((x, q) : xs) =
    case lookup x acc of
      Nothing -> groupBySVar ((x, [q]) : acc) xs
      Just p' -> groupBySVar ((x, q : p') : delete (x, p') acc) xs


-- Resolves substitution for function applications.
removeVar :: [VC] -> [VC] -> (Subst, [VC])
removeVar acc [] = (emptyS, reverse acc)
removeVar acc (Impl ids ctx p q : xs) =
  let vars = Self : ids ++ concatMap names' (ctx ++ p)
      (sigma, q') = solveUniqName vars q
      ids' = map (substID sigma) ids
      (acc', xs') = T.both (map (subst sigma)) (acc, xs)
   in T.first (`compose` sigma) (removeVar (Impl ids' ctx p q' : acc') xs')


solveUniqName :: [ID] -> [Predicate] -> (Subst, [Predicate])
solveUniqName vars = f []
  where
    f acc [] = (emptyS, reverse acc)
    f acc (SEq s1 s2 : xs) =
      case (s1, s2) of
        _ | Self `elem` (namesP s1 ++ namesP s2) -> f (SEq s1 s2 : acc) xs
        (SName x f, s) | x `notElem` (vars ++ namesP s) -> substAndContinue [(N x f, s)]
        (s, SName x f) | x `notElem` (vars ++ namesP s) -> substAndContinue [(N x f, s)]
        _ -> f (SEq s1 s2 : acc) xs
     where
      substAndContinue ss =
        let (acc', xs') = T.both (map (subst ss)) (acc, xs)
         in T.first (`compose` ss) $ f acc' xs'
    f acc (x : xs) = f (x : acc) xs


names' :: Predicate -> [ID]
names' = nub . foldPred f []
  where
    f acc (SVar _ ys)  = acc ++ ys
    f acc (SName y _)  = y : acc
    f acc _            = acc


solveName :: [Predicate] -> (Subst, [Predicate])
solveName p =
  let (sigma1, p1) = _solveName [] (flattenP p)
      (sigma2, p2) = _solveLen [] p1
   in if p == p2 then (sigma2 `compose` sigma1, p2) else solveName p2
  where
    _solveName acc [] = (emptyS, reverse acc)
    _solveName acc (SEq s1 s2 : xs) =
      case (s1, s2) of
        (SName x f, s) | x `notElem` namesP s -> substAndContinue [(N x f, s)]
        (s, SName x f) | x `notElem` namesP s -> substAndContinue [(N x f, s)]
        (SName x f, SList ss) ->
          let n = fromIntegral (length ss)
           in substAndContinue [(N x f, SList (map (\i -> SFun "nth" [SInt i, SName x f]) [0..n-1]))]
        (SFun "nth" [SInt n, SName x f], s) | x `notElem` namesP s ->
          let (acc', xs') = T.both (map (substNth n (x, f) s)) (acc, xs)
           in _solveName (SEq s1 s2 : acc') xs'
        (s, SFun "nth" [SInt n, SName x f]) | x `notElem` namesP s ->
          let (acc', xs') = T.both (map (substNth n (x, f) s)) (acc, xs)
           in _solveName (SEq s1 s2 : acc') xs'
        (_, _) -> _solveName (SEq s1 s2 : acc) xs
     where
      substAndContinue ss =
        let (acc', xs') = T.both (map (subst ss)) (acc, xs)
         in T.first (`compose` ss) $ _solveName (SEq s1 s2 : acc') xs'
    _solveName acc (x:xs) = _solveName (x : acc) xs

    _solveLen acc [] = (emptyS, reverse acc)
    _solveLen acc (SEq s1@(SFun "len" [SName x f]) s2@(SInt n) : xs) =
      let sigma = [(N x f, SList (map (\i -> SFun "nth" [SInt i, SName x f]) [0..n-1]))]
          (acc', xs') = T.both (map (subst sigma)) (acc, xs)
       in T.first (`compose` sigma) $ _solveLen (SEq s1 s2 : acc') xs'
    _solveLen acc (x:xs) = _solveLen (x : acc) xs


solveNth :: VC -> VC
solveNth (Impl ids ctx p q) =
  let sigma1 = f ctx
      p' = foldl (\acc f -> map f acc) p sigma1
      sigma2 = f p'
   in Impl ids ctx p' (foldl (\acc f -> map f acc) q (sigma1 ++ sigma2))
 where
  f [] = []
  f (SEq (SFun "nth" [SInt n, SName x fs]) s : xs) | x `notElem` namesP s = substNth n (x, fs) s : f xs
  f (_ : xs) = f xs


substNth :: Integer -> IDF -> Predicate -> Predicate -> Predicate
substNth n x p = f
  where
    f (SNeg s)       = SNeg (f s)
    f (SNot s)       = SNot (f s)
    f (SBi op s1 s2) = SBi op (f s1) (f s2)
    f (SFun "nth" [SInt m, SName y fs]) | n == m && x == (y, fs) = p
    f (SFun p ss)    = SFun p (map f ss)
    f (SList ss)     = SList (map f ss)
    f s              = s


simplifyPredInType :: [ID] -> Type -> Type
simplifyPredInType vars = snd . f vars
 where
  f vars (TyRef t p) =
    let (sigma1, t') = g vars t
        (sigma2, p') = solveName [p]
        (sigma3, p'') = solveUniqName (Self:vars) p'
     in (sigma3 `compose` sigma2 `compose` sigma1, TyRef t' (mkAnd p''))
  f vars (TyFun l Nothing t1 t2) =
    let (sigma1, t1') = f vars t1
        (sigma2, t2') = f vars t2
     in (sigma2 `compose` sigma1, TyFun l Nothing t1' t2')
  f vars (TyFun l (Just x) t1 t2) =
    let (sigma1, t1') = f vars t1
        sigma1' = map (\case (N Self f, p) -> (N x f, rename Self x p)
                             (y, p) -> (y, rename Self x p)) sigma1
        (sigma2, t2') = f (x:vars) (subst sigma1' t2)
     in if x `elem` names t2'
           then (sigma2 `compose` sigma1', TyFun l (Just x) t1' t2')
           else (sigma2 `compose` sigma1', TyFun l Nothing t1' t2')
  f vars (TyOptionFun x t1 s t2) =
    let (sigma1, t1') = f vars t1
        sigma1' = map (\case (N Self f, p) -> (N x f, rename Self x p)
                             (y, p) -> (y, rename Self x p)) sigma1
        (sigma2, t2') = f (x:vars) (subst sigma1' t2)
     in (sigma2 `compose` sigma1', TyOptionFun x t1' s t2')
  f vars (TyTuple ts) =
    let (sigmas, ts') = unzip (map (f vars) ts)
        sigmas' = map (filter (\case { (N Self _, _) -> False; _ -> True })) sigmas
     in (concat sigmas', TyTuple ts')
  f _ t = (emptyS, t)
  g vars (TyList t) = T.second TyList (f vars t)
  g _    t          = (emptyS, t)
