{-# LANGUAGE TupleSections #-}

module GraTen.Infer.Z3
  ( checkSat
  ) where

import           Data.List                  (intercalate, intersect, nub, union)
import qualified Data.Tuple.Extra           as T
import           GHC.IO.Handle              (hGetContents)
import           System.Process
import           Text.Megaparsec

import           GraTen.Infer.Constraint
import           GraTen.Infer.InferM
import           GraTen.Infer.Solve
import           GraTen.Infer.Subst
import           GraTen.Parser.Token
import           GraTen.Pretty
import           GraTen.Syntax.Type         hiding (Def (..))


-- | Checks the satisfiability of subtyping and consistent subtyping.
checkSat :: [VC] -> InferM Bool
checkSat [] = return True
checkSat cs = do
  cs <- simplify cs
  let names = nub (concatMap (concatMap sNameWithFields) cs)
  tellLn ("Constraints (subtyping):\n" ++ pretty cs)
  case inferVarType names cs of
    Nothing -> throwError "Failed to infer types of shape variables"
    Just env -> do
      -- 1. Check subtyping by interpreting |Impl ctx p q| as
      --      forall names. ctx /\ p => q
      lib <- liftIO $ readFile "lib.smtlib"
      let script' = mkCastScript env cs names
      tellLn ("script:\n" ++ script')
      let script = lib ++ '\n' : script'
      output <- liftIO (solveWithZ3 script)
      case parseResult output of
        Left err  -> throwError err
        Right Sat -> return True
        Right _   -> do
          -- 2. Check consistent subtyping by interpreting |Impl ctx p q| as
          --      exists names. ctx /\ p /\ q
          let ps = concatMap (\(Impl _ ctx p q) -> snd (solveName (ctx ++ p ++ q))) cs
          tellLn ("Constraints (consistent subtyping):\n" ++ pretty ps)
          let names = nub (concatMap sNameWithFields ps)
          let script = lib ++ '\n' : mkCheckScript env ps names
          output <- liftIO (solveWithZ3 script)
          case parseResult output of
            Left err    -> throwError err
            Right Unsat -> throwError "Unsat"
            Right _     -> return False

sNameWithFields :: Predicate -> [IDF]
sNameWithFields = foldPred go []
  where
    go acc (SName y fs) = (y, fs) : acc
    go acc _            = acc

simplify :: [VC] -> InferM [VC]
simplify vc = map (removeNth <$>) . normalizeImpl . removeEq <$> removeLen [] vc
 where
  removeEq [] = []
  removeEq (Impl ids ctx p q : xs) = do
    let (_, ctx') = solveUniqName [] ctx
        (_, p') = solveUniqName [] p
        vars = extend (ctx' ++ p') (concatMap namesP q)
     in Impl ids (removePremise vars ctx') (removePremise vars p') q : removeEq xs

  extend ps vars =
    let vars' = extend' ps vars
     in if vars' == vars then vars else extend ps vars'
  extend' []     vars = vars
  extend' (p:ps) vars =
    if null (namesP p `intersect` vars)
       then extend ps vars else extend ps (namesP p `union` vars)

  removePremise _    [] = []
  removePremise vars (p : ps) =
    if null (namesP p `intersect` vars)
       then removePremise vars ps else p : removePremise vars ps

  removeLen acc [] = return (reverse acc)
  removeLen acc (x@(Impl _ ctx p _) : xs) = do
    sigma0 <- removeLen' ctx
    sigma1 <- removeLen' p
    let sigma = sigma1 `compose` sigma0
    removeLen (map (subst sigma) (x : acc)) (map (subst sigma) xs)

  removeLen' [] = return emptyS
  removeLen' (SEq (SFun "len" [SName x f]) (SInt n) : ps) = do
    xs <- mapM (\_ -> SName <$> newID "s" <*> pure []) [1..n]
    let sigma = [(N x f, SList xs)]
    (`compose` sigma) <$> removeLen' ps
  removeLen' (_ : ps) = removeLen' ps

  removeNth (SFun "nth" [SInt n, s]) = nth n (removeNth s)
  removeNth (SFun f ps)    = SFun f (map removeNth ps)
  removeNth (SList ps)     = SList (map removeNth ps)
  removeNth (SBi op p1 p2) = SBi op (removeNth p1) (removeNth p2)
  removeNth (SNot p)       = SNot (removeNth p)
  removeNth (SNeg p)       = SNeg (removeNth p)
  removeNth p              = p

  nth n s = if n <= 0 then SFun "head" [s] else nth (n - 1) (SFun "tail" [s])


solveWithZ3 :: String -> IO String
solveWithZ3 script = do
  (_, Just hout, _, _) <- createProcess (proc "echo" [script]) { std_out = CreatePipe }
  (_, Just hout, _, _) <- createProcess (proc "z3" ["-in", "-T:1"])
    { std_in = UseHandle hout, std_out = CreatePipe }
  hGetContents hout

data Result
  = Sat
  | Unsat
  | Unknown

parseResult :: String -> Either String Result
parseResult str = do
  case parse (sc >> result <* eof) "" str of
    Left err -> Left ("Parse error at: " ++ errorBundlePretty err)
    Right r -> Right r

result :: Parser Result
result = Sat     <$ reserved "sat"
     <|> Unsat   <$ reserved "unsat"
     <|> Unknown <$ reserved "unknown"
     <|> Unknown <$ reserved "timeout"
     <|> (parens (reserved "error" <* stringLiteral) >> result)

mkCastScript :: PTyEnv -> [VC] -> [IDF] -> String
mkCastScript env cs vars =
  let body = uquantify vars (conj (map vcToScript cs))
   in "(assert " ++ body ++ ")\n(check-sat)"
 where
  uquantify [] body = body
  uquantify xs body = "(forall (" ++ unwords (map (declareTy env) xs) ++ ") " ++ body ++ ")"

mkCheckScript :: PTyEnv -> [Predicate] -> [IDF] -> String
mkCheckScript env ps vars =
  let body = equantify vars (conj (map predicateToScript ps))
   in "(assert " ++ body ++ ")\n(check-sat)"
 where
  equantify [] body = body
  equantify xs body = "(exists (" ++ unwords (map (declareTy env) xs) ++ ") " ++ body ++ ")"

declareTy :: PTyEnv -> IDF -> String
declareTy env x =
  case lookup x env of
    Just Shape -> "(" ++ idToVar x ++ " (List Int))"
    Just Bool  -> "(" ++ idToVar x ++ " Bool)"
    _          -> "(" ++ idToVar x ++ " Int)"

conj :: [String] -> String
conj [] = "true"
conj xs = "(and " ++ intercalate " " xs ++ ")"

idToVar :: IDF -> String
idToVar ((x, n), fs) = x ++ concatMap ('_':) fs ++ '!' : show n

vcToScript :: VC -> String
vcToScript (Impl _ [] [] q) = conj (map predicateToScript q)
vcToScript (Impl _ ctx p q) =
  let p' = conj (map predicateToScript (ctx ++ p))
      q' = conj (map predicateToScript q)
   in "(=> " ++ p' ++ " " ++ q' ++ ")"

predicateToScript :: Predicate -> String
predicateToScript (SInt n)    = show n
predicateToScript (SBool b)   = if b then "true" else "false"
predicateToScript (SVar _ _)  = "true"
predicateToScript (SName x f) = idToVar (x, f)
predicateToScript (SNot p)    = "(not " ++ predicateToScript p ++ ")"
predicateToScript (SNeg s)    = "(- " ++ predicateToScript s ++ ")"
predicateToScript (SBi op s1 s2) =
  let s1' = predicateToScript s1
      s2' = predicateToScript s2
      op' = case op of
              Add -> "+"
              Mul -> "*"
              FloorDiv -> "div"
              Eq -> "="
              Gt -> ">"
              Ge -> ">="
              And -> "and"
              Or -> "or"
   in "(" ++ op' ++ " " ++ s1' ++ " " ++ s2' ++ ")"
predicateToScript (SFun f xs) =
  "(" ++ f ++ " " ++ unwords (map predicateToScript xs) ++ ")"
predicateToScript (SList []) = "nil"
predicateToScript (SList (s:ss)) =
  let s' = predicateToScript s
      ss' = predicateToScript (SList ss)
   in "(insert " ++ s' ++ " " ++ ss' ++ ")"


data PredType = Bool | Int | Shape | Var Int
  deriving (Eq, Show)

type PTyEnv = [((ID, [String]), PredType)]
type PConstr = [(PredType, PredType)]

inferVarType :: [(ID, [String])] -> [VC] -> Maybe PTyEnv
inferVarType vars cs = do
  let env = zipWith (\x n -> (x, Var n)) vars [0..]
  c <- inferVC env cs
  ss <- solve c
  return (map (T.second (subst ss)) env)
 where
  solve :: PConstr -> Maybe [(Int, PredType)]
  solve [] = return []
  solve ((t1, t2) : xs) | t1 == t2 = solve xs
  solve ((Var n, t) : xs) = (`compose` (n, t)) <$> solve (map (T.both (subst [(n, t)])) xs)
  solve ((t, Var n) : xs) = (`compose` (n, t)) <$> solve (map (T.both (subst [(n, t)])) xs)
  solve _ = Nothing

  compose :: [(Int, PredType)] -> (Int, PredType) -> [(Int, PredType)]
  compose ss (n, t) = (n, subst ss t) : ss

  subst :: [(Int, PredType)] -> PredType -> PredType
  subst ss (Var m) = case lookup m ss of
                       Just t -> t
                       Nothing -> Var m
  subst _  t       = t

  -- Infer types of shape variables (SVar, SName) in best-effort manner
  inferVC :: PTyEnv -> [VC] -> Maybe PConstr
  inferVC env =
    foldM (\c (Impl _ ctx p q) -> do
      (ts, cs) <- unzip <$> mapM (infer env) (ctx ++ p ++ q)
      return (map (,Bool) ts ++ c ++ concat cs)) []

  infer :: PTyEnv -> Predicate -> Maybe (PredType, PConstr)
  infer env (SName x f) = (,[]) <$> lookup (x, f) env
  infer _   (SVar _ _)  = return (Bool, [])
  infer _   (SBool _)   = return (Bool, [])
  infer _   (SInt _)    = return (Int, [])
  infer env (SEq s1 s2) = do
    (t1, c1) <- infer env s1
    (t2, c2) <- infer env s2
    return (Bool, (t1, t2) : c1 ++ c2)
  infer env (SBi op s1 s2) | op `elem` [Add, Mul, FloorDiv] =
    (Int,) <$> checkAll env [(s1, Int), (s2, Int)]
  infer env (SBi op s1 s2) | op `elem` [Gt, Ge] =
    (Bool,) <$> checkAll env [(s1, Int), (s2, Int)]
  infer env (SBi _ s1 s2) = (Bool,) <$> checkAll env [(s1, Bool), (s2, Bool)]
  infer env (SList ss) = (Shape,) <$> checkAll env (map (,Int) ss)
  infer env (SNot s)   = (Bool,) <$> checkAll env [(s, Bool)]
  infer env (SNeg s)   = (Int,) <$> checkAll env [(s, Int)]
  infer env (SFun "append" [s1, s2]) = (Shape,) <$> checkAll env [(s1, Shape), (s2, Shape)]
  infer env (SFun "cons" [s1, s2])   = (Shape,) <$> checkAll env [(s1, Int), (s2, Shape)]
  infer env (SFun "init" [s1])       = (Shape,) <$> checkAll env [(s1, Shape)]
  infer env (SFun "last" [s1])       = (Int,) <$> checkAll env [(s1, Shape)]
  infer env (SFun "head" [s1])       = (Int,) <$> checkAll env [(s1, Shape)]
  infer env (SFun "tail" [s1])       = (Shape,) <$> checkAll env [(s1, Shape)]
  infer env (SFun "nth" [s1, s2])    = (Int,) <$> checkAll env [(s1, Int), (s2, Shape)]
  infer env (SFun "len" [s1])        = (Int,) <$> checkAll env [(s1, Shape)]
  infer env (SFun "prod" [s1])       = (Int,) <$> checkAll env [(s1, Shape)]
  infer env (SFun "dropAt" [s1, s2]) = (Shape,) <$> checkAll env [(s1, Int), (s2, Shape)]
  infer env (SFun "insertAt" [s1, s2, s3]) =
    (Shape,) <$> checkAll env [(s1, Int), (s2, Int), (s3, Shape)]
  infer env (SFun "swap" [s1, s2, s3]) = (Shape,) <$> checkAll env [(s1, Int), (s2, Int), (s3, Shape)]
  infer env (SFun "reshapeable" [s1, s2]) =
    (Bool,) <$> checkAll env [(s1, Shape), (s2, Shape)]
  infer env (SFun "reshape" [s1, s2]) =
    (Shape,) <$> checkAll env [(s1, Shape), (s2, Shape)]
  infer env (SFun "broadcastable" [s1, s2]) =
    (Bool,) <$> checkAll env [(s1, Shape), (s2, Shape)]
  infer env (SFun "broadcast" [s1, s2]) =
    (Shape,) <$> checkAll env [(s1, Shape), (s2, Shape)]
  infer env (SFun "matmul" [s1, s2]) =
    (Shape,) <$> checkAll env [(s1, Shape), (s2, Shape)]
  infer _ _ = Nothing

  checkAll :: PTyEnv -> [(Predicate, PredType)] -> Maybe PConstr
  checkAll _   [] = return []
  checkAll env ((s, t') : xs) = do
    (t, c) <- infer env s
    (((t, t') : c) ++) <$> checkAll env xs
