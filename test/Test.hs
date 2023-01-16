{-# LANGUAGE LambdaCase #-}

module Main where

import Data.List                      (find)
import Data.Text                      (count, pack)
import System.Environment             (getArgs)

import Test.Framework                 (defaultMainWithArgs, testGroup)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit

import GraTen
import GraTen.Parser

main :: IO ()
main = do
  args <- getArgs
  let unitTests = map (\file -> TestLabel file (makeOneUnitTest file)) unitTestCases
  let exampleTests = map (\(file, libs, numAssert) -> TestLabel file (makeOneExampleTest file libs numAssert)) exampleCases
  let tests = [ testGroup "unit-test" (hUnitTestToTests (TestList unitTests))
              , testGroup "full-test" (hUnitTestToTests (TestList exampleTests))
              ]
  defaultMainWithArgs tests args

makeOneUnitTest :: String -> Test
makeOneUnitTest file = TestCase $ do
  input <- readFile file
  expectedTypes <- readFile (file ++ "i")
  (r, _) <- evalInferM defaultOptions 1 (checkProg input)
  case r of
    Left err -> assertFailure err
    Right (test, _) -> do
      case parseSignatures expectedTypes of
        Left err -> assertFailure err
        Right ans -> case compareEnv test ans of
                       []   -> assertBool "" True
                       msgs -> assertFailure (unlines msgs)

compareEnv :: TyEnv -> TyEnv -> [String]
compareEnv _     [] = []
compareEnv tyenv (VarDef (x, _) t2 : xs) =
  case find (\case { VarDef (y, _) _ -> x == y; _ -> False }) tyenv of
    Just (VarDef _ t1) -> compareTypes x t1 t2 ++ compareEnv tyenv xs
    _ -> ("Variable " ++ x ++ " not defined") : compareEnv tyenv xs
compareEnv tyenv (_ : xs) = compareEnv tyenv xs

compareTypes :: String -> TyScheme -> TyScheme -> [String]
compareTypes x (fv1, ty1) (fv2, ty2)
  | length fv1 /= length fv2 =
    ["Different number of free shape variables for " ++ x ++ ":\n  expected " ++ pretty (fv2, ty2) ++ "\n  but got " ++ pretty (fv1, ty1)]
  | aeT (zip fv1 fv2) ty1 ty2 = []
  | otherwise =
    ["Inferred types are different for " ++ x ++ ":\n  expected " ++ pretty (fv2, ty2) ++ "\n  but got " ++ pretty (fv1, ty1)]
  where
    aeT :: [(ID, ID)] -> Type -> Type -> Bool
    aeT ps (TyRef t1 p1) (TyRef t2 p2) =
      aeBT ps t1 t2 && aeP ps (flatten [p1]) (flatten [p2])
    aeT ps (TyFun _ (Just x) t1 t2) (TyFun _ (Just y) t3 t4) =
      x `notElem` map fst ps && y `notElem` map snd ps &&
        aeT ps t1 t3 && aeT ((x, y) : ps) t2 t4
    aeT ps (TyFun (Just l1) _ t1 t2) (TyFun (Just l2) _ t3 t4) | l1 == l2 =
      aeT ps t1 t3 && aeT ps t2 t4
    aeT ps (TyFun _ Nothing t1 t2) (TyFun _ Nothing t3 t4) =
      aeT ps t1 t3 && aeT ps t2 t4
    aeT ps (TyOptionFun x t1 _ t2) (TyOptionFun y t3 _ t4) =
      x `notElem` map fst ps && y `notElem` map snd ps &&
        aeT ps t1 t3 && aeT ((x, y) : ps) t2 t4
    aeT _ TyUnit TyUnit = True
    aeT ps (TyTuple ts1) (TyTuple ts2) | length ts1 == length ts2 =
      and (zipWith (aeT ps) ts1 ts2)
    aeT _ _ _ = False

    aeBT :: [(ID, ID)] -> BaseType -> BaseType -> Bool
    aeBT _  TyInt       TyInt       = True
    aeBT _  TyBool      TyBool      = True
    aeBT _  TyTensor    TyTensor    = True
    aeBT ps (TyList t1) (TyList t2) = aeT ps t1 t2
    aeBT _  (TyUser xx) (TyUser yy) = xx == yy
    aeBT _ _ _ = False

    flatten []                   = []
    flatten (SBi And p1 p2 : ps) = flatten (p1 : p2 : ps)
    flatten (p : ps)             = p : flatten ps

    aeP :: [(ID, ID)] -> [Predicate] -> [Predicate] -> Bool
    aeP _  []     _  = True
    aeP xs (p:ps) qs = case find (aeS xs p) qs of
                         Nothing -> False
                         Just _ -> aeP xs ps qs

    aeS :: [(ID, ID)] -> Predicate -> Predicate -> Bool
    aeS _  (SBool x)       (SBool y)       = x == y
    aeS _  (SInt n)        (SInt m)        = n == m
    aeS ps (SName x n)     (SName y m)     = n == m && ((x, y) `elem` ps || x == y)
    aeS ps (SVar x _)      (SVar y _)      = (x, y) `elem` ps
    aeS ps (SShape x)      (SShape y)      = (x, y) `elem` ps || x == y
    aeS ps (SNeg s1)       (SNeg s2)       = aeS ps s1 s2
    aeS ps (SBi And s1 s2) (SBi And s3 s4) = aeS ps s1 s3 && aeS ps s2 s4
    aeS ps (SBi op1 s1 s2) (SBi op2 s3 s4) = op1 == op2 && aeS ps s1 s3 && aeS ps s2 s4
    aeS ps (SFun f ss1)    (SFun g ss2)    = f == g && and (zipWith (aeS ps) ss1 ss2)
    aeS ps (SList ss1)     (SList ss2)     = and (zipWith (aeS ps) ss1 ss2)
    aeS ps (SNot p1)       (SNot p2)       = aeS ps p1 p2
    aeS _ _ _ = False

makeOneExampleTest :: String -> [String] -> Int -> Test
makeOneExampleTest file libs numAssert = TestCase $ do
  input <- readFile file
  (r, _) <- evalInferM (defaultOptions { libs = libs }) 1 (checkProg input)
  case r of
    Left err -> assertFailure err
    Right (_, progWithAssert) -> do
      assertEqual file numAssert (count (pack "assert") (pack progWithAssert))

unitTestCases :: [FilePath]
unitTestCases =
  [ "test/fixture/tensor.ml"
  , "test/fixture/layer.ml"
  , "test/fixture/annot.ml"
  , "test/fixture/optional.ml"
  , "test/fixture/if-and-rec.ml"
  , "test/fixture/poly.ml"
  , "test/fixture/paper-intro.ml"
  , "test/fixture/cast.ml"
  , "test/fixture/record.ml"
  , "test/fixture/module.ml"
  , "test/mlp/mlp.ml"
  , "test/vision/alexnet.ml"
  , "test/vision/resnet.ml"
  , "test/vision/resnet-unannot.ml"
  ]

exampleCases :: [(FilePath, [FilePath], Int)]
exampleCases =
  [ ("examples/char_rnn/char_rnn.ml",                        [],                                  1)
  , ("examples/char_rnn/char_rnn_annotated.ml",              [],                                  0)
  , ("examples/cifar/cifar_train.ml",                        ["examples/cifar"],                  0)
  , ("examples/cifar/densenet.ml",                           ["examples/cifar"],                  6)
  , ("examples/cifar/densenet_annotated.ml",                 ["examples/cifar"],                  0)
  , ("examples/cifar/fast_resnet.ml",                        ["examples/cifar"],                  0)
  , ("examples/cifar/preact_resnet.ml",                      ["examples/cifar"],                  8)
  , ("examples/cifar/preact_resnet_annotated.ml",            ["examples/cifar"],                  0)
  , ("examples/cifar/resnet.ml",                             ["examples/cifar"],                  8)
  , ("examples/cifar/resnet_annotated.ml",                   ["examples/cifar"],                  0)
  , ("examples/gan/began.ml",                                [],                                  1)
  , ("examples/gan/gan_stability.ml",                        [],                                  40)
  , ("examples/gan/gan_stability_annotated.ml",              [],                                  2)
  , ("examples/gan/mnist_cgan.ml",                           [],                                  1)
  , ("examples/gan/mnist_dcgan.ml",                          [],                                  4)
  , ("examples/gan/mnist_dcgan_annotated.ml",                [],                                  0)
  , ("examples/gan/mnist_gan.ml",                            [],                                  0)
  , ("examples/gan/progressive_growing_gan.ml",              [],                                  0)
  , ("examples/gan/relativistic_dcgan.ml",                   [],                                  1)
  , ("examples/jit/load_and_run.ml",                         [],                                  1)
  , ("examples/min-gpt/mingpt.ml",                           [],                                  8)
  , ("examples/min-gpt/mingpt_annotated.ml",                 [],                                  0)
  , ("examples/mnist/conv.ml",                               [],                                  0)
  , ("examples/mnist/linear.ml",                             [],                                  0)
  , ("examples/mnist/nn.ml",                                 [],                                  0)
  , ("examples/pretrained/finetuning.ml",                    [],                                  0)
  , ("examples/pretrained/predict.ml",                       [],                                  2)
  , ("examples/reinforcement-learning/a2c.ml",               ["examples/reinforcement-learning"], 0)
  , ("examples/reinforcement-learning/ppo.ml",               ["examples/reinforcement-learning"], 0)
  , ("examples/reinforcement-learning/rollout.ml",           ["examples/reinforcement-learning"], 9)
  , ("examples/reinforcement-learning/rollout_annotated.ml", ["examples/reinforcement-learning"], 1)
  , ("examples/translation/seq2seq.ml",                      ["examples/translation"],            11)
  , ("examples/translation/seq2seq_annotated.ml",            ["examples/translation"],            3)
  , ("examples/vae/vae.ml",                                  [],                                  4)
  , ("examples/vae/vae_annotated.ml",                        [],                                  0)
  , ("examples/yolo/yolo.ml",                                ["examples/yolo"],                   4)
  , ("examples/yolo/yolo_annotated.ml",                      ["examples/yolo"],                   3)
  ]
