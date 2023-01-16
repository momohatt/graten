{-# LANGUAGE TupleSections #-}

module GraTen.Infer.InferM
  ( module Control.Monad.State
  , module Control.Monad.Except
  , module Control.Monad.Reader
  , module Control.Monad.Writer
  , CheckM
  , InferM
  , newInt
  , newID
  , runCheckM
  , evalInferM
  , tellLn
  ) where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Debug.Trace

import GraTen.CmdOptions

type CheckM = ExceptT String (WriterT String (ReaderT Options IO))

type InferM = StateT Int CheckM

newInt :: Monad m => StateT Int m Int
newInt = do
  n <- get
  modify (+ 1)
  return n

newID :: Monad m => String -> StateT Int m (String, Int)
newID x = (x,) <$> newInt

runCheckM :: Options -> CheckM a -> IO (Either String a, String)
runCheckM opts m = runReaderT (runWriterT (runExceptT m)) opts

evalInferM :: Options -> Int -> InferM a -> IO (Either String a, String)
evalInferM opts n m = runCheckM opts (evalStateT m n)

tellLn :: String -> InferM ()
tellLn s = do
  flush <- lift . lift . lift $ asks optFlush
  lift . lift $ if flush then trace s (pure ()) else tell (s ++ "\n")
