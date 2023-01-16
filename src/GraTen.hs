{-# LANGUAGE MultiWayIf    #-}
{-# LANGUAGE TupleSections #-}

module GraTen
  ( module GraTen.CmdOptions
  , module GraTen.Pretty
  , module GraTen.Print
  , module GraTen.Syntax
  , module GraTen.Infer
  , getInitEnv
  , check
  , checkProg
  ) where

import           Data.Char          (toUpper)
import           System.Directory   (listDirectory)
import           System.Posix.Files (isDirectory, getFileStatus)

import           GraTen.Alpha
import           GraTen.Assert
import           GraTen.CmdOptions
import           GraTen.Desugar
import           GraTen.Module
import           GraTen.Parser
import           GraTen.Pretty
import           GraTen.Print
import           GraTen.Infer
import           GraTen.Syntax
import qualified GraTen.SimpleType.Infer as S

check :: TyEnv -> String -> InferM (TyEnv, String)
check tyenv prog = do
  case parseProgram prog of
    Left err -> throwError err
    Right prog' -> do
      prog <- alpha (desugar prog')
      prog <- S.typecheck tyenv prog
      (env, prog) <- typecheck tyenv prog
      b <- lift . lift $ asks optCast
      if b then return (env, prettyStr prog)
           else (env,) . prettyStr . map undesugar <$> insertAssert prog

checkProg :: String -> InferM (TyEnv, String)
checkProg input = do
  initEnv <- getInitEnv
  libfiles <- lift . lift $ asks libs
  libenvs <- mapM (liftIO . readStubFiles) libfiles
  check (addPath [] (concat libenvs ++ initEnv)) input

getInitEnv :: InferM TyEnv
getInitEnv = do
  env1 <- liftIO (readStubFiles "stub")
  (env2, _) <- typecheck env1 [IOpen "Stdlib"]
  return (env2 ++ env1)

-- | Read type signature files under |currentDirectory| recursively and create
--   type environment.
readStubFiles :: FilePath -> IO TyEnv
readStubFiles currentDirectory = do
  files <- listDirectory currentDirectory
  foldM f [] files
 where
  f accenv fileName = do
    isDir <- isDirectory <$> getFileStatus (currentDirectory ++ '/' : fileName)
    let (fileName', extension) = splitAt (length fileName - 4) fileName
    if | extension == ".mli" -> do
         input <- readFile (currentDirectory ++ '/' : fileName)
         let moduleName = capitalize fileName'
         case parseSignatures input of
           Right env -> return (Module moduleName env : accenv)
           Left err -> do
             putStrLn (currentDirectory ++ '/' : fileName)
             putStrLn err
             return (Module moduleName [] : accenv)
       | isDir -> do
         mod <- readStubFiles (currentDirectory ++ '/' : fileName)
         let moduleName = capitalize fileName
         return (Module moduleName mod : accenv)
       | otherwise -> return accenv

capitalize :: String -> String
capitalize []     = []
capitalize (x:xs) = toUpper x : xs
