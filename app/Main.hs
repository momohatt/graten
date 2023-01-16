module Main where

import Options.Applicative

import GraTen

main :: IO ()
main = do
  opts <- execParser cmdParser
  input <- readFile (fileName opts)
  (result, log) <- evalInferM opts 1 (checkProg input)
  writeFile "log.out" log
  case result of
    Left err -> putStrLn err
    Right (tyenv', prog) -> do
      writeFile "cast.ml" prog
      putStr (pretty tyenv')
