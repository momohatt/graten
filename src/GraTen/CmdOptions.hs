{-# LANGUAGE TupleSections #-}

module GraTen.CmdOptions
  ( Options (..)
  , defaultOptions
  , cmdParser
  ) where

import Options.Applicative

data Options = Options
  { fileName   :: String
  , libs       :: [String]
  , optUseZ3   :: Bool
  , optCast    :: Bool
  , optFlush   :: Bool
  }

defaultOptions :: Options
defaultOptions = Options "" [] True False False

cmdParser :: ParserInfo Options
cmdParser = info (helper <*> cmdArgParser)
          $ fullDesc
          <> header "GraTen: Gradual Type Checker for Tensor"

cmdArgParser :: Parser Options
cmdArgParser =
  Options
    <$> strArgument (metavar "FILE")
    <*> many (strArgument (metavar "LIB"))
    <*> flag True False
          (long "no-z3"
          <> help "Do not use Z3")
    <*> flag False True
          (long "cast"
          <> help "Stop before the assertion insertion and output the source program with subsumption markers inserted")
    <*> flag False True
          (long "flush"
          <> help "Output log to stderr instead of a file (log.out)")
