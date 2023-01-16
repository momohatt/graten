module GraTen.Parser
  ( parseProgram
  , parseSignatures
  ) where

import Text.Megaparsec

import GraTen.Parser.Expr
import GraTen.Parser.Token
import GraTen.Parser.Type
import GraTen.Syntax

parseProgram :: String -> Either String (Program String)
parseProgram input =
  case parse (sc >> many stmt <* eof) "" input of
    Left err -> Left ("Parse error at: " ++ errorBundlePretty err)
    Right c  -> Right c

parseSignatures :: String -> Either String TyEnv
parseSignatures input =
  case parse (sc >> typeSignature <* eof) "" input of
    Left err -> Left ("Parse error at: " ++ errorBundlePretty err)
    Right c  -> Right c
