{-# LANGUAGE FlexibleInstances #-}

module GraTen.Syntax.Literal
  ( ConstantLiteral(..)
  , Pos
  , showPos
  ) where

data ConstantLiteral
  = LInt Integer
  | LBool Bool
  | LFloat Double
  | LChar Char
  | LString String
  deriving (Eq, Show)

-- | Position (line, column)
type Pos = (Int, Int)

showPos :: Pos -> String
showPos (l, c) = "line " ++ show l ++ ", col " ++ show c
