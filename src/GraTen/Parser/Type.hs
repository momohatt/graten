{-# LANGUAGE TupleSections #-}

module GraTen.Parser.Type
  ( typeSignature
  , typeDef
  , tySchemeAnnot
  , typeAnnot
  ) where

import           Prelude                        hiding (id)
import           Control.Monad.Combinators.Expr
import           Text.Megaparsec
import           Text.Megaparsec.Char           (char)

import           GraTen.Parser.Token
import           GraTen.Syntax.Type


typeSignature :: Parser TyEnv
typeSignature =
  -- Reverse so that the newest definition becomes the head.
  reverse <$> many (vals <|> modules <|> uncurry TyDef <$> typeDef)

vals :: Parser (Def TyScheme Type)
vals = do
  name <- reserved "val" >> (lowerId <|> parens ((infixSymbol <|> prefixSymbol) <* sc))
  ty <- symbol ":" >> tySchemeAnnot
  return (VarDef (name, 0) ty)

modules :: Parser (Def TyScheme Type)
modules = do
  name <- reserved "module" >> upperId
  env <- symbol ":" >> reserved "sig" >> typeSignature <* reserved "end"
  return (Module name env)

typeDef :: Parser (TyName, TypeDef Type)
typeDef = do
  t <- reserved "type" >> lowerId
  xs <- option [] (parens (sepBy1 lowerId comma))
  ts <- optional $ symbol "=" >>
              Left <$> rcd
          <|> Right <$> (optional (symbol "|") >> sepBy1 tag (symbol "|"))
  case ts of
    Nothing        -> return (TyName [] t, Record xs [])
    Just (Left ts) -> return (TyName [] t, Record xs ts)
    Just (Right f) -> return (TyName [] t, Variant f)
 where
  rcd = braces $ sepBy1 ((,) <$> lowerId <*> (symbol ":" >> typeAnnot)) (symbol ";")
  tag = (,) <$> upperId <*> optional (reserved "of" >> typeAnnot)

tySchemeAnnot :: Parser TyScheme
tySchemeAnnot = do
  fvs <- optional (reserved "forall" >> some id <* symbol ".")
  ty <- typeAnnot
  case fvs of
    Nothing -> return ([], ty)
    Just xs -> return (xs, ty)

typeAnnot :: Parser Type
typeAnnot =
      (TyOptionFun <$> ((, 0) <$> (symbol "?" >> lowerId)) <*> (symbol ":" >> tupleTypeAnnot)
                   <*> optional (symbol "=" >> tupleTypeAnnot)
                   <*> (symbol "->" >> typeAnnot))
  <|> (do l <- symbol "~" >> lowerId <* symbol ":"
          TyFun (Just l) (Just (l, 0)) <$> tupleTypeAnnot <*> (symbol "->" >> typeAnnot))
  <|> (TyFun Nothing <$> optional ((, 0) <$> try (lowerId <* symbol ":")) <*> try (tupleTypeAnnot <* symbol "->")
                     <*> typeAnnot)
  <|> tupleTypeAnnot

tupleTypeAnnot :: Parser Type
tupleTypeAnnot = do
  tys <- sepBy1 atomTypeAnnot (symbol "*")
  case tys of
    [ty] -> return ty
    _    -> return $ TyTuple tys

atomTypeAnnot :: Parser Type
atomTypeAnnot =
      tensorTypeAnnot
  <|> TyUnit <$ reserved "unit"
  <|> TyOption <$> (reserved "option" >> parens typeAnnot)
  <|> TyRef <$> baseTypeAnnot <*> pure (SBool True)
  <|> braceTypeAnnot
  <|> parens typeAnnot

baseTypeAnnot :: Parser BaseType
baseTypeAnnot =
      TyInt  <$ reserved "int"
  <|> TyBool <$ reserved "bool"
  <|> TyTensor <$ reserved "tensor"
  <|> TyList <$> (reserved "list" >> parens typeAnnot)
  <|> TyList <$> (reserved "array" >> parens typeAnnot)
  <|> TyUser <$> (TyName <$> many (upperId' <* char '.') <*> lowerId)

tensorTypeAnnot :: Parser Type
tensorTypeAnnot = do
  _ <- reserved "tensor"
  s <- optional (parens predAnnot)
  case s of
    Just s' -> return $ TyRef TyTensor (SEq (SShape Self) s')
    Nothing -> return $ TyRef TyTensor (SBool True)

braceTypeAnnot :: Parser Type
braceTypeAnnot = braces $ do
  t <- symbol "v:" >> baseTypeAnnot
  s <- optional (parens predAnnot)
  p <- option (SBool True) (symbol "|" >> predAnnot)
  case (t, s) of
    (TyTensor, Just s) -> return $ TyRef t (SBi And (SEq (SShape Self) s) p)
    _ -> return $ TyRef t p

predAnnot :: Parser Predicate
predAnnot = opPredAnnot

opPredAnnot :: Parser Predicate
opPredAnnot = makeExprParser (appPredAnnot <|> atomPredAnnot) table
  where
    table :: [[Operator Parser Predicate]]
    table =
      [ [ Prefix (sneg <$ symbol "-")
        , Prefix (SNot <$ symbol "!") ]
      , [ InfixL (SBi Mul      <$ symbol "*" )
        , InfixL (SBi FloorDiv <$ symbol "//" )
        ]
      , [ InfixL (SBi Add <$ symbol "+" )
        , InfixL ((\x y -> SBi Add x (sneg y)) <$ symbol "-" )
        ]
      , [ InfixR ((\x y -> SFun "cons" [x, y]) <$ symbol "::" )
        ]
      , [ InfixR ((\x y -> SFun "append" [x, y]) <$ symbol "@" )
        ]
      , [ InfixN (SBi Eq  <$ symbol "=")
        , InfixN (SBi Ge  <$ symbol ">=")
        , InfixN (flip (SBi Ge) <$ symbol "<=")
        , InfixN (SBi Gt  <$ symbol ">")
        , InfixN (flip (SBi Gt) <$ symbol "<")
        ]
      , [ InfixL (SBi And <$ symbol "&&" )
        ]
      , [ InfixL (SBi Or  <$ symbol "||" )
        ]
      ]

    sneg (SInt n) = SInt (-n)
    sneg s        = SNeg s

appPredAnnot :: Parser Predicate
appPredAnnot = do
  x <- lowerId' <|> upperId'
  s <- optional (char '.' >> lowerId') <* sc
  case s of
    Just n -> return (SName (x, 0) [n])
    Nothing -> do
      ss <- many atomPredAnnot
      case ss of
        [] -> return (SName (x, 0) [])
        _  -> return (SFun x ss)

id :: Parser ID
id = (,0) <$> ident

atomPredAnnot :: Parser Predicate
atomPredAnnot =
      SInt   <$> positiveIntegerLiteral
  <|> SBool  <$> boolLiteral
  <|> SName  <$> try (id <* char '.') <*> ((:[]) <$> lowerId)
  <|> SVar   <$> (char '\'' >> id) <*> (char '#' >> braces (sepBy id comma))
  <|> SName  <$> id <*> pure []
  <|> parens predAnnot
  <|> SList <$> brackets (sepBy predAnnot (symbol ";"))
