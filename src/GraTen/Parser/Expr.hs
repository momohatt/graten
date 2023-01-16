{-# LANGUAGE TupleSections #-}

module GraTen.Parser.Expr
  ( stmt
  ) where

import           Control.Monad.Combinators.Expr
import           Text.Megaparsec
import           Text.Megaparsec.Char           (char)
import qualified Data.Set                       as Set

import           GraTen.Parser.Token
import           GraTen.Parser.Type             (typeAnnot, typeDef)
import           GraTen.Syntax.Expr


stmt :: Parser (Decl String)
stmt = open
   <|> (reserved "module" >> moduleDecl)
   <|> (reserved "let" >> decl)
   <|> recordDecl

decl :: Parser (Decl String)
decl = (reserved "rec" >> rec) <|> nonrec
  where
    rec = do
      var  <- lowerId
      annot <- optional (symbol ":" >> typeAnnot)
      pos <- getPos
      args <- many argExpr
      retAnnot <- optional (symbol ":" >> typeAnnot)
      body <- symbol "=" >> expr
      case retAnnot of
        Nothing -> return $ DLetRec var annot (foldr (EFun pos) body args) pos
        Just t  -> return $ DLetRec var annot (foldr (EFun pos) (EType pos body t) args) pos
    nonrec = do
      var <- pattern
      args <- many argExpr
      annot <- optional (symbol ":" >> typeAnnot)
      pos <- getPos
      body <- symbol "=" >> expr
      case annot of
        Nothing -> return $ DLet var (foldr (EFun pos) body args) pos
        Just t  -> return $ DLet var (foldr (EFun pos) (EType pos body t) args) pos

moduleDecl :: Parser (Decl String)
moduleDecl = do
  name <- upperId <* symbol "="
  DModule name <$> (reserved "struct" >> many stmt <* reserved "end") <|>
    DLetModule name <$> sepBy1 upperId (char '.')

moduleAlias :: Parser (Decl String)
moduleAlias =
  reserved "module" >> DLetModule <$> upperId
                                  <*> (symbol "=" >> sepBy1 upperId (char '.'))

recordDecl :: Parser (Decl String)
recordDecl = uncurry DTypeDef <$> typeDef

open :: Parser (Decl String)
open = reserved "open" >> DOpen <$> upperId

expr :: Parser (Expr String)
expr = label "expression" $ do
  exprs <- sepBy1 (opExpr True) comma
  case exprs of
    [e] -> return e
    _   -> return (ETuple exprs)

exprInList :: Parser (Expr String)
exprInList = do
  exprs <- sepBy1 (opExpr False) comma
  case exprs of
    [e] -> return e
    _   -> return (ETuple exprs)

matchClause1 :: Parser [(Pattern String, Expr String)]
matchClause1 = do
  first <- optional (symbol "|") >> matchClause
  rest  <- many (symbol "|" >> matchClause)
  return (first : rest)
  where
    matchClause :: Parser (Pattern String, (Expr String))
    matchClause = (,) <$> pattern <*> (symbol "->" >> expr)

exprInOp :: Parser (Expr String)
exprInOp =
       EIf    <$> (reserved "if" >> getPos) <*> expr <*> (reserved "then" >> exprInList)
              <*> option EUnit (reserved "else" >> exprInList)
   <|> EFor   <$> (reserved "for" >> getPos) <*> lowerId <*> (symbol "=" >> expr)
              <*> ((reserved "downto" <|> reserved "to") >> expr)
              <*> (reserved "do" >> expr <* reserved "done")
   <|> ELetIn <$> (reserved "let" >> decl <|> open <|> moduleAlias) <*> (reserved "in" >> expr)
   <|> EMatch <$> (reserved "match" >> getPos) <*> expr  <*> (reserved "with" >> matchClause1)
   <|> funExpr
   <|> atomOrAppExpr
   <?> "expression without ops"

funExpr :: Parser (Expr String)
funExpr = do
  p <- getPos
  args <- reserved "fun" >> some argExpr
  body <- symbol "->" >> expr
  return $ foldr (EFun p) body args

argExpr :: Parser (Arg String)
argExpr = labeledArg <|> optionalArg <|> normalArg
  where
    normalArg = Required Nothing <$> atomPattern

    labeledArg =
      symbol "~" >> (
            parens (do
              x <- lowerId
              t <- optional (symbol ":" >> typeAnnot)
              case t of
                Nothing -> return $ Required (Just x) (PVar x)
                Just t' -> return $ Required (Just x) (PType x t'))
        <|>        (do
              x <- lowerId'
              p <- optional (symbol ":" >> atomPattern)
              _ <- sc
              case p of
                Just p  -> return $ Required (Just x) p
                Nothing -> return $ Required (Just x) (PVar x)))

    optionalArg =
      symbol "?" >> (
             parens (Optional <$> lowerId <*> (Just <$> (symbol "=" >> expr)))
         <|> Optional <$> lowerId <*> pure Nothing)

opExpr :: Bool -> Parser (Expr String)
opExpr parseSeq = makeExprParser exprInOp (table parseSeq)
  where
    -- See https://caml.inria.fr/pub/docs/manual-caml-light/node4.9.html for
    -- the priorities of the infixes
    table :: Bool -> [[Operator Parser (Expr String)]]
    table parseSeq =
      [ [ Prefix (EPrefix "~-" <$> (getPos <* symbol "~-")) ]
      , [ Prefix (EPrefix "-." <$> (getPos <* symbol "-."))
        , Prefix (EPrefix "-"  <$> (getPos <* symbol "-" ))
        ]
      , [ InfixL (flip EInfix <$> getPos <*> infixFrom '*')
        , InfixL (flip EInfix <$> getPos <*> infixFrom '/')
        , InfixL (flip EInfix <$> getPos <*> infixFrom '%')
        ]
      , [ InfixL (flip EInfix <$> getPos <*> infixFrom '+')
        , InfixL (flip EInfix <$> getPos <*> infixFrom '-')
        ]
      , [ InfixR (ECons       <$> (getPos <* symbol "::" ))
        ]
      , [ InfixR (flip EInfix <$> getPos <*> infixFrom '@')
        , InfixR (flip EInfix <$> getPos <*> infixFrom '^')
        ]
      , [ InfixL (EInfix "="  <$> (getPos <* symbol "="  ))
        , InfixL (EInfix ">=" <$> (getPos <* symbol ">=" ))
        , InfixL (EInfix "<=" <$> (getPos <* symbol "<=" ))
        , InfixL (EInfix "<>" <$> (getPos <* symbol "<>" ))
        , InfixL (EInfix ">"  <$> (getPos <* symbol ">"  ))
        , InfixL (EInfix "<"  <$> (getPos <* symbol "<"  ))
        ]
      , [ InfixL (EInfix "|>" <$> (getPos <* symbol "|>"))
        ]
      , [ InfixL (EInfix "&&" <$> (getPos <* symbol "&&" ))
        ]
      , [ InfixL (EInfix "||" <$> (getPos <* symbol "||" ))
        ]
      , if parseSeq then [ InfixR (ESeq <$> (getPos <* symbol ";")) ] else []
      ]

bracketExpr :: Parser (Expr String)
bracketExpr = do
  elems <- brackets $ sepBy exprInList (symbol ";")
  case elems of
    [] -> return ENil
    _  -> do p <- getPos
             return $ foldr (ECons p) ENil elems

atomOrAppExpr :: Parser (Expr String)
atomOrAppExpr = do
  func <- atomDotExpr
  args <- many (labeledArg <|> (Right <$> atomDotExpr))
  case args of
    [] -> return func
    _  -> do pos <- getPos
             return $ EAppEnd (foldl (go pos) func args)
  where
    labeledArg = do
      label <- char '~' >> lowerId'
      expr <- optional (symbol ":" >> atomDotExpr) <* sc
      return $ Left (label, expr)

    go p acc (Right a)           = EApp Nothing  p acc a
    go p acc (Left (l, Just a))  = EApp (Just l) p acc a
    go p acc (Left (l, Nothing)) = EApp (Just l) p acc (EVar l)

atomDotExpr :: Parser (Expr String)
atomDotExpr = do
  e <- atomExpr
  xs <- many (symbol "." >> Left <$> lowerId <|> Right <$> parens expr)
  pos <- getPos
  return $ foldl (\acc x -> case x of
                              Left x -> EField pos acc x
                              Right i -> EGet pos e i) e xs

atomExpr :: Parser (Expr String)
atomExpr = EConst  <$> constantLiteral
       <|> EVar    <$> lowerId
       <|> ELetIn  <$> (DOpen <$> try (upperId' <* char '.')) <*> atomExpr
       <|> ECtor   <$> upperId
       <|> ETag    <$> (char '`' >> ident)
       <|> EUnit   <$  try (parens sc)
       <|> EVar    <$> try (parens (infixSymbol <* sc))
       <|> bracesExpr
       <|> parensExpr
       <|> arrayExpr
       <|> bracketExpr
       <?> "atomic expression"

parensExpr :: Parser (Expr String)
parensExpr = parens $ do
  e <- expr
  t <- optional (symbol ":" >> typeAnnot)
  case t of
    Nothing -> return e
    Just t  -> do pos <- getPos
                  return $ EType pos e t

bracesExpr :: Parser (Expr String)
bracesExpr = do
  ts <- braces (sepBy fields (symbol ";"))
  pos <- getPos
  return $ ERecord pos ts
 where
  fields = do
    field <- lowerId
    value <- option (EVar field) (symbol "=" >> exprInList)
    return (field, value)

arrayExpr :: Parser (Expr String)
arrayExpr = EArray <$> (symbol "[|" >> getPos)
                   <*> (sepBy exprInList (symbol ";") <* symbol "|]")

pattern :: Parser (Pattern String)
pattern = label "pattern" $ do
  ps <- sepBy1 opPattern comma
  case ps of
    [p] -> return p
    _   -> return (PTuple ps)

opPattern :: Parser (Pattern String)
opPattern = makeExprParser atomPattern table
  where
    table :: [[Operator Parser (Pattern String)]]
    table =
      [ [ InfixR (PCons <$ symbol "::" ) ]
      ]

arrayPattern :: Parser (Pattern String)
arrayPattern = do
  elems <- symbol "[|" >> sepBy pattern (symbol ";") <* symbol "|]"
  -- Temporarily using list pattern for array pattern
  case elems of
    []  -> return PNil
    _   -> return $ foldr PCons PNil elems

bracketPattern :: Parser (Pattern String)
bracketPattern = do
  elems <- brackets $ sepBy pattern (symbol ";")
  case elems of
    []  -> return PNil
    _   -> return $ foldr PCons PNil elems

bracePattern :: Parser (Pattern String)
bracePattern = do
  path <- symbol "{" >> many (upperId <* symbol ".")
  head <- field
  tail <- many (try (symbol ";" >> field))
  _ <- optional (symbol ";" >> symbol "_") >> symbol "}"
  return $ PRecord path (head : tail)
 where
  field = do
    x <- lowerId
    p <- optional (symbol "=" >> pattern)
    case p of
      Nothing -> return (x, PVar x)
      Just p' -> return (x, p')

parenPattern :: Parser (Pattern String)
parenPattern = parens $ do
  p <- pattern
  t <- optional (symbol ":" >> typeAnnot)
  case (p, t) of
    (_, Nothing) -> return p
    (PVar x, Just t') -> return (PType x t')
    _ -> fancyFailure (Set.singleton (ErrorFail "Expected a pattern variable"))

atomPattern :: Parser (Pattern String)
atomPattern =
      PConst    <$> (constantLiteral <|> negativeNumLiteral)
  <|> PVar      <$> lowerId
  <|> PWildcard <$ symbol "_" -- must come after PVar
  <|> PUnit     <$ try (parens sc)
  <|> PTag      <$> (symbol "`" >> lowerId)
  <|> PConstr   <$> upperId <*> optional atomPattern
  <|> parenPattern
  <|> arrayPattern
  <|> bracketPattern
  <|> bracePattern
  <?> "atomic pattern"
