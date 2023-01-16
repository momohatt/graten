module GraTen.Parser.Token
  ( Parser
  , sc
  , getPos
  , constantLiteral
  , positiveNumLiteral
  , negativeNumLiteral
  , positiveIntegerLiteral
  , positiveIntLiteral
  , boolLiteral
  , charLiteral
  , stringLiteral
  , parens
  , brackets
  , braces
  , comma
  , symbol
  , identChar
  , infixFrom
  , infixSymbol
  , prefixSymbol
  , reserved
  , ident
  , lowerId
  , lowerId'
  , upperId
  , upperId'
  , lowerReservedWords
  ) where

import           Data.Functor               (($>))
import           Data.Void                  (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char       (lowerChar, upperChar, digitChar, alphaNumChar, string, char, space1)
import qualified Text.Megaparsec.Char.Lexer as L

import           GraTen.Syntax.Literal
import qualified GraTen.Syntax.Literal      as G


type Parser = Parsec Void String

-- Space Comsumer
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt  = L.skipLineComment "--"
    blockCmnt = L.skipBlockCommentNested "(*" "*)"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

getPos :: Parser G.Pos
getPos = do
  p <- getSourcePos
  return (unPos (sourceLine p), unPos (sourceColumn p))

constantLiteral :: Parser ConstantLiteral
constantLiteral =
      positiveNumLiteral
  <|> LBool   <$> boolLiteral
  <|> LChar   <$> charLiteral
  <|> LString <$> stringLiteral

-- Integer or Float
positiveNumLiteral :: Parser ConstantLiteral
positiveNumLiteral = (LFloat <$> lexeme (try L.float)) <|> ocamlFloat
  where
    ocamlFloat = lexeme $ do
      x <- (:) <$> digitChar <*> many (digitChar <|> char '_')
      y' <- optional (char '.')
      case y' of
        Nothing -> return $ LInt (read (filter (/= '_') x) :: Integer)
        Just _  -> return $ LFloat (read (filter (/= '_') x) :: Double)

positiveIntegerLiteral :: Parser Integer
positiveIntegerLiteral = lexeme L.decimal

positiveIntLiteral :: Parser Int
positiveIntLiteral = lexeme L.decimal

negativeNumLiteral :: Parser ConstantLiteral
negativeNumLiteral = do
  n <- try (symbol "-" >> positiveNumLiteral)
  case n of
    LInt x   -> return $ LInt (-x)
    LFloat x -> return $ LFloat (-x)
    _        -> error "Unreachable"

boolLiteral :: Parser Bool
boolLiteral = reserved "true"  $> True
          <|> reserved "false" $> False
          <?> "boolean"

charLiteral :: Parser Char
charLiteral = char '\'' >> L.charLiteral <* symbol "\'"

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (symbol "\"")
            <?> "string"

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

brackets :: Parser a -> Parser a
brackets  = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces  = between (symbol "{") (symbol "}")

comma :: Parser String
comma = symbol ","

symbol :: String -> Parser String
symbol sym = try $ L.symbol sc sym

identChar :: Parser Char
identChar = alphaNumChar <|> oneOf "_'"

opChar :: Parser Char
opChar = coreOpChar <|> oneOf "~!?%<:."

coreOpChar :: Parser Char
coreOpChar = oneOf "$&*+-/=>@^|"

infixFrom :: Char -> Parser String
infixFrom c = lexeme $ (c:) <$> (char c >> many opChar)

-- See https://ocaml.org/manual/lex.html#sss:lex-ops-symbols
infixSymbol :: Parser String
infixSymbol =
  lexeme $
      (:) <$> (coreOpChar <|> oneOf "%<") <*> many opChar
  <|> ('#':) <$> (char '#' >> some opChar)

prefixSymbol :: Parser String
prefixSymbol =
      (:) <$> char '!' <*> many opChar
  <|> (:) <$> oneOf "?~" <*> some opChar

reserved :: String -> Parser ()
reserved w = (lexeme . try) (string w *> notFollowedBy identChar)

ident :: Parser String
ident = lowerId <|> upperId

lowerId :: Parser String
lowerId = lexeme lowerId'

lowerId' :: Parser String
lowerId' = try ((p >>= check) <|> p2)
  where
    p       = (:) <$> lowerChar <*> many identChar
    p2      = (:) <$> char '_' <*> some identChar
    check x = if x `elem` lowerReservedWords
                then fail $ "keyword " ++ show x ++ " cannot be an identifier"
                else return x

upperId :: Parser String
upperId = lexeme upperId'

upperId' :: Parser String
upperId' = try ((:) <$> upperChar <*> many identChar)

-- See https://ocaml.org/manual/lex.html#sss:keywords
lowerReservedWords :: [String]
lowerReservedWords =
  [ "and"
  , "as"
  , "assert"
  , "asr"
  , "begin"
  , "class"
  , "constraint"
  , "do"
  , "done"
  , "downto"
  , "else"
  , "end"
  , "exception"
  , "external"
  , "false"
  , "for"
  , "fun"
  , "function"
  , "functor"
  , "if"
  , "in"
  , "include"
  , "inherit"
  , "initializer"
  , "land"
  , "lazy"
  , "let"
  , "lor"
  , "lsl"
  , "lsr"
  , "lxor"
  , "match"
  , "method"
  , "mod"
  , "module"
  , "mutable"
  , "new"
  , "nonrec"
  , "object"
  , "of"
  , "open"
  , "or"
  , "private"
  , "rec"
  , "sig"
  , "struct"
  , "then"
  , "to"
  , "true"
  , "try"
  , "type"
  , "val"
  , "virtual"
  , "when"
  , "while"
  , "with"
  ]
