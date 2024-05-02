{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
module Main where

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr
import Data.Functor.Identity (Identity)

-- ######################## Grammar ########################

-- Definition of the basic types in PCF
data Type = Nat                      -- Represents the type of natural numbers
          | Fun Type Type            -- Represents function types
          deriving (Show, Eq)

-- Definition of the expressions in PCF
data Expr = Var String               -- A variable
          | Lambda String Type Expr  -- Lambda abstraction, note the inclusion of a type for the argument
          | App Expr Expr            -- Function application
          | Y Type Expr              -- Y combinator for creating recursive functions, with type annotation
          | Zero                     -- Represents the natural number 0
          | Succ Expr                -- Represents the successor function (n + 1)
          | Pred Expr                -- Represents the predecessor function (n - 1)
          | If Expr Expr Expr        -- If-then-else expression
          deriving (Show, Eq)

-- ######################## Lexer ########################

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = emptyDef {
               Token.identStart = letter
             , Token.identLetter = alphaNum <|> char '_'
             , Token.reservedNames = ["if", "then", "else", "Y", "zero", "succ", "pred"]
             , Token.reservedOpNames = ["->", "\\", ".", ":"]
             }

--stringLiteral = Token.stringLiteral lexer
identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer

-- ######################## Parser ########################

expr :: Parser Expr
expr = buildExpressionParser operators term

term :: Parser Expr
term =  parens expr
    <|> try ifExpr
    <|> lambdaExpr
    <|> (Zero <$ reserved "zero")
    <|> (try $ Succ <$> (reserved "succ" *> parens expr))
    <|> (try $ Pred <$> (reserved "pred" *> parens expr))
    <|> (Var <$> identifier)
    <|> (Y <$> (reserved "Y" *> parens typeParser) <*> term)

operators :: OperatorTable String () Identity Expr
operators = [
    [Infix (do { whiteSpace; return App }) AssocLeft]
  ]

typeParser :: Parser Type
typeParser = (Nat <$ reserved "Nat")
         <|> (parens $ Fun <$> (typeParser <* reservedOp "->") <*> typeParser)

-- Parse Types
parseType :: Parser Type
parseType = parseBasicType <|> parseComplexType

parseBasicType :: Parser Type
parseBasicType = reserved "Nat" >> return Nat

parseComplexType :: Parser Type
parseComplexType = try (parens $ parseType `chainl1` typeOp)

typeOp :: Parser (Type -> Type -> Type)
typeOp = reservedOp "->" >> return Fun

ifExpr :: Parser Expr
ifExpr = do
  reserved "if"
  cond <- expr
  reserved "then"
  tr <- expr
  reserved "else"
  fl <- expr
  return $ If cond tr fl

lambdaExpr :: Parser Expr
lambdaExpr = do
    reservedOp "\\"
    var <- identifier
    whiteSpace
    reservedOp ":"
    varType <- typeParser
    whiteSpace
    reservedOp "."
    body <- expr
    return $ Lambda var varType body

main :: IO ()
main = do
    let testExpr = "\\x : Nat . succ y"
    print $ parse (whiteSpace >> expr) "" testExpr
