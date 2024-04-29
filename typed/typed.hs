{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr

-- ######################## Grammar ########################

data Type = TInt
          | TBool
          | TFun Type Type
          | TPair Type Type
          deriving (Show, Eq)

data Term = Var String Type           -- Variables
          | LitInt Int                -- Integer literals
          | LitBool Bool              -- Boolean literals
          | Lambda String Type Term   -- Lambda abstraction
          | App Term Term             -- Application of functions
          | Add Term Term             -- Addition of integers
          | Mul Term Term             -- Multiplication of integers
          | If Term Term Term         -- Conditional expressions
          | And Term Term             -- Logical Conjunction
          | Or Term Term              -- Logical Disjunction
          | Not Term                  -- Logical Negation
          | Pair Term Term            -- Tuple constructor
          | Fst Term                  -- First projection
          | Snd Term                  -- Second projection
          deriving (Show)

-- ######################## Lexical Analysis ########################

-- Lexer definition
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser lambdaStyle
  where
    lambdaStyle = emptyDef {
      Token.identStart      = letter,
      Token.identLetter     = alphaNum <|> oneOf "_'",
      Token.reservedNames   = ["True", "False", "Int", "Bool"],
      Token.reservedOpNames = [ ".", "\\",
                                "+", "*",
                                "if", "then", "else", "and", "or", "not",
                                ",", "(", ")",
                                "->", ":"]
    }

-- Lexical analysis helpers
integer :: Parser Integer
integer = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

symbol :: String -> Parser String
symbol = Token.symbol lexer

-- ######################## Parser ########################

-- Parse Types
parseType :: Parser Type
parseType = parseBasicType <|> parseComplexType

parseBasicType :: Parser Type
parseBasicType = (reserved "Int" >> return TInt)
              <|> (reserved "Bool" >> return TBool)

parseComplexType :: Parser Type
parseComplexType = try (parens $ parseType `chainl1` typeOp)

typeOp :: Parser (Type -> Type -> Type)
typeOp = (reservedOp "->" >> return TFun)
     <|> (reservedOp "*" >> return TPair)

-- Parse Terms
parseVar :: Parser Term
parseVar = do
    name <- identifier
    spaces  -- Handling potential spaces after variable name but before type annotation
    reservedOp ":"
    typ <- parseType
    return $ Var name typ

-- Parse Literals
parseLitInt :: Parser Term
parseLitInt = LitInt . fromIntegral <$> integer

parseLitBool :: Parser Term
parseLitBool = (reserved "True" >> return (LitBool True))
            <|> (reserved "False" >> return (LitBool False))

-- Parse Expressions 
parseExpr :: Parser Term
--parseExpr = parseTerm `chainl1` (spaces *> return App <* spaces)
parseExpr = spaces *> parseTerm <* spaces

parseTerm :: Parser Term
parseTerm = try parseParens
          <|> try parseLitInt
          <|> try parseLitBool
          <|> try parseVar  -- careful with placement of 'try'
          <|> parseLambda
          <|> parseIf
          <|> try parsePair
          <|> parseFst
          <|> parseSnd
          <|> parseNot
          -- <|> parseBinaryOp

parseParens :: Parser Term
parseParens = parens parseExpr

parseLambda :: Parser Term
parseLambda = do
    reservedOp "\\"
    var <- identifier
    spaces  -- Handling spaces before and after type annotations
    reservedOp ":"
    varType <- parseType
    spaces
    reservedOp "."
    expr <- parseExpr
    return $ Lambda var varType expr


-- Parse it-then-else
parseIf :: Parser Term
parseIf = do
    reserved "if"
    cond <- parseExpr
    spaces
    reserved "then"
    trueExpr <- parseExpr
    spaces
    reserved "else"
    If cond trueExpr <$> parseExpr

-- Parse Not
parseNot :: Parser Term
parseNot = do
    reserved "not"
    expr <- parseExpr  -- or parseTerm depending on how you organize precedence
    return $ Not expr


-- Parse Binary Operations
parseBinaryOp :: Parser Term
parseBinaryOp = buildExpressionParser table parseTerm
  where
    table = [ [Infix (reservedOp "*" >> return Mul) AssocLeft,
               Infix (reservedOp "+" >> return Add) AssocLeft],
              [Infix (reservedOp "and" >> return And) AssocLeft,
               Infix (reservedOp "or" >> return Or) AssocLeft]
            ]

-- Parse tuples
parsePair :: Parser Term
parsePair = do
    reservedOp "("
    t1 <- parseExpr
    reservedOp ","
    t2 <- parseExpr
    reservedOp ")"
    return $ Pair t1 t2

parseFst :: Parser Term
parseFst = Fst <$> (reserved "fst" *> parseTerm)

parseSnd :: Parser Term
parseSnd = Snd <$> (reserved "snd" *> parseTerm)

-- ######################## Run ########################

main :: IO ()
main = do
    let input = "True"
    case parse parseExpr "" input of
        Left err -> print err
        Right expr -> print expr