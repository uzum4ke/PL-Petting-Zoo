import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)

-- ######################## Grammar ########################

data Type = TInt
          | TBool
          | TFun Type Type
          | TPair Type Type  
          deriving (Show, Eq)

data Term = Var String                -- Variables
          | LitInt Int                -- Integer literals
          | LitBool Bool              -- Boolean literals
          | Lambda String Type Term   -- Lambda abstraction
          | App Term Term             -- Application of functions
          | Add Term Term             -- Addition of integers
          | Mul Term Term             -- Multiplication of integers
          | If Term Term Term         -- Conditional expressions
          | Pair Term Term             -- Tuple constructor
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
      Token.reservedNames   = ["True", "False"],
      Token.reservedOpNames = ["+", "*", ",", "->", "(", ")"]
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
