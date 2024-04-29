import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Text.Parsec.Prim (parse)

-- The arithmetic expression ADT
data Expr
  = Lit Integer
  | Add Expr Expr
  | Mul Expr Expr
  | Exp Expr Expr
  deriving (Eq, Show)

-- Lexer definition using Parsec's token parser
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = emptyDef {
               Token.commentLine = "#"
             , Token.reservedOpNames = ["+", "*", "^"]
             }

-- Parser utility functions
integer :: Parser Integer
integer = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

-- Parse expressions based on precedence and associativity
expr :: Parser Expr
expr = buildExpressionParser table term
  where
    table = [ [Infix (reservedOp "^" >> return Exp) AssocRight]
            , [Infix (reservedOp "*" >> return Mul) AssocLeft]
            , [Infix (reservedOp "+" >> return Add) AssocLeft]
            ]
    term = parens expr <|> (Lit <$> integer)

-- Convenience function to parse a string
parseString :: String -> Either ParseError Expr
parseString str = parse (wholeExpr) "" str
  where
    wholeExpr = do
      whiteSpace
      e <- expr
      eof
      return e

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer
