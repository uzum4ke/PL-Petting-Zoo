import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Text.Parsec.Prim (parse)
import System.IO (hFlush, stdout)

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
    term = parens expr <|> Lit <$> integer

-- Convenience function to parse a string
parseString :: String -> Either ParseError Expr
parseString = parse wholeExpr ""
  where
    wholeExpr = do
      whiteSpace
      e <- expr
      eof
      return e

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- Function to evaluate the expression
eval :: Expr -> Integer
eval (Lit n)   = n
eval (Add x y) = eval x + eval y
eval (Mul x y) = eval x * eval y
eval (Exp x y) = eval x ^ eval y



main :: IO ()
main = do
    putStrLn "Enter an arithmetic expression:"
    hFlush stdout  -- Make sure the prompt appears before input is entered
    input <- getLine
    case parseString input of
        Left err -> print err  -- If parsing fails, print the error
        Right expr -> do
            let result = eval expr  -- Evaluate the expression
            putStrLn ("The result of the expression is: " ++ show result)
