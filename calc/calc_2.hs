import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Expr
import Text.Parsec.Prim (parse)
import System.IO (hFlush, stdout)

-- Define the expression data type to represent arithmetic expressions.
data Expr
  = Lit Integer            -- Represents a literal integer.
  | Var String             -- Represents a variable.
  | Add Expr Expr          -- Represents addition of two expressions.
  | Mul Expr Expr          -- Represents multiplication of two expressions.
  | Exp Expr Expr          -- Represents exponentiation.
  | Let String Expr Expr   -- Represents a 'let' binding (local variable definition).
  deriving (Eq, Show)

-- Parse an identifier, using the lexer to handle tokenization.
identifier :: Parser String
identifier = Token.identifier lexer

-- Define the lexer to handle specific syntax related to programming languages.
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where style = emptyDef {
             Token.identStart = letter
           , Token.identLetter = alphaNum <|> oneOf "_'"
           , Token.reservedNames = ["let", "in"]
           , Token.reservedOpNames = ["+", "*", "^", "="]
           }

-- Utility functions for parsing using the lexer.
integer :: Parser Integer
integer = Token.integer lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

-- Define the parser for expressions, setting up precedence and associativity.
expr :: Parser Expr
expr = buildExpressionParser table term
  where
    table = [[Infix (reservedOp "^" >> return Exp) AssocRight]
            ,[Infix (reservedOp "*" >> return Mul) AssocLeft]
            ,[Infix (reservedOp "+" >> return Add) AssocLeft]]
    term = parens expr
        <|> Var <$> identifier
        <|> Lit <$> integer
        <|> parseLet

-- Parser for 'let' expressions, which define a local variable.
parseLet :: Parser Expr
parseLet = do
  reserved "let"
  var <- identifier
  reservedOp "="
  expr1 <- expr
  reserved "in"
  expr2 <- expr
  return $ Let var expr1 expr2

-- Parses a string into an Expr, handling the entire input until EOF.
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

-- Type for representing an environment of variable bindings.
type Environment = [(String, Integer)]

-- Evaluate an expression within a given environment.
eval :: Environment -> Expr -> Integer
eval env (Lit n) = n
eval env (Var x) = case lookup x env of
                     Just n -> n
                     Nothing -> error "Undefined variable"
eval env (Add x y) = eval env x + eval env y
eval env (Mul x y) = eval env x * eval env y
eval env (Exp x y) = eval env x ^ eval env y
eval env (Let var expr1 expr2) = 
  let varValue = eval env expr1
  in eval ((var, varValue) : env) expr2

-- Main function to drive the program.
main :: IO ()
main = do
    putStrLn "Enter an arithmetic expression:"
    hFlush stdout  -- Ensure that the prompt is displayed before user input.
    input <- getLine
    case parseString input of
        Left err -> print err  -- Print parse errors.
        Right expr -> do
            let result = eval [] expr  -- Evaluate the parsed expression.
            putStrLn ("The result of the expression is: " ++ show result)