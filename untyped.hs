import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Control.Applicative (many)
import Data.Char (isAlphaNum, isLetter)
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe)

-- ###################################### Grammar ######################################

-- Data type for Lambda Calculus terms
data LambdaTerm
    = Var String          -- Represents a variable by its name
    | Abs String LambdaTerm -- Represents an abstraction with a parameter (variable name) and a body (another lambda term)
    | App LambdaTerm LambdaTerm -- Represents an application of one lambda term to another
    deriving (Eq, Show)

-- ###################################### Lexer ######################################

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser emptyDef {
    Token.identStart = satisfy isLetter,
    Token.identLetter = satisfy isAlphaNum,
    Token.reservedOpNames = ["\\", "."]
}

-- Parser using the lexer
identifier :: Parser String
identifier = Token.identifier lexer  -- Parses identifiers using the lexer rules

-- ###################################### Parser ######################################

-- Definition of parsers using lexer for whitespace
lambdaVar :: Parser LambdaTerm
lambdaVar = do
    Var <$> identifier

lambdaAbs :: Parser LambdaTerm
lambdaAbs = do
    Token.reservedOp lexer "\\"    -- Use lexer to parse the lambda symbol
    v <- identifier
    Token.reservedOp lexer "."    -- Use lexer to parse the arrow
    Abs v <$> lambdaTerm

parens :: Parser LambdaTerm
parens = Token.parens lexer lambdaTerm  -- Use lexer to manage parentheses

lambdaApp :: Parser LambdaTerm
lambdaApp = chainl1 (parens <|> lambdaVar) (Token.whiteSpace lexer >> return App)

lambdaTerm :: Parser LambdaTerm
lambdaTerm = Token.whiteSpace lexer *> (try lambdaAbs <|> lambdaApp) <* Token.whiteSpace lexer

-- ###################################### Evaluator ######################################

-- Evaluates lambda calculus expressions by performing beta-reductions
eval :: LambdaTerm -> LambdaTerm
eval expr = case expr of
    Var x -> Var x  -- Variables are irreducible
    Abs x body -> Abs x (eval body)  -- Evaluate the body of abstractions recursively
    App (Abs x body) arg -> eval (substitute x arg body)  -- Perform substitution and evaluate
    App f a -> App (eval f) (eval a)  -- Evaluate both the function and the argument

-- Substitute the variable 'x' in 'body' with 'arg'
substitute :: String -> LambdaTerm -> LambdaTerm -> LambdaTerm
substitute x arg body = case body of
    Var y
        | y == x -> arg  -- Replace the variable with the argument
        | otherwise -> Var y
    Abs y body'
        | y == x -> Abs y body'  -- If the variable is the same as the abstraction's parameter, skip it
        | otherwise -> Abs y (substitute x arg body')
    App f a -> App (substitute x arg f) (substitute x arg a)

-- ###################################### Pretty Printer ######################################

-- Converts a LambdaTerm into a human-readable string
prettyPrint :: LambdaTerm -> String
prettyPrint (Var x) = x  -- Print variable names directly
prettyPrint (Abs x body) = "\\" ++ x ++ " . " ++ prettyPrint body  -- Correctly format abstractions
prettyPrint (App f a) = "(" ++ prettyPrint f ++ " " ++ prettyPrint a ++ ")"  -- Format applications with proper parentheses

-- ###################################### Main ######################################
main :: IO ()
main = do
    --putStrLn "Or enter your own expression:"
    putStr "Enter expression: "
    hFlush stdout
    input <- getLine
    case parse lambdaTerm "" input of
        Left err -> putStrLn ("Error parsing: " ++ show err)
        Right expr -> putStrLn ("Reduced expression: " ++ prettyPrint (eval expr))
