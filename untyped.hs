import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)
import Control.Applicative (many)
import Data.Char (isAlphaNum, isLetter)
import System.IO (hFlush, stdout)

-- Data type for Lambda Calculus terms
data LambdaTerm
    = Var String          -- Represents a variable by its name
    | Abs String LambdaTerm -- Represents an abstraction with a parameter (variable name) and a body (another lambda term)
    | App LambdaTerm LambdaTerm -- Represents an application of one lambda term to another
    deriving (Eq, Show)

-- Lexer setup
lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef {
    P.identStart = satisfy isLetter,
    P.identLetter = satisfy isAlphaNum,
    P.reservedOpNames = ["\\", "->"]
}

-- Parser using the lexer
identifier :: Parser String
identifier = P.identifier lexer  -- Parses identifiers using the lexer rules

-- Definition of parsers using lexer for whitespace
lambdaVar :: Parser LambdaTerm
lambdaVar = do
    Var <$> identifier

lambdaAbs :: Parser LambdaTerm
lambdaAbs = do
    P.reservedOp lexer "\\"    -- Use lexer to parse the lambda symbol
    v <- identifier
    P.reservedOp lexer "->"    -- Use lexer to parse the arrow
    Abs v <$> lambdaTerm

parens :: Parser LambdaTerm
parens = P.parens lexer lambdaTerm  -- Use lexer to manage parentheses

lambdaApp :: Parser LambdaTerm
lambdaApp = chainl1 (parens <|> lambdaVar) (P.whiteSpace lexer >> return App)

lambdaTerm :: Parser LambdaTerm
lambdaTerm = P.whiteSpace lexer *> (try lambdaAbs <|> lambdaApp) <* P.whiteSpace lexer

main :: IO ()
main = do
    --putStrLn "Or enter your own expression:"
    putStr "Enter expression: \t"
    hFlush stdout
    input <- getLine
    case parse lambdaTerm "" input of
        Left err -> putStrLn ("Error parsing: " ++ show err)
        Right expr -> putStrLn ("Parsed expression: " ++ show expr)