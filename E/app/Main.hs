{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module Main where

import Data.Maybe (isJust, fromJust)

import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Expr

-- ######################## Grammar ########################

-- | Data type representing types in our language. Currently supports numerical and string types.
data Typ = Num | Str
  deriving (Show, Eq)

-- | Data type for expressions in our language. This includes variables, literals, arithmetic and string operations, length calculations, and let bindings.
data Exp = Var String              -- ^ Variable expression with a name
         | NumLit Integer          -- ^ Numerical literal
         | StrLit String           -- ^ String literal
         | Plus Exp Exp            -- ^ Addition of two expressions
         | Times Exp Exp           -- ^ Multiplication of two expressions
         | Cat Exp Exp             -- ^ Concatenation of two expressions
         | Len Exp                 -- ^ Length of an expression (number of characters if string)
         | Let Exp String Exp      -- ^ Let binding for creating local bindings
  deriving (Show, Eq)

-- ######################## Lexer ########################

-- | Defines a lexer for the language using Parsec's built-in token parser generator.
lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where
    style = emptyDef {
                Token.identStart      = letter,      -- Identifiers start with a letter
                Token.identLetter     = alphaNum,    -- Identifiers can contain alphanumeric characters
                Token.reservedNames   = ["let", "be", "in"],  -- Reserved keywords
                Token.reservedOpNames = ["+", "*", "^"]       -- Reserved operators
            }

-- | Shorthand functions to simplify parsing of specific elements using the lexer.
integer = Token.integer lexer        -- Parses integers
stringLiteral = Token.stringLiteral lexer  -- Parses string literals
parens = Token.parens lexer          -- Parses expressions within parentheses
identifier = Token.identifier lexer  -- Parses identifiers
reserved = Token.reserved lexer      -- Parses reserved keywords
reservedOp = Token.reservedOp lexer  -- Parses reserved operators
whiteSpace = Token.whiteSpace lexer  -- Parses and skips whitespace

-- ######################## Parser ########################

-- | The main expression parser, builds expression parsers based on operators and terms.
expression :: Parser Exp
expression = buildExpressionParser operators term
  where
    operators = [ [ Infix (reservedOp "+" >> return Plus) AssocLeft
                  , Infix (reservedOp "*" >> return Times) AssocLeft
                  ]
                , [ Infix (reservedOp "^" >> return Cat) AssocLeft ]
                ]

-- | Parses terms in our language which could be any of the basic expressions.
term = parens expression
    <|> (NumLit <$> integer)
    <|> (StrLit <$> stringLiteral)
    <|> (Var <$> identifier)
    <|> letExpr
    <|> lenExpr

-- | Parses 'let' expressions.
letExpr = do
    reserved "let"
    name <- identifier
    reserved "be"
    expr1 <- expression
    reserved "in"
    expr2 <- expression
    return $ Let expr1 name expr2

-- | Parses the length expression '| expression |'.
lenExpr :: Parser Exp
lenExpr = do
    char '|'  
    spaces  
    expr <- expression  
    spaces  
    char '|'  
    return $ Len expr

-- ######################## Type Checker ########################

typCheck :: Exp -> Maybe Typ
typCheck (NumLit _) = Just Num
typCheck (StrLit _) = Just Str
typCheck (Plus e1 e2) =
    if typCheck e1 == Just Num && typCheck e2 == Just Num 
        then Just Num
    else Nothing
typCheck (Times e1 e2) =
    if typCheck e1 == Just Num && typCheck e2 == Just Num 
        then Just Num
    else Nothing
typCheck (Cat e1 e2) =
    if typCheck e1 == Just Str && typCheck e2 == Just Str 
        then Just Str
    else Nothing
typCheck (Len e) =
    if typCheck e == Just Str 
        then Just Num
    else Nothing
typCheck (Let e1 _ e2) =
    if isJust (typCheck e1) 
        then typCheck e2  -- Check e1 for type correctness, but return type of e2
    else Nothing

-- ######################## Main ########################

-- | Main function to execute the parser. It reads input from the user, tries to parse it,
-- | prints the parsed expression and its type check result.
main :: IO ()
main = do
    putStrLn "Enter an expression:"  -- Prompt user for input
    input <- getLine  -- Read input from the user
    case parse expression "" input of  -- Try to parse the input
        Left err -> print err  -- If there's a parsing error, print it
        Right expr -> do  -- If parsing is successful
            print expr  -- Print the parsed expression
            putStr "Type Check Result:" 
            print (typCheck expr)  -- Print the result of the type check




