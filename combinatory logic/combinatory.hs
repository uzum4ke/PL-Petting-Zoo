import Text.Parsec
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language (emptyDef)

-- Define the data type for combinatory expressions
data Combinator
  = S              -- The S combinator
  | K              -- The K combinator
  | I              -- The I combinator
  | App Combinator Combinator  -- Application of one combinator to another
  deriving (Eq, Show)

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser style
  where style = emptyDef { Token.identLetter = oneOf "SKI" }

-- Utility functions from the lexer
identifier :: Parser String
identifier = Token.identifier lexer
whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

-- Parse individual combinators
combinator :: Parser Combinator
combinator = do
  whiteSpace
  c <- identifier
  case c of
    "S" -> return S
    "K" -> return K
    "I" -> return I
    _   -> fail $ "Unknown combinator: " ++ c

-- Parse applications
expr :: Parser Combinator
expr = chainl1 combinator (return App)

-- Parse a string into a Combinator structure
parseCombinators :: String -> Either ParseError Combinator
parseCombinators = parse expr ""

eval :: Combinator -> Combinator
eval (App I x) = eval x
eval (App (App K x) _) = eval x
eval (App (App (App S x) y) z) = eval (App (eval $ App x z) (eval $ App y z))
eval (App x y) = 
    let ex = eval x
        ey = eval y
    in if ex == x && ey == y then App ex ey
       else eval (App ex ey)
eval x = x  -- Base case for S, K, I without enough arguments to reduce


-- Pretty printer for combinatory expressions
prettyPrint :: Combinator -> String
prettyPrint S = "S"
prettyPrint K = "K"
prettyPrint I = "I"
prettyPrint (App x y) = concat ["(", prettyPrint x, " ", prettyPrint y, ")"]


main :: IO ()
main = do
  putStrLn "Enter a combinatory expression (using S, K, I):"
  input <- getLine
  case parseCombinators input of
    Left err -> print err
    Right ast -> print $ prettyPrint (eval ast)


