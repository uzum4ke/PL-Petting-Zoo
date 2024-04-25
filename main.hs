data Expr
    = Lam String Expr
    | App Expr Expr
    | Ann Expr Type
    | Var String
    deriving (Show, Eq)

data Type
    = Arrow Type Type
    | TypeName String
    deriving (Show, Eq)

data SExp
    = SSymbol String
    | SList [SExp]
    deriving (Show, Eq)

parseExpr :: SExp -> Expr
parseExpr s = case s of
    SList [SSymbol "\\", SSymbol x, e, SSymbol ":", t] -> Ann (Lam x (parseExpr e)) (parseType t)
    SList [e1, e2]                                    -> App (parseExpr e1) (parseExpr e2)
    SSymbol x                                         -> Var x
    _                                                 -> error $ "bad syntax in expression: " ++ show s

parseType :: SExp -> Type
parseType t = case t of
    SList [SSymbol t1, SSymbol "->", SSymbol t2] -> Arrow (TypeName t1) (TypeName t2)
    SSymbol x                                    -> TypeName x
    _                                            -> error $ "bad syntax in type: " ++ show t

main :: IO ()
main = do
    let exampleExpr = SList [SSymbol "\\", SSymbol "x", SList [SSymbol "f", SSymbol "x"], SSymbol ":", SList [SSymbol "Int", SSymbol "->", SSymbol "Int"]]
    let parsedExpr = parseExpr exampleExpr
    putStrLn "Parsed Expression:"
    print parsedExpr

    let exampleType = SList [SSymbol "Int", SSymbol "->", SSymbol "Int"]
    let parsedType = parseType exampleType
    putStrLn "\nParsed Type:"
    print parsedType
