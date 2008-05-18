
module Paper.Haskell2.Type where


data Pos = Pos FilePath !Int
           deriving Show

linePragma :: Pos -> String
linePragma (Pos file line) = "{- # LINE " ++ show line ++ " " ++ show file ++ " # -}"


data HsLow = HsDef   Pos String
           | HsCheck Pos Bool String String
             deriving Show

-- HsCheck a b c d
--     a: line number 
--     b: True is Expr, False is Stmt
--     c: any proceeding command, possibly none
--        \ignore, or \hs{command}
--     d: code


data HsItem = HsItem Pos String HsType

data HsType = Import
            | Stmt
            | Expr
            | TExpr
            | Variable
            | TVariable
              deriving Show
