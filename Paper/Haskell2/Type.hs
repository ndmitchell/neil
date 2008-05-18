
module Paper.Haskell2.Type where



data Pos = Pos FilePath !Int
           deriving Show

data HsLow = HsDef   Pos String
           | HsCheck Pos Bool String String
             deriving Show

-- HsCheck a b c d
--     a: line number 
--     b: True is Expr, False is Stmt
--     c: any proceeding command, possibly none
--        \ignore, or \hs{command}
--     d: code


data HsItem = Import Pos String
            | Stmt   Pos String [String]
            | Expr   Pos String
            | TExpr  Pos String
            | Variable   String
            | TVariable  String
              deriving Show
