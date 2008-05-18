
module Paper.Haskell2.Type where

import Data.List


data Pos = Pos FilePath !Int
           deriving Show

linePragma :: Pos -> String
linePragma (Pos file line) = "{- # LINE " ++ show line ++ " " ++ show file ++ " # -}"


data HsLow = HsDef   {lowPos :: Pos, lowText :: String}
           | HsCheck {lowPos :: Pos, lowExpr :: Bool, lowCmd :: String, lowText :: String}
             deriving Show

--     lowExpr: True is Expr, False is Stmt
--     lowCmd: any proceeding command, possibly none
--             \ignore, or \hs{command}


data HsItem = HsItem {itemType :: HsType, itemPos :: Pos, itemText :: String, itemFiles :: Where}
              deriving Show

data HsType = Import
            | Stmt
            | Expr
            | TExpr
            | Variable
            | TVariable
              deriving Show


data Where = Always
           | Only [String]
             deriving Show


parseWhere :: [String] -> Where
parseWhere [] = Only ["default"]
parseWhere xs = Only xs


allWhere :: [Where] -> [String]
allWhere xs = nub $ sort $ concat [x | Only x <- xs]


matchWhere :: String -> Where -> Bool
matchWhere x Always = True
matchWhere x (Only xs) = x `elem` xs
