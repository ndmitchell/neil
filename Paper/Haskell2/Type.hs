
module Paper.Haskell2.Type where

import Data.List


data Pos = Pos FilePath !Int
           deriving Show

linePragma :: Pos -> String
linePragma (Pos file line) = "{- # LINE " ++ show line ++ " " ++ show file ++ " # -}"


data HsLow = HsDef   {lowPos :: Pos, lowText :: String}
           | HsCheck {lowPos :: Pos, lowType :: HsType, lowCmd :: String, lowText :: String}
             deriving Show


instance Eq HsLow where
    (HsDef a1 b1) == (HsDef a2 b2) = b1 == b2
    (HsCheck a1 b1 c1 d1) == (HsCheck a2 b2 c2 d2) = b1 == b2 && c1 == c2 && d1 == d2
    _ == _ = False


isHsDef (HsDef{}) = True; isHsDef _ = False

--     lowExpr: True is Expr, False is Stmt
--     lowCmd: any proceeding command, possibly none
--             \ignore, or \hs{command}


data HsItem = HsItem {itemType :: HsType, itemPos :: Pos, itemText :: String, itemWhere :: Where}
              deriving Show

data HsType = Stmt
            | Expr
            | TExpr
              deriving (Eq,Show)


data Where = Always
           | Only [String]
           | OnlyNot [String]
             deriving Show


parseWhere :: [String] -> Where
parseWhere [] = Only ["default"]
parseWhere ["*"] = Always
parseWhere xs | null neg = Only xs
              | otherwise = OnlyNot $ map tail neg
    where neg = filter ("!" `isPrefixOf`) xs


allWhere :: [Where] -> [String]
allWhere = nub . sort . concatMap f
    where
        f (Only x) = x
        f (OnlyNot x) = x
        f _ = []


matchWhere :: String -> Where -> Bool
matchWhere x Always = True
matchWhere x (Only xs) = x `elem` xs
matchWhere x (OnlyNot xs) = x `notElem` xs
