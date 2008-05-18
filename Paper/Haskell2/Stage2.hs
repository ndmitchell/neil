
module Paper.Haskell2.Stage2(stage2) where

import Data.Char
import Data.List
import Paper.Util.String
import Paper.Haskell2.Type
import Paper.Haskell2.Haskell


stage2 :: [HsLow] -> [HsItem]
stage2 xs = concat defStmts ++ reverse stmts2 ++ exprs2
    where
        (defs,checks) = partition isHsDef $ nub xs
        (defNames,defStmts) = unzip $ map parseDefs defs
        (exprs,stmts) = partition lowExpr $ filter ((==) "ignore" . lowCmd) checks
        (useNames,stmts2) = unzip $ map parseStmt stmts
        exprs2 = zipWith parseExpr [1..] exprs


readCmd :: String -> ([String], String)
readCmd ('@':xs) =  (a:c,d)
    where
        (a,b) = break (== ' ') xs
        (c,d) = readCmd $ drop 1 b
readCmd xs = ([], xs)


fixExpr n x | all isHaskellSymbol $ trim x = fixExpr n ("(" ++ x ++ ")")
            | otherwise = "auto_" ++ show n ++ " = " ++ x


parseDefs :: HsLow -> ([String], [HsItem])
parseDefs (HsDef pos x) | "instance " `isPrefixOf` x  || "import " `isPrefixOf` x
                            = ([],[HsItem Stmt pos x Always])
                        | null x = ([],[])


parseStmt :: HsLow -> ([String],HsItem)
parseStmt (HsCheck pos expr cmd x) = ([], HsItem Stmt pos (fakeImplement x) whr)
    where
        whr = parseWhere files
        (files,_) = readCmd cmd


parseExpr :: Int -> HsLow -> HsItem
parseExpr n (HsCheck pos expr cmd x) = HsItem Stmt pos (fixExpr n x) whr
    where
        whr = parseWhere files
        (files,_) = readCmd cmd
