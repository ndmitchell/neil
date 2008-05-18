
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
        (stmts,exprs) = partition ((==) Stmt . lowType) checks
        (useNames,stmts2) = unzip $ map parseStmt stmts
        exprs2 = zipWith parseExpr [1..] exprs


fixExpr n x | all isHaskellSymbol $ trim x = fixExpr n ("(" ++ x ++ ")")
            | otherwise = "auto_" ++ show n ++ " = " ++ x


parseDefs :: HsLow -> ([String], [HsItem])
parseDefs (HsDef pos x) | "instance " `isPrefixOf` x  || "import " `isPrefixOf` x
                            = ([],[HsItem Stmt pos x Always])
                        | null x = ([],[])


parseStmt :: HsLow -> ([String],HsItem)
parseStmt (HsCheck pos Stmt whr x) = ([], HsItem Stmt pos (fakeImplement x) whr)


parseExpr :: Int -> HsLow -> HsItem
parseExpr n (HsCheck pos expr whr x) = HsItem Stmt pos (fixExpr n x) whr
