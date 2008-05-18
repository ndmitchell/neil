
module Paper.Haskell2.Stage2(stage2) where

import Data.Char
import Data.List
import Paper.Util.String
import Paper.Haskell2.Type
import Paper.Haskell2.Haskell


stage2 :: [HsLow] -> [HsItem]
stage2 = concat . zipWith f [1..] . nub
    where
        f n (HsDef pos x) | "instance " `isPrefixOf` x 
                        || "import " `isPrefixOf` x  = [HsItem Stmt pos x Always]
                        | null x = []

        f n (HsCheck pos expr cmd x) | cmd == "ignore" = []
            | expr =      [HsItem Stmt pos (fixExpr n x) whr]
            | otherwise = [HsItem Stmt pos (fakeImplement x) whr]
            where
                whr = parseWhere files
                (files,_) = readCmd cmd

        f n x = error $ "Stage2, todo: " ++ show x


readCmd :: String -> ([String], String)
readCmd ('@':xs) =  (a:c,d)
    where
        (a,b) = break (== ' ') xs
        (c,d) = readCmd $ drop 1 b
readCmd xs = ([], xs)


fixExpr n x | all isHaskellSymbol $ trim x = fixExpr n ("(" ++ x ++ ")")
            | otherwise = "auto_" ++ show n ++ " = " ++ x
