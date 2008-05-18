
module Paper.Haskell2.Stage3(stage3) where

import Data.Char
import Data.List
import Paper.Haskell2.Type
import Paper.Haskell2.Haskell


-- should be removing [T]Expr and [T]Variable
stage3 :: [HsItem] -> [HsItem]
stage3 = zipWith f [1..]
    where
        f n (HsItem Stmt pos x files) = HsItem Stmt pos (fixStmt x) files
        f n (HsItem Expr pos x files) = HsItem Stmt pos (fixExpr n x) files


fixStmt = fakeImplement . unlines . map f . lines
    where
        f x | "module" `isPrefixOf` x = "-- HIDE " ++ x
            | otherwise = x



fixExpr n x | all isHaskellSymbol $ trim x = fixExpr n ("(" ++ x ++ ")")
            | otherwise = "auto_" ++ show n ++ " = " ++ x


isHaskellSymbol = flip elem "|+-*"

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

