
module Paper.Haskell2.Stage3(stage3) where

import Data.List
import Paper.Haskell2.Type


-- should be removing [T]Expr and [T]Variable
stage3 :: [HsItem] -> [HsItem]
stage3 = map f
    where
        f (HsItem Stmt pos x files) = HsItem Stmt pos (fixStmt x) files
        f x = x


fixStmt = unlines . map f . lines
    where
        f x | "module" `isPrefixOf` x = "-- " ++ x
            | otherwise = x

