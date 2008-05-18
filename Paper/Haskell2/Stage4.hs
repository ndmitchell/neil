
module Paper.Haskell2.Stage4(stage4) where

import Data.List
import Paper.Haskell2.Type


stage4 :: [HsItem] -> String
stage4 xs = unlines $ "module Temp where" : concatMap f xs
    where
        f (Stmt pos x _) = [linePragma pos, fixStmt x, ""]
        f _ = [] -- TODO: Hides wrong answers!



fixStmt = unlines . map f . lines
    where
        f x | "module" `isPrefixOf` x = "-- " ++ x
            | otherwise = x

        
        