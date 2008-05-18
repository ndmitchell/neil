
module Paper.Haskell2.Stage4(stage4) where

import Data.Char
import Data.List
import System.FilePath
import Paper.Haskell2.Type


stage4 :: FilePath -> [HsItem] -> [(FilePath,String)]
stage4 file xs = (filename "", importer) : [(filename n, text n) | n <- need]
    where
        filename n = dropFileName file </> modname n <.> "hs"
        modname n = capital (takeBaseName file) ++ n
        need = map ('_':) $ nub $ sort $ concatMap itemFiles xs

        importer = unlines $ ("module " ++ modname "" ++ " where") :
                             ["import " ++ modname n | n <- need]

        text un@('_':n) = unlines $ ("module " ++ modname un ++ " where") :
                                    concatMap f items
            where items = filter ((\i -> null i || n `elem` i) . itemFiles) xs

        f (HsItem Stmt pos x _) = [linePragma pos, fixStmt x, ""]
        f _ = [] -- TODO: Hides wrong answers!



fixStmt = unlines . map f . lines
    where
        f x | "module" `isPrefixOf` x = "-- " ++ x
            | otherwise = x


capital (x:xs) = toUpper x : xs
