
module Paper.Haskell2.Stage4(stage4) where

import Data.Char
import Data.List
import System.FilePath
import Paper.Haskell2.Type


stage4 :: FilePath -> [HsItem] -> [(FilePath,String)]
stage4 file xs = (filename "", importer) : [(filename (show n), text n) | n <- need]
    where
        filename n = dropFileName file </> modname n <.> "hs"
        modname n = capital (takeBaseName file) ++ n
        need = nub $ sort $ concatMap itemFiles xs

        importer = unlines $ ("module " ++ modname "" ++ " where") :
                             ["import " ++ modname (show n) | n <- need]

        text n = unlines $ ("module " ++ modname (show n) ++ " where") :
                           concatMap (f n) xs

        f n (HsItem Stmt pos x files) | null files || n `elem` files = [linePragma pos, fixStmt x, ""]
        f n _ = [] -- TODO: Hides wrong answers!



fixStmt = unlines . map f . lines
    where
        f x | "module" `isPrefixOf` x = "-- " ++ x
            | otherwise = x


capital (x:xs) = toUpper x : xs
