
module Paper.Haskell2.Stage4(stage4) where

import Data.Char
import Data.List
import System.FilePath
import Paper.Haskell2.Type
import Paper.Haskell2.Haskell


stage4 :: FilePath -> [HsItem] -> [(FilePath,String)]
stage4 file xs = (filename "", importer) : [(filename n, text n) | n <- need]
    where
        filename n = dropFileName file </> modname n <.> "hs"
        modname n = capital (takeBaseName file) ++ n
        need = map ('_':) $ nub $ sort $ concatMap itemFiles xs

        importer = unlines $ ("module " ++ modname "" ++ " where") :
                             ["import " ++ modname n | n <- need]

        text un@('_':n) = unlines $ ("module " ++ modname un ++ " where") :
                                    render items
            where items = filter ((\i -> null i || n `elem` i) . itemFiles) xs


render = f [] . zip [1..] . reverse
    where
        f seen [] = []
        f seen ((n,HsItem Stmt pos x _) : xs) = linePragma pos : x2 : "" : f seen2 xs
            where
                def = defines x
                bad = def `intersect` seen
                x2 = rename [(b, prime n b) | b <- bad] x
                seen2 = def `union` seen

        f seen (x:xs) = f seen xs -- TODO: Hiding errors here

capital (x:xs) = toUpper x : xs


prime n name = name ++ "''" ++ show n
