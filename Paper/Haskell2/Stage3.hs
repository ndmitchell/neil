
module Paper.Haskell2.Stage3(stage3) where

import Data.Char
import Data.List
import System.FilePath
import Paper.Haskell2.Type
import Paper.Haskell2.Haskell


exts = "NoMonomorphismRestriction, MultiParamTypeClasses, FlexibleContexts, " ++
       "FlexibleInstances, UndecidableInstances, Rank2Types"

prefix = "{-# LANGUAGE " ++ exts ++ " #-}\n" ++
         "{-# OPTIONS_GHC -fno-warn-missing-methods -fno-warn-overlapping-patterns #-}\n"


stage3 :: FilePath -> [HsItem] -> [(FilePath,String)]
stage3 file xs = (filename "", importer) : [(filename n, text n) | n <- need]
    where
        filename n = dropFileName file </> modname n <.> "hs"
        modname n = capital (takeBaseName file) ++ ['_'| n/=""] ++ n
        need = allWhere $ map itemWhere xs

        importer = unlines $ ("module Main where") :
                             ["import " ++ modname n ++ "()" | n <- need] ++
                             ["main = putStrLn \"Successfully checked\""]

        text n = unlines $ prefix :
                           ("module " ++ modname n ++ " where") :
                           render items
            where items = filter (matchWhere n . itemWhere) xs


render = collectImports . f [] . zip [1..]
    where
        f seen [] = []
        f seen ((n,HsItem Stmt pos x _) : xs) =
                linePragma pos : defNote : lines x2 ++ "" : f seen2 xs
            where
                def = defines x
                defNote = "-- !defines " ++ show def
                bad = def `intersect` seen
                x2 = rename (zip bad $ prime n bad) x
                seen2 = def `union` seen

        f seen (x:xs) = error $ "Stage 4, todo: " ++ show x


collectImports xs = filter ("import " `isPrefixOf`) xs ++ map f xs
    where
        f x | any (`isPrefixOf` x) ["import ","module "] = "-- HIDE " ++ x
            | otherwise = x


capital (x:xs) = toUpper x : xs


prime :: Int -> [String] -> [String]
prime n xs | length pos == length (nub pos) = pos
           | otherwise = map (++ end) xs
    where
        end = "''" ++ show n
        pos = map ((++ end) . reverse1 . drop1 (length end) . reverse1) xs
        drop1 n (x:xs) = x : drop n xs
        reverse1 (x:xs) = x : reverse xs

