
module Paper.Haskell2.Stage3(stage3) where

import Data.Char
import Data.List
import Data.Maybe
import System.FilePath
import Paper.Haskell2.Type
import Paper.Haskell2.Haskell


exts = "NoMonomorphismRestriction, MultiParamTypeClasses, FlexibleContexts, " ++
       "FlexibleInstances, UndecidableInstances, Rank2Types, EmptyDataDecls, " ++
       "FunctionalDependencies, TypeSynonymInstances, IncoherentInstances, " ++
       "ExtendedDefaultRules"

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


render = collectImports . f [] . zip [0..]
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
           | otherwise = def
    where
        end = "''" ++ (uniqueStr !! n)
        sym = uniqueSym !! n
        (pos,def) = unzip $ map f xs

        f (x:xs) | isHaskellSymbol x = (x:xs ++ "++" ++ sym, x:xs ++ "++" ++ sym)
                 | otherwise = (x : (reverse $ drop (length end) $ reverse xs) ++ end, x:xs ++ end)



uniqueStr = map (:[]) one ++ two ++ error "Stage3, uniqueStr exhausted"
    where
        one = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
        two = [[a,b] | a <- one, b <- one]


uniqueSym = [[a,b] | a <- syms, b <- syms] ++ error "Stage3, uniqueSym exhausted"
    where syms = "!+-<>=*&^%$"
