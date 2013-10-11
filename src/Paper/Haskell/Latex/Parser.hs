
module Paper.Haskell.Latex.Parser(Line, HsCode(..), parseLatex) where

import Data.List

type Line = Int

data HsCode = HsCode Line String
            | HsExpr Line String
            deriving (Eq,Show)



parseLatex :: String -> [HsCode]
parseLatex x = f $ zip [1..] (lines x)
    where
        f ((i,x):xs) | beginCode x = let (a,b) = break (endCode . snd) xs
                                         code = (unlines $ map snd a)
                                     in HsCode i code : f b
                     | ignoreCode x = f $ drop 1 $ dropWhile (not . endCode . snd) xs
                     | otherwise = concat (zipWith (g i) [1..] (parseLine x)) ++ f xs
        f [] = []

        g line col s = [HsExpr line s]


ignoreCode = isPrefixOf "\\ignore\\begin{code}"
beginCode = isPrefixOf "\\begin{code}"
endCode = isPrefixOf "\\end{code}"


parseLine :: String -> [String]
parseLine ('%':xs) = []
parseLine ('|':'|':xs) = parseLine xs
parseLine ('|':xs) = a : parseLine b
    where (a,b) = parseBar xs
parseLine xs | "\\ignore|" `isPrefixOf` xs = parseLine $ snd $ parseBar $ drop 8 xs
parseLine (x:xs) = parseLine xs
parseLine [] = []

parseBar ('|':'|':xs) = ('|':a,b)
    where (a,b) = parseBar xs
parseBar ('|':xs) = ("",xs)
parseBar (x:xs) = (x:a,b)
    where (a,b) = parseBar xs
parseBar [] = ("","")
