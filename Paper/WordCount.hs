
module Paper.WordCount(wordCountNorm, wordCountMin) where

import Data.Char
import Data.List


-- Do not follow \include{} links
wordCount :: ([String] -> Int) -> FilePath -> IO Int
wordCount f file = return . f . dropComments . lines =<< readFile file


dropComments :: [String] -> [String]
dropComments (x:xs) | isBeginComment x = drop 1 $ dropWhile (not . isEndComment) xs
dropComments (x:xs) = f x : dropComments xs
    where
        f ('\\':'%':xs) = '\\':'%':f xs
        f ('%':xs) = []
        f (x:xs) = x : f xs
        f [] = []
dropComments [] = []

isBeginComment = isPrefixOf "\\begin{comment}"
isEndComment   = isPrefixOf "\\end{comment}"


wordCountNorm = wordCount wordsBlock
    

wordsBlock :: [String] -> Int
wordsBlock = sum . map f
    where
        f ('\\':xs) = f (dropWhile isAlpha xs)
        f (x:xs) | isAlpha x = 1 + f (dropWhile (not . isSpace) xs)
        f (x:xs) = f xs
        f [] = 0


wordCountMin = wordCount (wordsBlock . map dropDollar . dropEqn)

dropDollar ('\\':'$':xs) = '\\':'$':dropDollar xs
dropDollar ('$':xs) = dropDollar $ drop 1 $ dropWhile (/= '$') xs
dropDollar (x:xs) = x : dropDollar xs
dropDollar [] = []

dropEqn (x:xs) | "\\beqa" `isPrefixOf` x = dropEqn $ f (x:xs)
    where
        f (x:xs) | "\\eeqa" `isPrefixOf` x = drop 5 x : xs
        f ((x:xs):ys) = f (xs:ys)
        f ([]:ys) = f ys
        f [] = []
dropEqn ((x:xs):ys) = (x:a):b
    where (a:b) = dropEqn (xs:ys)
dropEqn ([]:ys) = [] : dropEqn ys
dropEqn [] = []
