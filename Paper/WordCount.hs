
module Paper.WordCount(wordCount) where

import Data.Char
import Data.List


-- Do not follow \include{} links
wordCount :: FilePath -> IO Int
wordCount file = return . sum . map countLine . dropComments . lines =<< readFile file

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

countLine x = f x
    where
        f ('\\':xs) = f (dropWhile isAlpha xs)
        f (x:xs) | isAlpha x = 1 + f (dropWhile (not . isSpace) xs)
        f (x:xs) = f xs
        f [] = 0
