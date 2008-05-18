
module Paper.Haskell2.Stage1(stage1) where

import Data.Char
import Data.List
import Paper.Util.String
import Paper.Util.Error
import Paper.Haskell2.Type


stage1 :: FilePath -> String -> [HsLow]
stage1 file = f 1 ""
    where
        pos = Pos file

        f i cmd xs | "\\hsdef{" `isPrefixOf` xs = map (HsDef (pos i)) (lines a) ++ f (i + newlines a) "" b
            where (a,b) = break (== '}') $ drop 7 xs

        f i cmd xs | "\\begin{hsdef}" `isPrefixOf` xs = map (HsDef (pos i)) (lines a) ++ f (i + newlines a) "" b
            where (a,b) = breakStr "\\end{hsdef}" $ drop 13 xs

        f i cmd xs | "\\ignore" `isPrefixOf` xs = f i "ignore" $ drop 7 xs

        f i cmd xs | "\\hs{" `isPrefixOf` xs = f (i + newlines a) a $ drop 1 b
            where (a,b) = break (== '}') $ drop 4 xs

        f i cmd ('|':'|':xs) = f i "" xs
        f i cmd ('|':xs) | '\n' `elem` a = errorDie file i "Failed to parse | lines" a
                         | otherwise = hsCheck (pos i) Expr cmd a ++ f i "" b
            where (a,b) = spanExpr xs

        f i cmd xs | "\\begin{code}" `isPrefixOf` xs
                   = hsCheck (pos i) Stmt cmd a ++ f (i + newlines a) "" b
            where (a,b) = breakStr "\\end{code}" $ drop 12 xs

        f i cmd ('%':xs) = f i "" $ dropWhile (/= '\n') xs
        f i cmd (x:xs) | x == '\n' = f (i+1) cmd xs
                       | isSpace x = f i cmd xs
                       | otherwise = f i "" xs
        f i cmd [] = []


spanExpr :: String -> (String, String)
spanExpr ('|':'|':xs) = ('|':a, b)
    where (a,b) = spanExpr xs
spanExpr ('|':xs) = ("", xs)
spanExpr (x:xs) = (x:a,b)
    where (a,b) = spanExpr xs
spanExpr [] = ("","")


newlines :: String -> Int
newlines = length . filter (== '\n')


readCmd :: String -> ([String], String)
readCmd ('@':xs) =  (a:c,d)
    where
        (a,b) = break (== ' ') xs
        (c,d) = readCmd $ drop 1 b
readCmd xs = ([], xs)


hsCheck pos typ cmd x
        | b == "ignore" = []
        | b /= "" && b `notElem` known = error $ "Stage 1, todo: " ++ show b 
        | otherwise = [HsCheck pos typ2 (parseWhere a) x]
    where
        typ2 = if cmd == "stmt" then Stmt else typ
        (a,b) = readCmd cmd

known = ["stmt"]
