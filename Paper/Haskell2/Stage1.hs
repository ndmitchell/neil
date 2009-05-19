
module Paper.Haskell2.Stage1(stage1) where

import Data.Char
import Data.List
import Paper.Util.String
import Paper.Util.Error
import Paper.Haskell2.Type
import Paper.Haskell2.Haskell


defs = [("\\hsdef{\\begin{comment}","\\end{comment}}")
       ,("\\begin{hsdef}","\\end{hsdef}")
       ,("\\hsdef\\begin{comment}","\\end{comment}")
       ,("\\hsdef{","}")
       ]

stage1 :: FilePath -> String -> [HsLow]
stage1 file = f 1 ""
    where
        pos = Pos file

        f i cmd xs | match /= [] = map (HsDef (pos i)) (lines a) ++ f (i + newlines a) "" b
            where
                match = filter ((`isPrefixOf` xs) . fst) defs
                (start,end):_ = match
                (a,b) = breakStr end $ drop (length start) xs

        f i cmd xs | "\\ignore" `isPrefixOf` xs = f i "ignore" $ drop 7 xs

        f i cmd xs | "\\h{" `isPrefixOf` xs = f (i + newlines a) a $ drop 1 b
            where (a,b) = break (== '}') $ drop 3 xs

        f i cmd ('|':'|':xs) = f i "" xs
        f i cmd ('|':xs) | '\n' `elem` a = errorDie file i "Failed to parse | lines" a
                         | otherwise = hsCheck (pos i) Expr cmd a ++ f i "" b
            where (a,b) = spanExpr xs

        f i cmd xs | "\\begin{code}" `isPrefixOf` xs
                   = hsCheck (pos i) Stmt cmd a ++ concatMap (f i "") (comments a) ++ f (i + newlines a) "" b
            where (a,b) = breakStr "\\end{code}" $ drop 12 xs

        f i cmd xs | "%if 0" `isPrefixOf` xs = f (i + newlines a) "" b
            where (a,b) = breakStr "%endif" xs

        f i cmd ('%':xs) = f i "" $ dropWhile (/= '\n') xs
        f i cmd (x:xs) | x == '\n' = f (i+1) cmd xs
                       | isSpace x = f i cmd xs
                       | otherwise = f i "" xs
        f i cmd [] = []


comments :: String -> [String]
comments = concatMap f . lines
    where
        f xs = [drop 2 b | not $ null b]
            where (a,b) = breakStr "--" xs


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
readCmd ('.':xs) = (a:c,d)
    where
        (a,b) = break (== ' ') xs
        (c,d) = readCmd $ drop 1 b
readCmd xs = ([], xs)


hsCheck pos typ cmd x
        | b == "ignore" = []
        | b == "" = cont [(typ,x)]
        | otherwise = cont $ hsCustom b x
    where
        cont tx = [HsCheck pos t (parseWhere a) x | (t,x) <- tx]
        (a,b) = readCmd cmd


-- @ foo = bar
-- @     = def
-- implicitly insert foo before each =
hsCustom "deflist" x = [(Stmt,unlines $ map f ls)]
    where
        ls = lines x
        prefix = takeWhile (/= '=') $ head $ dropWhile (all isSpace) ls
        f x | "=" `isPrefixOf` dropWhile isSpace x = prefix ++ x
            | otherwise = x

-- each line started at column 1 is a new expr
hsCustom "exprlist" x = map ((,) Expr) $ f $ lines x
    where
        f (x:(y:ys):z) | isSpace y = f ((x ++ '\n':y:ys) : z)
        f (x:xs) = x : f xs
        f [] = []

-- a piece of context
hsCustom "ctxt" x = [(Stmt,"context_expression :: " ++ x ++ " => " ++ free ++ "()\ncontext_expression = undefined")]
    where free = concat [x:xs ++ " -> " | x:xs <- lexer x, isLower x]

-- a type expression
hsCustom "type" x = [(Stmt,"type TypeType " ++ free ++ " = " ++ x)]
    where free = unwords [x | x <- lexer x, isLower $ head x]

hsCustom "stmt" x = [(Stmt,x)]
hsCustom "expr" x = [(Expr,unlines $ map (' ':) $ lines x)]
hsCustom name _ = error $ "Stage1 todo: " ++ show name
