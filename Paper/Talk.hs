
module Paper.Talk(talk) where

import Control.Monad
import Data.List
import Data.Char


talk :: FilePath -> [FilePath] -> IO ()
talk obj files = do
    ss <- mapM (liftM text . readFile) files
    writeFile "obj/talk.txt" $ flatten $ concat $ intersperse "\n\n" ss


flatten ('\n':'\n':'\n':xs) = flatten ('\n':'\n':xs)
flatten (x:xs) = x : flatten xs
flatten [] = []


text :: String -> String

text ('%':xs) = text $ dropWhile (/= '\n') xs
text ('\\':'%':xs) = '%' : text xs

-- Given an environment there are several behaviours:
-- replace the entire environment with some alternative text (figure, code)
-- drop the begin/end bits
text ('\\':xs) | "begin" `isPrefixOf` xs =
    case lookup a rep of
        Nothing -> text b
        Just y -> y ++ "\n\n" ++ text (skipTilEnd a b)
    where
        (a,b) = spanBraces $ drop 5 xs
        rep = [("code","Code."), ("figure",""),("tabular","Table.")]

text ('\\':xs) 
        -- lists
        | a == "item" && "[" `isPrefixOf` b =
            let (c,d) = break (== ']') (tail b) in
            "\n" ++ text c ++ [':' | not $ ":" `isSuffixOf` c] ++ text (dropWhile (== ']') d)
        | a == "item" = "\nBullet: " ++ text b

        -- accents
        | a == "o" = 'o' : text b
        | null a && "\"" `isPrefixOf` b = text (tail b)

        | a `elem` skip = text (snd $ spanBraces b)
        | a == "ref" = "1" ++ text (snd $ spanBraces b)
        | null a && "[" `isPrefixOf` b = "Code." ++ text (skipTil "\\]" b)
        | otherwise = text b
    where
        (a,b) = span isAlpha xs
        skip = ["end","label","cite","vspace","hspace","footnote"]

text (x:xs) | x `elem` "|$" = case lex xs of
    [(s,y:xs)] | x == y -> s ++ text xs
    _ -> "expression" ++ text (drop 1 $ dropWhile (/= x) xs)

text (x:xs) | x `elem` "{}" =  text xs
text (x:xs) = x : text xs
text [] = []



-- "{foo}test" = ("foo","test")
-- "test = ("","test")
spanBraces ('{':xs) = (a, drop 1 b)
    where (a,b) = break (== '}') xs
spanBraces xs = ("", xs)


skipTilEnd s = skipTil ("\\end{" ++ s ++ "}")

skipTil s xs | s `isPrefixOf` xs = drop (length s) xs
skipTil s (x:xs) = skipTil s xs
skipTil s [] = []
