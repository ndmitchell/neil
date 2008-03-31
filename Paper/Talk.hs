
module Paper.Talk(talk) where

import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import System.FilePath
import System.Cmd
import System.Directory


talk :: FilePath -> [FilePath] -> [String] -> IO ()
talk obj files flags = do
    ss <- mapM (liftM text . readFile) files
    let out = obj </> "talk.txt"
    writeFile out $ flatten $ concat $ intersperse "\n\n" ss
    when ("" `notElem` flags) $ do
        let prog = head (flags ++ ["ptts"])
        r <- findExecutable prog
        case r of
            Nothing -> putStrLn $ "Can't find \"" ++ prog ++ "\" program, please install"
            Just y -> speak prog out (obj </> "talk.wav")


speak "ptts" input output = do
    system $ "ptts -voice \"Microsoft Mary\" -w \"" ++ output ++ "\" < \"" ++ input ++ "\""
    return ()

speak prog input output = putStrLn $ "Don't know how to speak with " ++ prog


-- compress \n\n\n -> \n
--          "  " -> " "
--          " \n" -> "\n"
flatten = f . g
    where
        g (' ':'\n':xs) = g ('\n':xs)
        g ('\n':' ':xs) = g ('\n':xs)
        g (' ':' ':xs) = g (' ':xs)
        g (x:xs) = x : g xs
        g [] = []

        f ('\n':'\n':'\n':xs) = f ('\n':'\n':xs)
        f (x:xs) = x : f xs
        f [] = []


text :: String -> String

text ('%':xs) = text $ dropWhile (/= '\n') xs
text ('\\':'%':xs) = '%' : text xs

text (x:xs) | x `elem` "|$" = case lex xs of
    [(s,y:xs)] | x == y -> s ++ text xs
    _ -> "expression" ++ text (drop 1 $ dropWhile (/= x) xs)
text ('\\':'$':xs) = '$' : text xs


-- Given an environment there are several behaviours:
-- replace the entire environment with some alternative text (figure, code)
-- drop the begin/end bits
text ('\\':xs) | "begin" `isPrefixOf` xs =
    case lookup a rep of
        Nothing -> text b
        Just y -> y ++ "\n\n" ++ text (skipTilEnd a b)
    where
        (a,b) = spanCurly $ drop 5 xs
        rep = [("figure",""),("table",""),("comment","")
              ,("tabular","Table."),("verbatim","Code."),("code","Code.")]

text ('\\':xs) 
        -- lists
        | a == "item" && "[" `isPrefixOf` b =
            let (c,d) = break (== ']') (tail b) in
            "\n" ++ text c ++ [':' | not $ ":" `isSuffixOf` c] ++ text (dropWhile (== ']') d)
        | a == "item" = "\nBullet: " ++ text b

        -- accents
        | a == "o" = 'o' : text b
        | null a && "\"" `isPrefixOf` b = text (tail b)

        -- special
        | a == "ref" = "1" ++ text (skipCurly b)
        | null a && "[" `isPrefixOf` b = "Code." ++ text (skipTil "\\]" b)
        | a == "url" = "url" ++ text (skipCurly b)
        | a == "citet" = "paper" ++ text (skipCurly b)

        | otherwise = text $ skippy (fromMaybe "" $ lookup a skip) b
    where
        (a,b) = span isAlpha xs

text (x:xs) | x `elem` "{}" =  text xs
text (x:xs) = x : text xs
text [] = []



skip = let (*) = (,) in
    map (* "[{")
        ["end","label","cite","vspace","hspace","footnote","documentclass"
        ,"include","title","author","institute","caption","usepackage"
        ,"copyrightyear","copyrightdata","titlebanner","title","subtitle"
        ,"bibliographystyle"]
    ++ ["newenvironment" * "{{{", "newcommand" * "{[{", "conferenceinfo" * "{{"
       ,"authorinfo" * "{{{","category" * "{{{", "terms" * "\n\n", "keywords" * "\n\n"]


skippy ('{':xs) t = skippy xs $ skipCurly t
skippy ('[':xs) t = skippy xs $ skipSquare t
skippy ('\n':xs) t = skippy xs $ drop 1 $ dropWhile (/= '\n') t
skippy [] t = t



-- "{foo}test" = ("foo","test")
-- "test = ("","test")
spanCurly = spanPair '{' '}'
skipCurly = snd . spanCurly

skipSquare = snd . spanPair '[' ']'


spanPair start stop xs | [start] `isPrefixOf` rest = f 1 $ drop 1 rest
    where
        rest = dropWhile isSpace xs
        f 1 (x:xs) | x == stop  = ([],xs)
        f n (x:xs) | x == stop  = g (n-1)
                   | x == start = g (n+1)
                   | otherwise  = g n
            where g n = let (a,b) = f n xs in (x:a,b)
        f n [] = ([],[])
spanPair start stop xs = ([],xs)



skipTilEnd s = skipTil ("\\end{" ++ s ++ "}")

skipTil s xs | s `isPrefixOf` xs = drop (length s) xs
skipTil s (x:xs) = skipTil s xs
skipTil s [] = []
