
module Paper.Talk(talk) where

import Control.Monad
import Data.List
import Data.Char
import Data.Maybe
import Paper.Util.CmdNumber
import Paper.Util.String
import System.FilePath
import System.Process
import System.Directory


talk :: FilePath -> [FilePath] -> [String] -> IO ()
talk objDir files args = do
    rPtts <- findExecutable "ptts"
    when (isNothing rPtts) $
        error $ "Can't find ptts executable, please install"

    rLame <- findExecutable "lame"
    when (isNothing rLame) $
        putStrLn "Warning: lame not found, .wav files will be generated"

    mapM_ (f (isJust rLame)) files
    where
        permit = cmdNumber args

        f lame file = do
            src <- readFileSpeach file
            mapM_ (g lame file) $ zip [0..] src

        g lame file (n,s) | not $ permit n = return ()
                          | otherwise = do
            let name ext = objDir </> (takeBaseName file ++ "_" ++ show n) <.> ext
            writeFile (name "txt") s
            putStrLn $ "Writing " ++ name "wav"
            system $ "ptts -voice \"Microsoft Mary\" -w \"" ++ name "wav" ++ "\" < \"" ++ name "txt" ++ "\""
            putStrLn $ "Writing " ++ name "mp3"
            system $ "lame -b 64 --quiet \"" ++ name "wav" ++ "\" \"" ++ name "mp3" ++ "\""
            removeFile (name "wav")


---------------------------------------------------------------------
-- TEXT PROCESSING BIT


-- read a file and split it into text chunks
readFileSpeach :: FilePath -> IO [String]
readFileSpeach file = do
    src <- readFile file
    return $ map (flatten . text) $ divide src


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


divide = splitStr "\\section"


text :: String -> String

text ('%':xs) = text $ dropWhile (/= '\n') xs
text ('\\':'%':xs) = '%' : text xs

text xs | "|(:)|" `isPrefixOf` xs = "cons" ++ text (drop 5 xs)
text xs | "|[]|" `isPrefixOf` xs = "nil" ++ text (drop 4 xs)
text (x:xs) | x `elem` "|$" = case lex xs of
    [(s,y:xs)] | x == y -> s ++ text xs
    _ -> "expression" ++ text (drop 1 $ dropWhile (/= x) xs)
text ('\\':'$':xs) = '$' : text xs

text ('`':'`':xs) = '\"' : text xs
text ('\'':'\'':xs) = '\"' : text xs
text ('`':xs) = '\'' : text xs

-- change some constructs to insert pauses
text (' ':'-':'-':xs) = " ; " ++ text (dropWhile (== '-') xs)
text ('(':xs) = " ; " ++ text xs
text (')':xs) = " ; " ++ text xs

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
        | null a && "[" `isPrefixOf` b = "Code." ++ text (dropUntilStr "\\]" b)
        | a == "url" = "url" ++ text (skipCurly b)
        | a == "citet" = "paper" ++ text (skipCurly b)
        | a == "S" = "section " ++ text b

        | otherwise = text $ skippy (fromMaybe "" $ lookup a skip) b
    where
        (a,b2) = span isAlpha xs
        b = if "*" `isPrefixOf` b2 then tail b2 else b2

text (x:xs) | x `elem` "{}" =  text xs
text (x:xs) = x : text xs
text [] = []



skip = let (*) = (,) in
    map (* "[{")
        ["end","label","cite","vspace","hspace","footnote","documentclass"
        ,"include","title","author","institute","caption","usepackage"
        ,"copyrightyear","copyrightdata","titlebanner","title","subtitle"
        ,"bibliographystyle","citep"]
    ++ ["newenvironment" * "{{{", "newcommand" * "{[{", "conferenceinfo" * "{{"
       ,"authorinfo" * "{{{","category" * "{{{", "terms" * "\n\n", "keywords" * "\n\n"
       ,"setcounter" * "{{"]


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



skipTilEnd s = dropUntilStr ("\\end{" ++ s ++ "}")
