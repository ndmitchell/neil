
module Text.Latex.TexSoup(Tex(..), parseTex, parseTexFile) where

{-
Tricky points:
Need to deal with not lexing inside verbatim
And only lexing inside comments in \begin{code}
-}

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe


---------------------------------------------------------------------
-- RESULT DATA TYPE AND DRIVER

data Tex
    = Command String           -- \foo
    | Curly [Tex]              -- {foo}
    | Square [Tex]             -- [foo]
    | MathExpr [Tex]           -- $foo$
    | MathBlock [Tex]          -- \[foo\]   OR   $$foo$$
    | CodeExpr [Tex]           -- |foo|
    | Comment String           -- %foo
    | Environ String [Tex]     -- \begin{foo}bar\end{foo}
    | Text String              -- foo
    | Newline                  -- \\
    | Line Int                 -- a line number
      deriving Show


parseTexFile :: FilePath -> IO [Tex]
parseTexFile = liftM parseTex . readFile


parseTex :: String -> [Tex]
parseTex = joinEnviron . joinText . run tex


joinEnviron :: [Tex] -> [Tex]
joinEnviron = transformTexs f2
    where
        f2 xs = a ++ (if b == Nothing then [] else error $ "Unexpected end: " ++ show b)
            where (a,b,c) = f xs

        -- return (inner,end,rest)
        f :: [Tex] -> ([Tex],Maybe String,[Tex])
        f (Command "begin":xs) = (Environ text inner : a ++ err, b, c)
            where
                err = if Just text == end then [] else
                      error $ "Environment started with " ++ text ++ " but ended with " ++ show end
                (text,rest) = g xs
                (inner,end,post) = f rest
                (a,b,c) = f post

        f (Command "end":xs) = ([], Just text, rest)
            where (text,rest) = g xs

        f (x:xs) = (x:a,b,c)
            where (a,b,c) = f xs
        f [] = ([],Nothing,[])

        g (Curly [Text s]:xs) = (s,xs)
        g xs = error $ "\\begin or \\end not properly ended, " ++ take 25 (show xs)



joinText = transformTexs f
    where
        f (Text s:xs) = Text (s++a) : f b
            where (a,b) = g xs
        f (x:xs) = x : f xs
        f [] = []

        g (Text s:xs) = (s ++ a, b)
            where (a,b) = g xs
        g xs = ([], xs)


transformTexs :: ([Tex] -> [Tex]) -> [Tex] -> [Tex]
transformTexs f xs = f $ map g xs
    where
        g x = case x of
            Curly xs -> fs Curly xs
            Square xs -> fs Square xs
            MathExpr xs -> fs MathExpr xs
            CodeExpr xs -> fs CodeExpr xs
            Environ s xs -> fs (Environ s) xs
            x -> x

        fs c xs = c $ transformTexs f xs


---------------------------------------------------------------------
-- COMBINATOR DATA TYPES AND OPERATIONS

-- text and line number
type State = (String,Int)

type Parser = State -> Maybe (State, [Tex])
type Combiner = State -> [Tex] -> (State, [Tex])


parserOne :: ([Tex] -> Tex) -> String -> String -> Parser -> Parser
parserOne join start stop inner (s,n0)
    | not $ start `isPrefixOf` s = Nothing
    | otherwise = Just (a, [join b])
    where
        (a,b) = f (drops start s, n0)

        -- note: could be made lazy, but isn't
        f (s,n) | stop `isPrefixOf` s = ((drops stop s,n), [])
                | null s = error $ "Command started on line " ++ show n0 ++ " not finished, " ++
                                   show start ++ " -> " ++ show stop
                | otherwise = (s3, r1++r2)
            where
                Just (s2,r1) = inner (s,n)
                (s3,r2) = f s2


parserStr :: Tex -> String -> Parser
parserStr res s1 (s2,n)
    | not $ s1 `isPrefixOf` s2 = Nothing
    | otherwise = Just ((drops s1 s2, n), [res])


(+=) :: Parser -> Parser -> Parser
(+=) f g x = case f x of
                 Nothing -> g x
                 Just y -> Just y


run :: Parser -> String -> [Tex]
run p s = Line 1 : f (s,1)
    where
        f ([],n) = []
        f s = r ++ f s2
            where Just (s2,r) = p s


drops s = drop (length s)

---------------------------------------------------------------------
-- COMBINATION PARSERS


tex = escape += line += curly += comment +=
      mathBlock += mathExpr += codeExpr +=
      code += verb += commentEnv +=
      command += text


---------------------------------------------------------------------
-- RAW PARSERS

text (c:cs, n) = Just ((cs,n), [Text [c]])
text _ = Nothing


line ('\n':cs,n) = Just ((cs,n+1), [Line (n+1)])
line _ = Nothing


command ('\\':cs,n) = Just (s, Command a : r)
    where
        (a,b) = span isAlpha cs
        b2 = dropWhile isSpace b
        (s,r) = fromMaybe ((b2,n),[]) $ square (b2,n)
command _ = Nothing


comment ('%':cs,n) = Just ((b,n), [Comment a])
    where (a,b) = break (== '\n') cs
comment _= Nothing

---------------------------------------------------------------------
-- HIGH LEVEL PARSERS

commentEnv = parserOne (Environ "comment") "\\begin{comment}" "\\end{comment}" text 

verb = parserOne (Environ "verbatim") "\\begin{verbatim}" "\\end{verbatim}" text

-- TODO: not quite correct, need an internal comment/tex parser
code = parserOne (Environ "code") "\\begin{code}" "\\end{code}" text

mathBlock = parserOne MathBlock "$$" "$$" tex +=
            parserOne MathBlock "\\[" "\\]" tex

curly = parserOne Curly "{" "}" tex
square = parserOne Square "[" "]" tex

mathExpr = parserOne MathExpr "$" "$" tex
codeExpr = parserOne CodeExpr "|" "|" text

newline = parserStr Newline "\\\\"

escape = parserStr (Text "|") "||" +=
         parserStr (Text "@") "@@" +=
         parserStr (Text "%") "\\%" +=
         parserStr (Text "\\") "\\\\"
