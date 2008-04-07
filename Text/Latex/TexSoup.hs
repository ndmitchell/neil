
module Text.Latex.TexSoup where


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

{-
Tricky points:
Need to deal with not lexing inside verbatim
And only lexing inside comments in \begin{code}

Need to maintain a parse stack, and push/pop from it
-}


data S = S {file :: FilePath, line :: Int, text :: String, stack :: [(Bracket,[Tex])]}

lineInc = do s <- get; put s{line = line s + 1}; return (line s+1)
strGet = liftM text get
strPut x = modify $ \s -> s{text = x}


-- Environ's are put together afterwards
-- Text's are joined up afterwards


parseTex :: FilePath -> [Tex]
parseTex = f 0
    where
        -- line number, stack
        f i 



-- bracket/tex is stored in reverse
parse :: FilePath -> Int -> [(Bracket,[Tex])] -> [Lex] -> [Tex]
parse file n xs ys = f xs ys
  where
    f xs (LToggle b:ys) | is xs b = shut xs ys
                        | otherwise = open b xs ys
    f xs (LShut b:ys) | is xs b = shut xs ys
                      | otherwise = error $ "Unexpected closing lump, more info available"             
    f xs (LOpen b:ys) = open b xs ys
    f xs (y:ys) = add y xs ys

    is ((b1,_):_) b2 = b1 == b2
    is _ _ = False

    shut ((b1,x):[]) ys = bracket b1 (reverse x) : f [] ys
    shut ((b1,x1):(b2,x2):xs) ys = f ((b2,bracket b1 (reverse x1):x2):xs) ys

    add (f x xs


    -- open a bracket
    add b xs ys = 
  

((b1,as):xs) (LToggle b2:ys) | b1 == b2 = add xs ys


---------------------------------------------------------------------
-- LEXING

data Bracket
    = BCurly
    | BSquare
    | BDollar
    | BDollarDollar
    | BSlashSquare
    | BBar

bracket BCurly = Curly
bracket BSquare = Square
bracket BDollar = MathExpr


data Lex
    = LText Char
    | LComment String
    | LCommand String
    | LLine Int
    | LNewline
    | LOpen Bracket
    | LShut Bracket
    | LToggle Bracket


lexeme :: State S (Maybe Lex)
lexeme = do
  let a & b = strPut b >> return (Just a)
  xs <- strGet 
  case xs of
    -- escapes first
    '|':'|':xs -> LText "|" & xs
    '\\':'%':xs -> LText "%" & xs
    '\\':'\\':xs -> LNewline & xs
    
    -- brackets
    '{':xs -> LOpen BCurly & xs
    '}':xs -> LShut BCurly & xs
    '[':xs -> LOpen BSquare & xs
    ']':xs -> LShut BSquare & xs
    '$':'$':xs -> LToggle BDollarDollar & xs
    '$':xs -> LToggle BDollar & xs
    '\\':'[':xs -> LOpen BSlashSquare & xs
    '\\':']':xs -> LShut BSlashSquare & xs
    '|':xs -> LToggle BBar & xs
    
    -- rest
    '%':xs -> LComment a & b
        where (a,b) = break (== '\n') xs
    '\\':xs -> LCommand a & b
        where (a,b) = span isAlpha xs
    '\n':xs -> do i <- incLine ; LLine i & xs
    x:xs -> LText x & xs
    [] -> return Nothing
