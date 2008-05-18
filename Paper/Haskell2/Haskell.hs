
module Paper.Haskell2.Haskell(defines, wants, rename) where

import Data.Char
import Data.Maybe


defines :: String -> [String]
defines = f . map lexer2 . lines
    where
        f ((x:"::":_):rest) = x : f rest
        f (_:rest) = f rest
        f [] = []


wants :: String -> [String]
wants _ = []


rename :: [(String, String)] -> String -> String
rename [] = id
rename ren = concat . map f . lexer
    where f x = fromMaybe x $ lookup x ren


-- concat . lexer == id
lexer :: String -> [String]
lexer [] = []
lexer xs@(x:_) | isSpace x = a : lexer b
    where (a,b) = span isSpace xs
lexer xs = case lex xs of
                [(a,b)] -> a : lexer b


lexer2 = filter (not . isSpace . head) . lexer
