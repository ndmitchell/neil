
module Paper.Haskell2.Haskell(defines, rename, fakeImplement) where

import Data.Char
import Data.List
import Data.Maybe


defines :: String -> [String]
defines = f . map lexer2 . lines
    where
        f ((x:"::":_):rest) = x : f rest
        f (("(":x:")":"::":_):rest) = x : f rest
        f (_:rest) = f rest
        f [] = []


rename :: [(String, String)] -> String -> String
rename [] = id
rename ren = concat . map f . lexer
    where f x = fromMaybe x $ lookup x ren


-- figure out which definitions have a type signature
-- but no associated body, and make one up
fakeImplement :: String -> String
fakeImplement xs = xs ++ "\n" ++ unlines (
    [operator x ++ " = undefined -- stub" | x <- def \\ imp] ++
    ["-- " ++ show def, "-- " ++ show imp])
    where (def,imp) = (definesFunction xs, implementsFunction xs)


-- more a "this function is definately defined"
-- as conservative
definesFunction :: String -> [String]
definesFunction = concatMap (f . lexer2) . filter (not . isSpace . head) . filter (not .null) . lines
    where
        f ("(":x:")":"::":_) = [x]
        f (x:"::":_) = [x]
        f _ = []


-- more a "this function is possibly implemented"
-- as conservative
implementsFunction :: String -> [String]
implementsFunction = concatMap (f . lexer2) . lines
    where
        f (x:"::":xs) = []
        f (x:xs) = [x]
        f [] = []


operator (x:xs) | isAlpha x = x:xs
operator x = "(" ++ x ++ ")"


-- concat . lexer == id
lexer :: String -> [String]
lexer [] = []
lexer xs@(x:_) | isSpace x = a : lexer b
    where (a,b) = span isSpace xs
lexer xs = case lex xs of
                [(a,b)] -> a : lexer b


lexer2 = filter (not . isSpace . head) . lexer
