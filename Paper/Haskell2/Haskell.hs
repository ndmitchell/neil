
module Paper.Haskell2.Haskell(isHaskellSymbol, defines, rename, fakeImplement) where

import Data.Char
import Data.List
import Data.Maybe


isHaskellSymbol :: Char -> Bool
isHaskellSymbol = flip elem "|+-*"


defines :: String -> [String]
defines = nub . filter validName . concatMap f . map lexer2 . classLeft . lines
    where
        f ("(":name:")":_) = [name]
        f (name:_) = [name]
        f _ = []


validName x = isAlpha (head x) && x `notElem` keyword
keyword = ["class","instance","where","data","type","import"]


flushLeft (x:xs) = not $ isSpace x
flushLeft [] = False


classLeft (x:xs) | "class" `isPrefixOf` x = x : a ++ classLeft xs
    where (a,b) = span (\x -> null x || isSpace (head x)) xs
classLeft ((x:_):xs) | isSpace x = classLeft xs
classLeft (x:xs) = x : classLeft xs
classLeft [] = []


rename :: [(String, String)] -> String -> String
rename [] = id
rename ren = concat . map f . lexer
    where f x = fromMaybe x $ lookup x ren


-- figure out which definitions have a type signature
-- but no associated body, and make one up
fakeImplement :: String -> String
fakeImplement xs = unlines $
    ["-- !typesigFunction " ++ show typ, "-- !implementsFunction " ++ show imp, xs] ++
    [operator x ++ " = undefined -- stub" | x <- typ \\ imp]
    where (typ,imp) = (typesigFunction xs, implementsFunction xs)


-- more a "this function is definately defined"
-- as conservative
typesigFunction :: String -> [String]
typesigFunction = nub . concatMap (f . lexer2) . filter flushLeft . lines
    where
        f ("(":x:")":"::":_) = [x]
        f (x:"::":_) = [x]
        f _ = []


-- more a "this function is possibly implemented"
-- as conservative
implementsFunction :: String -> [String]
implementsFunction = nub . concatMap (f . lexer2) . lines
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
