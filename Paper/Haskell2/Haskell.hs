
module Paper.Haskell2.Haskell(
    isHaskellSymbol, haskellKeywords, lexer,
    defines, rename, fakeImplement
    ) where

import Data.Char
import Data.List
import Data.Maybe
import Paper.Util.String


isHaskellSymbol :: Char -> Bool
isHaskellSymbol = flip elem "|+-*<>"

haskellKeywords = ["class","instance","where","data","type","import","in","let","do","module","import"]
haskellKeySymbols = ["--"]

defines :: String -> [String]
defines = nub . filter validName . concatMap f . map lexer . classLeft . lines
    where
        f ("(":name:")":_) | all isHaskellSymbol name = [name]
        f ("(":xs) | all isHaskellSymbol name = [name]
            where name:_ = drop 1 $ dropWhile (/= ")") xs
        f (_:name:_) | name /= "=" && all isHaskellSymbol name = [name]
        f (name:_) = [name]
        f _ = []


validName x = (all isHaskellSymbol x && x `notElem` haskellKeySymbols) ||
              (isAlpha (head x) && x `notElem` haskellKeywords)


flushLeft (x:xs) = not $ isSpace x
flushLeft [] = False


classLeft (x:xs) | "class" `isPrefixOf` x = x : a ++ classLeft xs
    where (a,b) = span (\x -> null x || isSpace (head x)) xs
classLeft ((x:_):xs) | isSpace x = classLeft xs
classLeft (x:xs) = x : classLeft xs
classLeft [] = []


rename :: [(String, String)] -> String -> String
rename [] = id
rename ren = concatMap f . lexerSpace
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
typesigFunction = nub . concatMap (typesigs . lexer) . filter flushLeft . lines


typesigs :: [String] -> [String]
typesigs = f []
    where
        f seen ("(":x:")":xs) = f seen (x:xs)
        f seen (x:"::":xs) = x:seen
        f seen (x:",":xs) = f (x:seen) xs
        f _ _ = []


-- more a "this function is possibly implemented"
-- as conservative
implementsFunction :: String -> [String]
implementsFunction = nub . concatMap (f . lexer) . lines
    where
        f xs | not $ null $ typesigs xs = []
        f (_:"`":x:"`":_) = [x]
        f (_:x:_) | all isHaskellSymbol x = [x]
        f ("(":xs) | all isHaskellSymbol b = [b]
            where b:_ = drop 1 $ dropWhile (/= ")") xs
        f (x:xs) = [x]
        f [] = []


operator (x:xs) | isAlpha x = x:xs
operator x = "(" ++ x ++ ")"


-- concat . lexerSpace == id (nearly, just comments)
lexerSpace :: String -> [String]
lexerSpace [] = []
lexerSpace xs@(x:_) | isSpace x = a : lexerSpace b
    where (a,b) = span isSpace xs
lexerSpace ('{':'-':'\"':xs) = (" {-\"" ++ a ++ c) : lexerSpace d
    where (a,b) = breakStr "\"-}" xs
          (c,d) = splitAt 3 b
lexerSpace ('-':'-':x:xs) | isAlphaNum x || isSpace x = (" --" ++ a) : lexerSpace b
    where (a,b) = break (== '\n') (x:xs)
lexerSpace xs = case lex xs of
                [(a,'.':x:xs)] | isUpper x -> (a++'.':b) : c
                    where b:c = lexerSpace (x:xs)
                [(a,b)] -> a : lexerSpace b
                other -> error $ "lexerSpace, unexpected: " ++ show (xs, other)

lexer = filter (not . isSpace . head) . lexerSpace
