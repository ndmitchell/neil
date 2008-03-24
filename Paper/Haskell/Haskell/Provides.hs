
module Paper.Haskell.Haskell.Provides(Provides(..), provides, fromProvides, isStmt, lexemes) where

import Data.List
import Data.Char


data Provides = ProvidesBody String
              | ProvidesSig  String
              | ProvidesName String
                deriving (Eq,Show)


fromProvides (ProvidesBody x) = x
fromProvides (ProvidesSig  x) = x
fromProvides (ProvidesName x) = x


provides :: String -> [Provides]
provides x = nub $ concatMap (provider . lexemes) $ indents $ lines x


indents (x:(y:ys):zs) | isSpace y = indents $ (x++" ; "++y:ys) : zs
indents (x:xs) = x : indents xs
indents [] = []


isStmt x = "=" `elem` before "let" xs || "::" `elem` xs
    where xs = lexemes x

before x xs = if x `elem` xs then takeWhile (/= x) xs else xs


split on xs = a : if null b then [] else split on (tail b)
    where (a,b) = break (== on) xs


provider :: [String] -> [Provides]
provider xs = case xs of
    "type":xs -> [ProvidesName $ headNote 1 con]
    "data":xs -> ProvidesName (headNote 2 con) : providerCtors (map (rep "=" "|") (tail con))
    "class":xs -> ProvidesName (headNote 3 con) : map asClass (concatMap provider (tail $ split ";" con))
    _ | "::" `elem` (takeWhile (`notElem` ["where","="]) xs) -> providesSig xs
    _ -> providesBody xs
    where
        con = dropContext $ tail xs
        asClass = ProvidesName . fromProvides

        dropContext xs = if null b then a else tail b
            where (a,b) = break (== "=>") xs



providerCtors = map f . tail . split "|"
    where
        f xs = ProvidesName $ headNote 4 $ y ++ xs
            where y = filter (\x -> ":" `isPrefixOf` x && x /= "::") xs


providesSig = map ProvidesSig . filter (`notElem` [",","(",")"]) . takeWhile (/= "::") . takeWhile (/= "=")


-- approximation:
-- (symbol)
-- (...) symbol
-- ... symbol
-- name
providesBody [] = []
providesBody xs = [ProvidesBody $ f xs]
    where
        f (x:ys:xs) | isSymbol ys = getSymbol (ys:xs)
        f ("(":xs) = dropWhile (/= ")") xs !! 1
        f (x:xs) = x

        isSymbol (y:ys) = not (isAlpha y) && y `notElem` "([" && (y:ys) /= "="

        getSymbol ("`":x:xs) = x
        getSymbol (x:xs) = x


lexemes :: String -> [String]
lexemes x = case lex x of
    [("",_)] -> []
    [(x,y)] -> x : lexemes y
    [] -> []

rep from to x = if x == from then to else x


headNote i [] = error $ show i
headNote i (x:xs) = x
