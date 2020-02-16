
module Paper.Util.String where

import Data.List.Extra


dropUntilStr s xs | s `isPrefixOf` xs = drop (length s) xs
dropUntilStr s (x:xs) = dropUntilStr s xs
dropUntilStr s [] = []



breakStr :: String -> String -> (String,String)
breakStr s xs | s `isPrefixOf` xs = ("",xs)
breakStr s (x:xs) = (x:a,b)
    where (a,b) = breakStr s xs
breakStr s [] = ([],[])



splitStr s [] = []
splitStr s xs = a : case splitStr s (drop (length s) b) of
                         [] -> []
                         x:xs -> (s++x):xs
    where (a,b) = breakStr s xs
