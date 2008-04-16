
module Paper.Ref(ref) where

import Control.Monad
import Data.Char
import Data.List
import qualified Data.Map as Map


data Ref = Ref FilePath Int Bool -- True = ref, False = label
type Refs = Map.Map String [Ref]

addRef m (s,ref) = Map.insertWith (++) s [ref] m


ref :: [FilePath] -> IO ()
ref files = do
    r <- mapM readRefs files
    let errs = checkRefs $ foldl addRef Map.empty $ concat r
    putStr $ unlines errs
    when (not $ null errs) $
        error $ "Error: " ++ show (length errs) ++ " references failed"
    putStrLn "All references are correct"


readRefs :: FilePath -> IO [(String,Ref)]
readRefs file = liftM (f 1) $ readFile file
    where
        f n ('\n':xs) = f (n+1) xs
        f n ('\\':'r':'e':'f':xs) = g n True xs
        f n ('\\':'l':'a':'b':'e':'l':xs) = g n False xs
        f n (x:xs) = f n xs
        f n [] = []

        g n b ('{':xs) | "}" `isPrefixOf` post = (pre, Ref file n b) : f n (tail post) 
            where (pre,post) = span (`notElem` "\n}") xs
        g n b (x:xs) | isSpace x || x == '{' =
            error $ file ++ "(" ++ show n ++ ") Unrecognised label: " ++ take 25 xs
        g n b xs = f n xs


checkRefs :: Refs -> [String]
checkRefs = concatMap f . Map.toList
    where
        f (s,xs) | null b = err "used but not defined" a
                 | length b > 1 = err "defined mulitple times" b
                 | otherwise = []
            where
                (a,b) = partition (\(Ref a b c) -> c) xs
                err msg cs = ["Ref " ++ s ++ ", " ++ msg ++ ": " ++ unwords (map g cs)]

        g (Ref a b c) = a ++ "(" ++ show b ++ ")"
