
module Paper.Ref(ref) where

import Control.Monad
import Data.List
import qualified Data.Map as Map
import Text.Latex.TexSoup


data Ref = Ref !FilePath !Int !Bool -- True = ref, False = label
type Refs = Map.Map String [Ref]

addRef s ref m = ref `seq` Map.insertWith (++) s [ref] m


ref :: [FilePath] -> IO ()
ref files = do
    r <- liftM (Map.unionsWith (++)) $ mapM readRefs files
    let errs = checkRefs r
    putStr $ unlines errs
    when (not $ null errs) $
        error $ "Error: " ++ show (length errs) ++ " references failed"
    putStrLn "All references are correct"


readRefs :: FilePath -> IO Refs
readRefs file = liftM (f Map.empty 1 . universeCommands) $ parseTexFile file
    where
        f r n _ | r `seq` False = undefined
        f r n ((x, Curly [Text s]:_): xs)
            | x == "label" = g False
            | x == "ref"   = g True
            where g b = f (addRef s (Ref file n b) r) n xs
        f r n (x:xs) = f r n xs
        f r n [] = r


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
