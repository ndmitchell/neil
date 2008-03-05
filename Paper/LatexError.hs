
module Paper.LatexError(latexError) where

import Control.Monad
import Data.Char
import System.Directory


-- given a list of all the files that went into the mix
-- and the log file
-- and the errors reported to the screen
-- attempt to diagnose something
latexError :: [FilePath] -> FilePath -> String -> IO ()
latexError texFiles logFile messages = mapM_ f $ lines messages
    where
        rep from to x = if x == from then to else x

        f s = putStrLn s >> g (lines $ s1 ++ map (rep ':' '\n') s2)
            where (s1,s2) = splitAt 3 s

        g (file:pos:text) | not (null pos) && all isDigit pos = showSection file (read pos)
        g _ = return ()


showSection :: FilePath -> Int -> IO ()
showSection file pos = do
        b <- doesFileExist file
        when b $ do
            src <- readFile file
            putStrLn $ unlines $ map f $ take 7 $ drop (pos - 3) $ zip [1..] $ lines src
    where f (p,s) = show p ++ " : " ++ s


