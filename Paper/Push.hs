
module Paper.Push(push) where

import Control.Monad
import Data.List
import Data.Maybe
import System.Cmd
import System.FilePath


push :: FilePath -> IO ()
push darcs = do
    src <- readFile $ darcs </> "_darcs" </> "prefs" </> "repos"
    () <- length src `seq` return ()
    let r = pick $ lines src
    when (isNothing r) $ error "No non-http repos in the prefs/repos file"

    putStrLn $ "Pushing to " ++ show (fromJust r) ++ "..."
    system $ "darcs push --no-set-default \"" ++ fromJust r ++ "\""
    return ()


-- pick the ssh address
pick :: [String] -> Maybe String
pick xs = listToMaybe $ ssh ++ [b ++ drop (length a) h | h <- http, (a,b) <- mapping, a `isPrefixOf` h]
    where (http,ssh) = partition ("http://" `isPrefixOf`) xs


mapping = [("http://www.cs.york.ac.uk/fp/darcs/","ndm@community.haskell.org:/home/ndm/darcs/")
          ,("http://community.haskell.org/~ndm/darcs/","ndm@community.haskell.org:/home/ndm/darcs/")
          ,("http://code.haskell.org/","ndm@code.haskell.org:/srv/code/")
          ]

