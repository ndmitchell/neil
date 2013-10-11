
module Paper.Haskell.Haskell.Tweak(tweak) where

import Paper.Haskell.Haskell.Provides
import Data.List


tweak :: String -> String
tweak = dummy . unlines . filter (not . isModule) . lines
    where isModule x = "module " `isPrefixOf` x || "import " `isPrefixOf` x


dummy x = unlines (x : map f undef)
    where
        p = provides x
        undef = [x | ProvidesSig x <- p] \\ [x | ProvidesBody x <- p]
        f x = "(" ++ x ++ ") = undefined"
