
module Paper.Haskell2.All(haskell2) where

import Control.Monad
import System.Cmd
import System.Exit
import System.FilePath

import Paper.Haskell2.Stage1
import Paper.Haskell2.Stage2
import Paper.Haskell2.Stage3


haskell2 :: FilePath -> [FilePath] -> IO ()
haskell2 obj files = mapM_ f files
    where
        f file = do
            putStrLn $ "Checking " ++ takeBaseName file
            src <- readFile file
            let dest = obj </> takeFileName file
                res = stage3 dest $ stage2 $ stage1 file src
            mapM_ (uncurry writeFile) res
            res <- system $ "runhaskell -i" ++ obj ++ " " ++ fst (head res)
            when (res /= ExitSuccess) $ error "Failed to check"
