
module Paper.Haskell2.All(haskell2) where

import System.FilePath

import Paper.Haskell2.Stage1
import Paper.Haskell2.Stage2
import Paper.Haskell2.Stage3


haskell2 :: FilePath -> [FilePath] -> IO ()
haskell2 obj files = mapM_ f files
    where
        f file = do
            src <- readFile file
            let dest = obj </> takeFileName file
                res = stage3 dest $ stage2 $ stage1 file src
            mapM_ (uncurry writeFile) res
