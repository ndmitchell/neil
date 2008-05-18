
module Paper.Haskell2.All(haskell2) where

import System.FilePath

import Paper.Haskell2.Stage1
import Paper.Haskell2.Stage2
import Paper.Haskell2.Stage3
import Paper.Haskell2.Stage4


haskell2 :: FilePath -> [FilePath] -> IO ()
haskell2 obj files = mapM_ f files
    where
        f file = do
            src <- readFile file
            let res = stage4 $ stage3 $ stage2 $ stage1 file src
            writeFile (obj </> takeBaseName file <.> "hs") res
