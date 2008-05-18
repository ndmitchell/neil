
module Paper.Haskell2.All(haskell2) where

import Paper.Haskell2.Stage1
import Paper.Haskell2.Stage2


haskell2 :: FilePath -> [FilePath] -> IO ()
haskell2 obj files = mapM_ f files
    where
        f file = do
            src <- readFile file
            error $ show $ stage2 $ stage1 file src
