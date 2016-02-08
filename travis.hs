-- If a travis.hs file is present in the repo, it will be compiled and executed
-- after doing the 'cabal test'.

import System.Time.Extra
import System.Process.Extra
import System.Directory

main :: IO ()
main = do
    print =<< getCurrentDirectory
    (time,_) <- duration $ system_ $ "neil +RTS -M100M -RTS --help"
    putStrLn $ "Running --help took " ++ showDuration time
