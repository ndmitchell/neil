
module Paper.Util.IO where

import System.IO

readFile' x = do
    h <- openFile x ReadMode
    s <- hGetContents h
    seq (length s) $ do
        hClose h
        return s
