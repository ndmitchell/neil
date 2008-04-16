
module Paper.Util.Error(errorMsg) where

import System.FilePath


errorMsg :: FilePath -> Int -> String -> String -> IO ()
errorMsg file line short long = putStrLn $
    takeFileName file ++
    (if line == 0 then "" else "(" ++ show line ++ ")") ++
    " " ++ short ++
    (if null long then "" else ": " ++ take 25 long)
