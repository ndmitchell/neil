
module Paper.Util.Error(errorMsg, errorDie) where

import System.FilePath


errorMsg :: FilePath -> Int -> String -> String -> IO ()
errorMsg = errorWith putStrLn


errorDie :: FilePath -> Int -> String -> String -> a
errorDie = errorWith error


errorWith :: (String -> a) -> FilePath -> Int -> String -> String -> a
errorWith action file line short long = action $
    takeFileName file ++
    (if line == 0 then "" else "(" ++ show line ++ ")") ++
    " " ++ short ++
    (if null long then "" else ": " ++ take 25 long)
