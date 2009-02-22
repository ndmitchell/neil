
module Paper.Todo(todo) where

import Control.Monad
import Data.Char
import Paper.Util.Error


todo :: [FilePath] -> IO ()
todo files = do
    errs <- liftM concat $ mapM readTodos files
    sequence errs
    when (not $ null errs) $
        error $ "Error: " ++ show (length errs) ++ " todo commands found"
    putStrLn "No todo commands found"


readTodos :: FilePath -> IO [IO ()]
readTodos file = liftM (f 1) $ readFile file
    where
        f n ('\n':xs) = f (n+1) xs
        f n ('\\':t:'o':'d':'o':x:xs)
                | toLower t == 't' && not (isAlpha x) && x /= '}'
                = errorMsg file n "\\todo" msg : f n xs
            where msg = g $ dropWhile isSpace (x:xs)
        f n (x:xs) = f n xs
        f n [] = []

        g ('{':xs) = "{" ++ takeWhile (`notElem` "\n}") xs ++ "}"
        g _ = ""

