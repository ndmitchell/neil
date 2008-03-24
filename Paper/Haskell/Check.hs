
module Paper.Haskell.Check where

import Paper.Haskell.Fragment
import System.Cmd
import System.Exit
import System.IO
import Data.List
import Data.Char
import Control.Monad
import Debug.Trace


data Result = Pass | Fail String | Missing String | Instances [String]
              deriving (Eq,Show)

builtin = ["where"] ++
          ["Int","Bool","Float","Integer","String","Char"]


checkFragments :: Bool -> (Int -> Bool) -> String -> [Frag] -> IO ()
checkFragments debug test prefix xs = mapM_ f xs
    where
        names = [drop 5 x | x <- lines prefix, "-- # " `isPrefixOf` x]
        insts = [drop 12 x | x <- lines prefix, "-- instance " `isPrefixOf` x]
    
        f (Expr i s)
            | test i && s `elem` (names ++ builtin ++ concat [has | Stmt _ has _ <- xs])
            = putStrLn $ "Checking line " ++ show i ++ "... success"

        f (Expr i s) = f $ Stmt i [] ("tex2hs _ = (" ++ s ++ ")")
        f (Stmt i has s) | test i = do
            putStr $ "Checking line " ++ show i ++ "... "
            res <- check (prefix ++ "\n" ++ s)
            case res of
                Pass -> putStrLn "success"
                Fail msg -> do
                    putStrLn "FAILURE"
                    putStr $ unlines $ map ("  "++) $ lines msg
                    error "Fix your code, or we'll reject you!"

        f _ = return ()

        check s = do
            res <- checkCode debug s
            case res of
                Missing x -> g (Fail $ "Can't find: " ++ show x) s [t | Stmt _ has t <- xs, x `elem` has]
                Instances x ->
                    let add = unlines ["instance " ++ i | i <- x, i `elem` insts]
                    in g (Fail $ "No instance: " ++ show x) s [add | not $ null add]
                _ -> return res
        
        g err s [] = return err
        g err s (x:xs) = do
            r <- check (s ++ "\n" ++ x)
            if r == Pass then return Pass else g r s xs

checkCode :: Bool -> String -> IO Result
checkCode debug orig = do
    writeFile "temp.hs" orig
    res <- system "ffihugs -98 temp.hs 2> temp.txt"
    if res == ExitSuccess then return Pass else do
        x <- readFileStrict "temp.txt"
        let s = unlines $ filter (not . null) $ drop 1 $ lines x
            err = parseError s
        
        when debug $ do
            putStrLn orig
            putStrLn s
            print err
            getChar
            return ()

        return err


readFileStrict s = do
    h <- openFile s ReadMode
    s <- hGetContents h
    length s `seq` hClose h
    return s


parseError s =
    if any ("- Undefined" `isPrefixOf`) (tails s) then
        Missing $ takeWhile (/= '\"') $ drop 1 $ dropWhile (/= '\"') $ dropWhile (/= '-') s
    else if any ("- Instance" `isPrefixOf`) (tails s) then
        Instances $ insts s
    else if any ("requires extra context" `isPrefixOf`) (tails s) then
        Instances $ nub $ insts2 s
    else
        Fail s



insts = map (unwords . words) . lines . map rep . sel
    where
        rep x = if x == ',' then '\n'
                else if x `elem` "()" then ' '
                else x
    
        sel (' ':'o':'f':' ':xs) = g xs
        sel (x:xs) = sel xs
        
        g (' ':'r':xs) = []
        g (x:xs) = x : g xs


insts2 = map (unwords . words) . lines . map rep . reverse . takeWhile (/= ':') . reverse
    where
        rep x = if x == ',' then '\n'
                else if x `elem` "()" then ' '
                else x
