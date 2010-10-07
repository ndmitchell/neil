
module Main(main) where

import System.IO
import System.Console.CmdArgs
import Darcs
import Arguments


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- cmdArgsRun arguments
    case Darcs.run args of
        Nothing -> error $ "Don't know how to deal with argument: " ++ show args
        Just x -> x
