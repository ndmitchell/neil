
module Main(main) where

import Control.Monad
import Data.Maybe
import System.Console.CmdArgs
import System.IO

import Arguments
import Cabal
import Darcs


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- cmdArgsRun arguments
    fromMaybe (error $ "Don't know how to deal with argument: " ++ show args) $
        msum [Darcs.run args, Cabal.run args]
           
