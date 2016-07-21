{-# LANGUAGE CPP #-}

module Main(main) where

import Control.Monad
import Data.Maybe
import System.Console.CmdArgs
import System.Environment
import System.IO

import Arguments
import Cabal
import Git
#ifndef SMALL
import Travis
#endif
import qualified Paper.Main as Paper

foo = error __GLASGOW_HASKELL__


main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    a <- getArgs
    case a of
        "paper":a -> withArgs a Paper.main
        _ -> do
            args <- cmdArgsRun arguments
            fromMaybe (error $ "Don't know how to deal with argument: " ++ show args) $ msum
                [Git.run args
                ,Cabal.run args
#ifndef SMALL
                ,Travis.run args
#endif
                ]
