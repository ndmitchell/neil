{-# LANGUAGE CPP #-}

module Main(main) where

import Control.Monad
import Data.Maybe
import System.Console.CmdArgs
import System.Environment
import System.IO

import Arguments
import Cabal
import Binary
import Git
#ifndef SMALL
import Travis
import Releases
#endif
import qualified Paper.Main as Paper


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
                ,Binary.run args
#ifndef SMALL
                ,Travis.run args
                ,Releases.run args
#endif
                ]
