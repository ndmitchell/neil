{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Main(main) where

import System.Console.CmdArgs
import System.IO
import Darcs


data Neil
    = Whatsnew {repo :: FilePath, deleteLocks :: Bool}
    | Pull {repo :: FilePath, deleteLocks :: Bool}
    | Push {repo :: FilePath}
    | Send {repo :: FilePath, patch :: FilePath}
    | Apply {patch :: FilePath}
      deriving (Data,Typeable)

mode = cmdArgsMode $ modes
    [Whatsnew {repo = "." &= typDir &= help "Repo to use"
              ,deleteLocks = False &= help "Delete lock files"}
              &= help "See what has changed (local and remote changes)"
    ,Pull{}
          &= help "Pull from the default locations"
    ,Push{}
          &= help "Push to the calculated SSH location"
    ,Send{patch="patches.tar.gz" &= typFile &= help "Location for patches"}
          &= help "Send patches as a tarball"
    ,Apply{}
           &= help "Apply a patch tarball"
    ]
    &= summary "Neil's utility tool"

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- cmdArgsRun mode
    case args of
        Whatsnew x y -> whatsnew x y
        Pull x y -> pull x y
        Push x -> push x
        Send x y -> send x y
        Apply x -> apply x
