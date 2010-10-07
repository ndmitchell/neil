{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Arguments where

import System.Console.CmdArgs

data Arguments
    = Whatsnew {repo :: FilePath, delete_locks :: Bool, local :: Bool}
    | Pull {repo :: FilePath, delete_locks :: Bool}
    | Push {repo :: FilePath}
    | Send {repo :: FilePath, patch :: FilePath}
    | Apply {patch :: FilePath}
    | Sdist
      deriving (Data,Typeable,Show)

arguments = cmdArgsMode $ modes
    [Whatsnew {repo = "." &= typDir &= help "Repo to use"
              ,delete_locks = False &= help "Delete lock files"
              ,local = False &= help "Only check for local changes, no network required"}
              &= help "See what has changed (local and remote changes)"
    ,Pull {}
          &= help "Pull from the default locations"
    ,Push {}
          &= help "Push to the calculated SSH location"
    ,Send {patch="patches.tar.gz" &= typFile &= help "Location for patches"}
          &= help "Send patches as a tarball"
    ,Apply {}
           &= help "Apply a patch tarball"
    ,Sdist {}
           &= help "Create a cabal sdist with extra checks"
    ]
    &= summary "Neil's utility tool"
