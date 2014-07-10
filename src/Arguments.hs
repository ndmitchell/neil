{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Arguments where

import System.Console.CmdArgs

data Arguments
    -- darcs stuff
    = Whatsnew {repo :: FilePath, delete_locks :: Bool, local :: Bool, look_for_adds :: Bool, ssh :: Bool}
    | Pull {repo :: FilePath, delete_locks :: Bool}
    | Push {repo :: FilePath, ssh :: Bool}
    | Send {repo :: FilePath, patch :: FilePath}
    | Apply {patch :: FilePath}
    | Tag
 
    | Travis {wait :: Double}
 
    -- cabal stuff
    | Sdist
    | Check
    | Test {install :: Bool}
      deriving (Data,Typeable,Show)

arguments = cmdArgsMode $ modes
    [Whatsnew {repo = "." &= typDir &= help "Repo to use"
              ,delete_locks = False &= help "Delete lock files"
              ,local = False &= help "Only check for local changes, no network required"
              ,look_for_adds = False &= name "l" &= help "Look for files to add"
              ,ssh = False &= help "SSH connections only, for if the http is down"}
              &= help "See what has changed (local and remote changes)"
    ,Pull {}
          &= help "Pull from the default locations"
    ,Push {}
          &= help "Push to the calculated SSH location"
    ,Send {patch="patches.tar" &= typFile &= help "Location for patches"}
          &= help "Send patches as a tarball"
    ,Apply {}
           &= help "Apply a patch tarball"
    ,Sdist {}
          &= help "Create a cabal sdist with extra checks"
    ,Tag {} &= help "Tag the repo, after a release"
    ,Check
    ,Travis {wait = 0 &= help "Time to wait after each wget request"}
    ,Test {install = False &= help "Install after building"}
    ]
    &= summary "Neil's utility tool"
