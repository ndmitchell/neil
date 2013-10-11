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
 
    -- cabal stuff
    | Sdist {official :: [String], partial :: [String], ignore_partial :: Bool, ignore_warnings :: Bool}
    | Check
    | Test
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
    ,Sdist {official = [] &= help "Officially supported GHC versions"
           ,partial = [] &= help "Partially supported GHC versions"
           ,ignore_partial = False &= help "Don't check on partially supported GHC's"
           ,ignore_warnings = False &= help "Ignore warnings"}
           &= help "Create a cabal sdist with extra checks"
    ,Check
    ,Test
    ]
    &= summary "Neil's utility tool"
