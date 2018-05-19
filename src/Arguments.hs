{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module Arguments where

import System.Console.CmdArgs

data Arguments
    -- darcs stuff
    = Whatsnew {repo :: FilePath, local :: Bool, look_for_adds :: Bool, ssh :: Bool}
    | Pull {repo :: FilePath}
    | Push {repo :: FilePath, ssh :: Bool}
    | Send {repo :: FilePath, patch :: FilePath}
    | Apply {patch :: FilePath}
    | Tag
    | Docs {username :: String, host :: String}

    | Travis {wait :: Double}
    | Releases {wait :: Double}
 
    -- cabal stuff
    | Sdist {github_user :: String, commit :: String}
    | Check {path :: FilePath, github_user :: String, commit :: String}
    | Binary {path :: FilePath}
    | Test {install :: Bool, no_warnings :: Bool, github_user :: String, commit :: String}
      deriving (Data,Typeable,Show)

arguments = cmdArgsMode $ modes
    [Whatsnew {repo = "." &= typDir &= help "Repo to use"
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
    ,Sdist {commit="None", github_user="None"}
          &= help "Create a cabal sdist with extra checks"
    ,Tag {} &= help "Tag the repo, after a release"
    ,Check {path = "." &= args &= typDir, commit="None", github_user="None"}
    ,Binary{}
    ,Docs {username = "NeilMitchell", host = "https://hackage.haskell.org"}
    ,Travis {wait = 0.1 &= help "Time to wait after each wget request"}
    ,Releases {wait = 0.1 &= help "Time to wait after each wget request"} &= help "List releases for a project"
    ,Test {install = False &= help "Install after building", no_warnings = False, commit="None", github_user="None"}
    ]
    &= summary "Neil's utility tool"
