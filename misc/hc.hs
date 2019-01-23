
module Main(main) where

import System.Environment
import System.Process.Extra
import System.Time.Extra
import Control.Monad.Extra
import Data.List.Extra
import System.Directory
import System.FilePath


data Args = Setup | Checkout | Pull | Build | Ghcid | Run | Clean
    deriving (Show, Enum, Bounded)

args :: [(String, Args)]
args = [(lower $ show x, x) | x <- [minBound ..]]

-- data Foo where {Bar :: Foo; Baz :: Foo} deriving Show


main :: IO ()
main = do
    xs <- getArgs
    case xs of
        x:xs | Just x <- lookup (lower x) args -> do
            (t, ()) <- duration $ case x of
                Setup -> setup
                Checkout -> checkout
                Build -> build xs
                Pull -> pull
                Ghcid -> ghcid xs
                Run -> run xs
                Clean -> clean
            putStrLn $ "SUCCESS (" ++ showDuration t ++ ")"
        x:_ -> putStrLn $ "Unknown command " ++ x ++ ", expected one of: " ++ unwords (map fst args)
        [] -> putStrLn $ "Add a command, one of: " ++ unwords (map fst args)

checkGhcDir = do
    unlessM (doesFileExist ".git/HEAD") $ fail "Not in a git directory"
    pwd <- getCurrentDirectory
    unless (takeFileName pwd == "ghc") $ fail "Not in a ghc directory"

setup = do
    system_ "stack exec -- pacman -S autoconf automake-wrapper make patch python tar --noconfirm"

checkout = do
    system_ "git clone https://gitlab.haskell.org/ghc/ghc.git --recursive"
    putStrLn "\nYou need to change to the 'ghc' directory to continue"

pull = do
    checkGhcDir
    system_ "git pull"
    system_ "git submodule update"


build xs = do
    checkGhcDir
    system_ $ unwords $ "hadrian\\build.bat -j --flavour=quickest --configure" : xs

ghcid xs = do
    checkGhcDir
    system_ $ "ghcid -c \"" ++ unwords
        ("_build\\stage1\\bin\\ghc.exe" :
        "--interactive" :
        "-ghci-script ./utils/ghc-in-ghci/settings.ghci" :
        "-ghci-script ./utils/ghc-in-ghci/load-main.ghci" :
        "+RTS -A128m -RTS" :
        "-j" :
        xs) ++ "\""

run xs = do
    checkGhcDir
    system_ $ unwords $ "_build\\stage1\\bin\\ghc.exe" : xs

clean = do
    checkGhcDir
    system_ "hadrian\\build.bat clean"
