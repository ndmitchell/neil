{-# LANGUAGE ScopedTypeVariables #-}

module Neil(
    neil,
    module Util,
    module Control.Monad,
    module Data.List,
    module Data.Maybe,
    module Extra
    ) where

import Util
import Control.Monad
import Data.List
import Data.Maybe
import Extra
import System.Environment


neil :: IO () -> IO ()
neil act = do
    args <- getArgs
    if "--go" `elem` args then
        act
     else
        putStrLn "Script type checks successfully, pass --go to run it"
