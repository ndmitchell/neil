
module Paper.Util.CmdNumber(cmdNumber) where

import Data.Char
import Data.Ix
import Paper.Util.String


cmdNumber :: [String] -> Int -> Bool
cmdNumber args | null nums = \n -> True
               | otherwise = \n -> any (`inRange` n) nums
    where
        nums :: [(Int,Int)]
        nums = map f $ filter (isDigit . head) args

        f x = (read a
              ,case b of {[] -> read a; ".." -> maxBound; _ -> read $ drop 2 b})
            where (a,b) = breakStr ".." x
