
module Paper.Haskell2.Stage3(stage3) where

import Data.Char
import Data.List
import Paper.Util.String
import Paper.Haskell2.Type
import Paper.Haskell2.Haskell


-- should be removing [T]Expr and [T]Variable
stage3 :: [HsItem] -> [HsItem]
stage3 = id

