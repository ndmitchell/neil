
module Paper.Haskell2.Stage3(stage3) where

import Paper.Haskell2.Type


-- should be removing [T]Expr and [T]Variable
stage3 :: [HsItem] -> [HsItem]
stage3 = id
