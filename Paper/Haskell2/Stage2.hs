
module Paper.Haskell2.Stage2(stage2) where

import Data.Char
import Data.List
import Paper.Util.String
import Paper.Haskell2.Type


stage2 :: [HsLow] -> [HsItem]
stage2 = concatMap f
    where
        f (HsDef pos x) | "instance" `isPrefixOf` x 
                        || "import" `isPrefixOf` x  = [HsItem Stmt pos x []]

        f (HsCheck pos expr cmd x) | cmd == "ignore" = []
                                   | expr = [HsItem Expr pos x []]
                                   | otherwise = [HsItem Stmt pos x []]

        f x = error $ "Stage2, todo: " ++ show x





{-

\hsDef{xs,ys} -- some variables, introduced into expressions only
\hsDef{instance Eq Foo} -- an instance
\hsDef{type alpha} -- a type variable
\hsDef{type Foo alpha} -- a type definition
\hsDef{foo :: Int -> String} -- a top-level function
\hsDef{import Prelude} -- an import that is always pulled in
\hsDef{import Prelude hiding (map)} -- an import always used hiding stuff
\hsDef{import Data.List(nub)} -- an import that is sometimes wanted



expression
type-expression
statement
list of expressions

-}
