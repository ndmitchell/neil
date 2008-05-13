
module Paper.Haskell2.Type where




data Pos = Pos FilePath !Int
           deriving Show

data HsLow = HsDef   Pos String
           | HsCheck Pos Bool String String
             deriving Show


data HsItem = Item Pos Type String


data Type = None | Stmt | Expr | Instance | Import | ImportSome


data Provide = PInst String
             | PName String






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

