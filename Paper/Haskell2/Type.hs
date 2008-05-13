
module Paper.Haskell2.Type where



data Pos = Pos FilePath !Int
           deriving Show

data HsLow = HsDef   Pos String
           | HsCheck Pos Bool String String
             deriving Show

-- HsCheck a b c d
--     a: line number 
--     b: True is Expr, False is Stmt
--     c: any proceeding command, possibly none
--        \ignore, or \hs{command}
--     d: code


data HsItem = Item {pos :: Pos, typ :: Type, code :: String, give :: [String], want :: [String]}


data Type = Stmt -- a statement
          | Expr -- an expression
          | Variable -- a variable
          | TypeVariable -- a type variable
          | TypeExpr -- a type expression
          | Import -- an import statement
          | Instance -- an instance definition



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
