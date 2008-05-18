
module Paper.Haskell2.Stage2(stage2) where

import Data.Char
import Data.List
import qualified Data.Set as Set
import Paper.Util.String
import Paper.Haskell2.Type
import Paper.Haskell2.Haskell


stage2 :: [HsLow] -> [HsItem]
stage2 xs = concat defStmts ++ reverse stmts2 ++ exprs2
    where
        (defs,checks) = partition isHsDef $ nub xs
        (defNames,defStmts) = unzip $ map parseDefs defs
        (stmts,exprs) = partition ((==) Stmt . lowType) checks
        (useNames,stmts2) = unzip $ map parseStmt stmts
        names = flip Set.member $ Set.fromList $ concat $ defNames ++ useNames
        exprs2 = concat $ zipWith (parseExpr names) [1..] exprs



parseDefs :: HsLow -> ([String], [HsItem])
parseDefs (HsDef pos x) | "instance " `isPrefixOf` x  || "import " `isPrefixOf` x
                            = ([],[HsItem Stmt pos x Always])
                        | null x = ([],[])


parseStmt :: HsLow -> ([String],HsItem)
parseStmt (HsCheck pos Stmt whr x) = (lexer x, HsItem Stmt pos (fakeImplement x) whr)


parseExpr :: (String -> Bool) -> Int -> HsLow -> [HsItem]
parseExpr seen n (HsCheck pos expr whr x) =
    case lexer x of
        [y] | seen y -> []
        ["(",y,")"] | seen y -> []
        [y] | all isHaskellSymbol y -> f ("(" ++ y ++ ")")
        _ -> f x
    where
        f x = [HsItem Stmt pos ("auto_" ++ show n ++ " = " ++ x) whr]
