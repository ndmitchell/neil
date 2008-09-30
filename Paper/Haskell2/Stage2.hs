
module Paper.Haskell2.Stage2(stage2) where

import Data.Char
import Data.List
import qualified Data.Set as Set
import Paper.Util.String
import Paper.Haskell2.Type
import Paper.Haskell2.Haskell


stage2 :: [HsLow] -> [HsItem]
stage2 xs = reverse stmts2 ++ exprs2
    where
        (defs,checks) = partition isHsDef $ nub xs
        defNames = nub $ concatMap parseDefs defs
        (stmts,exprs) = partition ((==) Stmt . lowType) checks
        (useNames,stmts2) = unzip $ map parseStmt stmts
        names = flip Set.member $ Set.fromList $ haskellKeywords ++ defNames ++ concat useNames
        exprs2 = concat $ zipWith (parseExpr defNames names) [1..] exprs



parseDefs :: HsLow -> [String]
parseDefs (HsDef pos x) = map (dropWhile (== ',')) $ splitStr "," x


parseStmt :: HsLow -> ([String],HsItem)
parseStmt (HsCheck pos Stmt whr x) = (lexer x, HsItem Stmt pos (fakeImplement x) whr)


parseExpr :: [String] -> (String -> Bool) -> Int -> HsLow -> [HsItem]
parseExpr names seen n (HsCheck pos expr whr x) =
    case lexer x of
        [] -> []
        [y] | seen y -> []
        ["(",y,")"] | seen y -> []
        [y] | isHaskellSym y -> f [y] ("(" ++ y ++ ")")
        lexed -> f lexed x
    where
        f lexed x = [HsItem Stmt pos (unwords (("auto_" ++ show n) : free) ++ " = " ++ x ++ "\n") whr]
            where free = names `intersect` lexed
