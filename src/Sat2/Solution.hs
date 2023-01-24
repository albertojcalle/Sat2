module Sat2.Solution (
    addSatToSolutionTree,
    addComponentToSolutionTree,
    addValueList,
    addValueList',
    flattenSolution,
    addValue,
    emptySolution,
    addClause,
    andValue,
    intToValue,
    checkSolution
    )
    where

import SAT.Mios.Criteria ()
import qualified Data.Map as Map
import Sat2.SatTypes
    ( Lit, Sat2, SolutionTree, Value(..), SatInfo (..), solution )
import Sat2.Common ( isNegative, isPositive, intersects )
import Data.List (foldl')
import Data.Map (toList)
import Data.Maybe (isNothing)

{-|
Input:
    - Solution tree
    - Literal to add
Output:
    -
-}
andValue :: Value -> Value -> Value
andValue x y = case (x, y) of
    (NONE, _) -> NONE
    (_, NONE) -> NONE
    (BOTH, v) -> v
    (v, BOTH) -> v
    (TRUE, FALSE) -> NONE
    (FALSE, TRUE) -> NONE
    (TRUE, TRUE) -> TRUE
    (FALSE, FALSE) -> FALSE

addValue ::  (Lit, Value) -> SolutionTree -> Maybe SolutionTree
addValue (lit, v2) tree =
    let
        valueTree = Map.lookup lit tree
    in case valueTree of
      Nothing -> Just $ Map.insert lit v2 tree
      Just v1 -> let v3 = andValue v1 v2 in
        case v3 of
            NONE -> Nothing
            _ -> Just $ Map.insert lit v3 tree

addValueList :: [Lit] -> SolutionTree -> Maybe SolutionTree
addValueList [] tree = Just tree
addValueList (x:xs) tree = addValue (intToValue x) tree >>= addValueList xs

addValueList' :: ([Lit], Value) -> SolutionTree -> Maybe SolutionTree
addValueList' ([], _) tree = Just tree
addValueList' (x:xs,v) tree = addValue (abs x, v) tree >>= addValueList xs

addClause :: Maybe SolutionTree -> [Lit] -> Maybe SolutionTree
addClause Nothing _ = Nothing
addClause tree [] = tree
addClause (Just tree) [x] = addValue (intToValue x) tree
addClause (Just tree) (x:xs) =
    let
        valueTree = Map.lookup (abs x) tree
        newValue =  andValue ((snd . intToValue) x) <$> valueTree
    in case newValue of
        Just NONE -> addClause (Just tree) xs
        Just newValue -> addValue (abs x, newValue) tree
        Nothing -> addValue (intToValue x) tree >>= addValueList' (xs, BOTH)

addSatToSolutionTree :: Sat2 -> SolutionTree -> Maybe SolutionTree
addSatToSolutionTree s tree = foldl' addClause (Just tree) s

emptySolution :: SolutionTree
emptySolution = Map.empty

valueToInt ::  (Lit, Value) -> Lit
valueToInt (lit, value)
    | value == TRUE = lit
    | value == FALSE = negate lit
    | otherwise = error "Conversion error valueToInt."

intToValue :: Int -> (Lit, Value)
intToValue int
    | isPositive int = (int, TRUE)
    | isNegative int = (abs int, FALSE)
    | otherwise = error "Conversion error intToValue."

addEqToSolutionTree :: SolutionTree -> (Int, [Int]) -> Maybe SolutionTree
addEqToSolutionTree tree (int, scc) = let
        valueTree = Map.lookup int tree
    in case valueTree of
        Just NONE -> Nothing
        Just valueTree -> addValueList' (scc, valueTree) tree
        Nothing -> addValueList' (scc, BOTH) tree

addComponentToSolutionTree :: [(Int, [Int])] -> SolutionTree -> Maybe SolutionTree
addComponentToSolutionTree [] tree = Just tree
addComponentToSolutionTree (e:eq) tree = addEqToSolutionTree tree e >>= addComponentToSolutionTree eq

flattenSolution :: SolutionTree -> [Lit]
flattenSolution sT = map valueToInt $ toList sT


{-|
Given a reduced formula with no connected clauses, opposite clauses or definite clauses gives a particular solution.

-}
getSolution :: (Eq a) => [[a]] -> [a]-> [a]
getSolution [] s = []
getSolution (c:cs) s
    | intersects c s = s ++ getSolution cs s
    | otherwise = c ++ s ++ getSolution cs s

getSolution' :: SatInfo -> SatInfo
getSolution' info =
    let
        rF = reducedFormula info
        solutionTree = addSatToSolutionTree rF emptySolution
    in case solutionTree of
        Just solutionTree -> info{solution = flattenSolution solutionTree}
        Nothing -> error "Error replacing solution values."

{- 
TODO: this is very slow
 -}
checkSolution :: SatInfo -> Bool
checkSolution info = subSat (solution info) (formula info)

subSat :: Eq a => [a] -> [[a]] -> Bool
subSat _ [] = True
subSat [] f = null (concat f)
subSat (l:ls) f = subSat ls $ subLit l f

subLit :: (Eq a) => a -> [[a]] -> [[a]]
subLit l = filter (l `notElem`)