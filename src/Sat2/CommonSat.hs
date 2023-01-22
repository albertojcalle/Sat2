{-# LANGUAGE BangPatterns #-}
{-|
Functions over sat formulas. Inputs need to be sorted. 
-}
module Sat2.CommonSat
(sortSat,
    opposite,
    subEquivalences
)
where

import Data.List ( sortBy, sort, partition )
import Sat2.SatTypes( SatInfo(..), Scc )
import Sat2.Common ( allEqual, isNegative, isPositive, intersects )
import Data.Graph ( components, Graph, scc, Vertex, Forest )
import Data.Tree (flatten)


setValues :: SatInfo -> SatInfo
setValues info =
    let ! components = map snd (equivalences info)
        --solution = foldl' collapseSolution [] components
        solutionList = solution info ++ collapse components
    in info{solution = solutionList}

{-|
Collapses all the components in a particular solution.

If there are already positive Assignments for a literal, opposite literals must be assigned to false so the whole component sign is flipped. In the solution a negative sign implies the literal value is false.
-}

collapse :: (Ord a, Num a) => [[a]] -> [a]
collapse [] = []
collapse (x:xs)
    | (not . null) common = x ++ negCommonEnd ++ collapse (negCommonTry : uncommon)
    | otherwise = x ++ collapse uncommon
    where
        negX = map negate x
        (common, uncommon) = partition (negX `intersects`) xs
        negCommon = (map negate . concat) common
        (negCommonEnd, negCommonTry) = partition (`elem` x) negCommon

getComponents :: Graph -> [Scc]
getComponents g = map (sort . flatten) $ scc g

getScc :: SatInfo -> Forest Vertex
getScc info = scc (graph info)

{-|
SORTING FUNCTIONS
-}
sortSat :: (Ord a, Num a) => [[a]] -> [[a]]
sortSat x = sortBy compareClause $ map sortClause x

sortClause :: (Ord a, Num a) => [a] -> [a]
sortClause = sortBy compareLit

compareClause :: (Ord a, Num a) => [a] -> [a] -> Ordering
compareClause _ [] = EQ
compareClause [] _ = EQ
compareClause x y = compareLit (head x) (head y)

compareLit :: (Ord a, Num a) => a -> a -> Ordering
compareLit x y
    | rel == LT = LT
    | rel == GT = GT
    | signum x == signum y = EQ
    | otherwise = if signum x > signum y then GT else LT
    where
        rel = compare (abs x) (abs y)

{-|
COMMON FUNCTIONS
-}

{-|
Removes opposites in a list.
-}
removeSatOpps :: (Eq a, Num a) => [[a]] -> [[a]]
removeSatOpps = filter (not . null) . map removeOpps

removeOpps :: (Eq a, Num a) => [a] -> [a]
removeOpps ls = if opposite ls then [] else ls

{-|
Whether opposite elements are on the list or not.
List must be sorted.
-}
opposite :: (Eq a, Num a) => [a] -> Bool
opposite ls =  case ls of
    x:y:xs -> (x == negate y && x /= 0) || opposite (y:xs)
    [q]    -> False
    []     -> False


{-|
Substitutes a list of equivalences in a formula for the first literal of the equivalence, also opposites.
Removes opposite clauses.
Input:
    List of equivalences.
    Sat formula
Output:
    Reduced formula that can be used to generate a solution.
-}
subEquivalences :: SatInfo -> SatInfo
subEquivalences info = case isSolvable info of
    Nothing -> error "isSolvable must be known by this point."
    Just False -> info
    Just True -> info{
    reducedFormula = comp
    , solution = sol
   {-  ,solutionTree = case sT of
        Nothing -> error "Formula is solvable but adding values causes a contradiction."
        Just sT -> sT -}
        }
    where
        comp = map flatten (components $ graph info)
        comp2 = takeWhile (/=[0]) comp
        sol = concat comp2 -- TODO: this may be incorrect
        --rF = removeSatOpps $ map (subClause (equivalences info)) (formula info)
        -- (singles, rF2) = partition isDefiniteClause rF
        -- eq = equivalences info
        -- sT = addValueList (concat singles) emptySolution >>= addSatToSolutionTree rF2 >>= (eq `addComponentToSolutionTree`) 


isDefiniteClause :: Eq a => [a] -> Bool
isDefiniteClause = allEqual

{-|
Input:
    List of equivalences.
    Clause
-}

subClause :: (Num b, Eq b) => [(b, [b])] -> [b] -> [b]
subClause xs c = foldl (\ c x -> map (subLit x) c) c xs

--TODO: try to use a boolean instead to filter out already changed variables

subLit :: (Num a, Eq a) => (a, [a]) -> a -> a
subLit eq l
    | positiveIn = if isPositive e then e else negate e
    | negativeIn = if isNegative e then e else negate e
    | otherwise= l
    where
        positiveIn = l `elem` es
        negativeIn = negate l `elem` es
        es = snd eq
        e = fst eq




