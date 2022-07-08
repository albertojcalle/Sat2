module Sat
    (
    SatInfo,
    Sat2,
    solution,
    checkSolution,
    solve
    ) where

import Data.Graph ( buildG, path, scc, Edge, Graph, components )
import Data.Tree ( flatten, drawForest )
import Data.List ( sort, union)
import Data.List.Extra (disjoint)
import Common ( buildEdge, opposite )
import SatTypes
    ( Scc,
      Contradiction,
      Sat2,
      Solution,
      SatInfo(equivalences, formula, maxLiteral, graph, solution) )
import GHC.Base (VecElem(Int16ElemRep))
import Data.Either (isLeft)

{-|
TODO: Check topological order of scc output

Supposedly scc uses Tarjan's algorithm, so the output is in reverse topological order. topSort should be avoided in this case as it is simpler just to reverse the order.
-}
solve :: SatInfo -> Either Solution Contradiction
solve info =
    let
        sccS = getComponents info2
        info2 = (sat2ToGraph info) {
            equivalences = zip ([1..] :: [Int]) sccS}
    in case condensate info2 of
        Right x -> Right x
        Left info2 ->  Left $ setValues info2

getComponents :: SatInfo -> [Scc]
getComponents info = map (sort . flatten) $ scc (graph info)

--TODO: Strict evaluation?
setValues :: SatInfo -> Solution
setValues info =
    let components = map snd (equivalences info)
    in foldl collapseSolution [] components

{-|
Collapses all the components in a particular solution.

If there are already positive Assignments for a literal, opposite literals must be assigned to false so the whole component sign is flipped. In the solution a negative sign implies the literal value is false.
-}
collapseSolution :: [Int] -> [Int] -> Solution
collapseSolution solution x
  | disjoint solution opposite = solution `union` x
  | otherwise = solution `union` opposite
  where opposite = map negate x

{-|
Either returns the first contradiction that makes the problem unsolvable or a list of equivalences in the formula. An equivalence is a sorted list of literals which could have the same value.
-}
condensate :: SatInfo -> Either SatInfo Contradiction
condensate info =
    let
        equivalences2 = equivalences info
        sccS = map snd equivalences2
        firstContradiction = dropWhile (not . opposite) sccS
        satGraph = [graph info]
        newEdges =  concat $ buildEdge <$> satGraph  <*> equivalences2 <*> equivalences2
        newGraph = buildG (1, maxLiteral info) newEdges
    in case firstContradiction of
        [] -> Left info{graph = newGraph}
        x:_  -> Right x

{-|
Auxiliar function for reading formulas as lists.
-}
--sat2ToGraph :: Sat2 -> ((Int,Int), Graph)
sat2ToGraph :: SatInfo -> SatInfo
sat2ToGraph info =
    let
        vertList = concatMap (\[x,y]-> [(-x,y),(-y,x)]) (formula info)
        uppBound = maxLiteral info
        lowBound = -uppBound
        bounds = (lowBound, uppBound)
    in info{graph = buildG bounds vertList}




{-|
Checks if a solution is correct for a given Sat formula.
-}

type Assignment = Either Bool Int

checkSolution :: Sat2 -> Solution -> Bool
checkSolution sat2 sol =
    let
        result = subSat sat2 sol
        isCorrect = all (all isLeft) result

        toAssignment :: Sat2 -> [[Assignment]]
        toAssignment = (map . map) Right

        subSat :: Sat2 -> Solution -> [[Assignment]]
        subSat sat2 sol = foldl subFormula (toAssignment sat2) sol
        
        subFormula :: [[Assignment]] -> Int -> [[Assignment]]
        subFormula intro s = (map . map) (`subLiteral` s) intro

        subLiteral :: Assignment -> Int -> Assignment
        subLiteral lit s = case lit of
            Left lit -> Left lit
            Right lit -> case () of
                ()| lit == s -> Left True
                  | lit == negate s -> Left False
                  | otherwise -> Right lit
    in isCorrect


