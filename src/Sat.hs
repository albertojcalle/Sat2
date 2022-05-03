module Sat
    (
    SatInfo,
    Sat2,
    solution,
    solve
    ) where

import Data.Graph ( buildG, path, scc, Edge, Graph, components )
import Data.Tree ( flatten, drawForest )
import Data.List ( sort, union)
import Data.List.Extra (disjoint)
import SAT.Mios.Util.DIMACS ( fromFile )

import Common
import SatTypes
{-|
Basic converter of a cnf file to internal module type. Does not solve the formula. Checks if given information is coherent.
-}
cnfToSatInfo :: FilePath -> IO SatInfo
cnfToSatInfo ruta = 
    do
        input <- fromFile ruta
        case input of
            Nothing -> error $ "Bad cnf file at path"  ++ ruta
            Just tuple -> do
                let 
                    ((nVar,nClauses), clauses) = tuple
                if isSat2 clauses
                    then
                        return satInfo
                    else 
                        error $ "Input cnf is not sat2 formula at path" ++ ruta

{-|
Basic converter of a sat2 list to internal module type. Does not solve the formula. Checks if given information is coherent.
-}
sat2ToSatInfo :: Sat2 -> SatInfo
sat2ToSatInfo info
    | otherwise = satInfo

{-|
Check if there are two literals in each clause.
TODO: add warning when ther are less than 2 literals
-}
isSat2 clauses = 2 == maximum (map length clauses)

solveInfo :: SatInfo -> SatInfo
solveInfo info = 
    let
        a = 1
    in info

{-|
TODO: Check topological order of scc output

Supposedly scc uses Tarjan's algorithm, so the output is in reverse topological order. topSort should be avoided in this case as it is simpler just to reverse the order.
-}
solve :: Sat2 -> Either Solution Contradiction
solve sat2 =
    let
        (bounds, satGraph) = sat2ToGraph sat2
        sccS = map (sort . flatten) $ scc satGraph
        info = satInfo{
             graph = satGraph
            ,equivalences = zip ([1..] :: [Int]) sccS
            ,maxLiteral = snd bounds}
        --solution = setValues condensation
    in case condensate info of
        Right x -> Right x
        Left info ->  Left $ setValues info

--TODO: Strict evaluation?
setValues :: SatInfo -> Solution
setValues info =
    let components = map snd (equivalences info)
    in foldl collapseSolution [] components

{-|
Collapses all the components in a particular solution.

If there are already positive asignments for a literal, opposite literals must be assigned to false so the whole component sign is flipped. In the solution a negative sign implies the literal value is false.
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
sat2ToGraph :: Sat2 -> ((Int,Int), Graph)
sat2ToGraph c =
    let
        vertList = concatMap (\[x,y]-> [(-x,y),(-y,x)]) c
        uppBound = maximum (map (uncurry max) vertList)
        lowBound = -uppBound
        bounds = (lowBound, uppBound)
    in (bounds, buildG bounds vertList)