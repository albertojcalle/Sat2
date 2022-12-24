module Sat2.Sat
     (
    SatInfo,
    Sat2,
    solution,
    checkSolution,
    solve,
    getComponents,
    collapseSolution,
    condensate,
    sat2ToGraph
    )
    where

import Data.Graph ( buildG, path, scc, Edge, Graph, components )
import Data.Tree ( flatten, drawForest )
import Data.List ( sort, union, foldl')
import Data.List.Extra (disjoint)
import Sat2.Common ( buildEdge, opposite )
import Sat2.SatTypes
    ( Scc,
      Sat2,
      Solution,
      SatInfo(..))

import Data.Either (isLeft)


{-|
TODO: Check topological order of scc output

Supposedly scc uses Tarjan's algorithm, so the output is in reverse topological order. topSort should be avoided in this case as it is simpler just to reverse the order.
-}
solve :: SatInfo -> SatInfo
solve info =
    let result = (condensate . findEquivalences . sat2ToGraph) info
    in case isSolvable result of
        Just True -> setValues result
        Just False -> result
        Nothing -> error "Condensation does not return a valid result."

{- 
TODO: change to Integral
-}

findEquivalences :: SatInfo -> SatInfo
findEquivalences info = info { equivalences = zip ([1..] :: [Int]) (getComponents info)}

getComponents :: SatInfo -> [Scc]
getComponents info = map (sort . flatten) $ scc (graph info)

--TODO: Strict evaluation?
setValues :: SatInfo -> SatInfo
setValues info =
    let components = map snd (equivalences info)
        solution = foldl' collapseSolution [] components
    in info{solution = solution}

{-|
Collapses all the components in a particular solution.

If there are already positive Assignments for a literal, opposite literals must be assigned to false so the whole component sign is flipped. In the solution a negative sign implies the literal value is false.
-}
collapseSolution :: Solution -> [Int] -> Solution
collapseSolution solution x
  | disjoint solution opposite = solution `union` x
  | otherwise = solution `union` opposite
  where opposite = map negate x

{-|
Either returns the first contradiction that makes the problem unsolvable or a list of equivalences in the formula. An equivalence is a sorted list of literals which could have the same value.
-}
condensate :: SatInfo -> SatInfo
condensate info =
    let
        equivalences2 = equivalences info
        sccS = map snd equivalences2
        firstContradiction = dropWhile (not . opposite) sccS
        satGraph = [graph info]
        newEdges =  concat $ buildEdge <$> satGraph <*> equivalences2 <*> equivalences2
        newGraph = buildG (1, nVar info) newEdges
    in case firstContradiction of
        [] -> info{graph = newGraph, isSolvable = Just True}
        c:_  -> info{contradiction = c, isSolvable = Just False}

{-|
Convert sat2 formula to scc graph.
-}
sat2ToGraph :: SatInfo -> SatInfo
sat2ToGraph info =
    let
        vertList = concatMap (\[x,y]-> [(negate x,y),(negate y,x)]) (formula info)
        bound = nVar info
    in info{graph = buildG (negate bound, bound) vertList}

{-|
Checks if a solution is correct for a given Sat formula.
-}

type Assignment = Either Bool Int


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