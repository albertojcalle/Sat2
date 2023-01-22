module Sat2.Sat
    (
        solve
    ) where

import Data.Graph
    ( buildG, path, scc, Edge, Graph, components, Vertex, Forest, stronglyConnComp )
import Data.Tree
 ( flatten, drawForest, Tree (subForest) )
import Data.List ( sort, union, foldl', partition )
import Data.List.Extra (disjoint)
import Sat2.Common ( buildEdge, intersects, isPositive)
import Sat2.CommonSat ( sortSat, opposite, subEquivalences)
import Sat2.SatTypes
    ( Scc,
      Sat2,
      Solution,
      SatInfo(..))

import Data.Either (isLeft)
import qualified Control.Applicative as Map
import Sat2.Solution (emptySolution, flattenSolution, addSatToSolutionTree)


{-|
TODO: Check topological order of scc output

Supposedly scc uses Tarjan's algorithm, so the output is in reverse topological order. topSort should be avoided in this case as it is simpler just to reverse the order.
-}
solve :: SatInfo -> SatInfo
solve info =
    let result = (subEquivalences . findEquivalences . infoAddGraph) info
    in case isSolvable result of
        Just True -> result
        Just False -> result
        Nothing -> error "Condensation does not return a valid result."

{-|
TODO: change to Integral
-}
findEquivalences :: SatInfo -> SatInfo
findEquivalences info = info{
    equivalences = eq
    , isSolvable = Just isSolvable}
    where
        sccs = filter (not . null . subForest) (scc (graph info))
        components = sortSat $ map flatten sccs
        isSolvable = not $ any opposite components
        classRepresentatives = map head components :: [Int]
        eq = filter (isPositive . fst) $ zip classRepresentatives components

{-|
Creates the condensated graph with 1 vertex for each scc.
TODO: is it faster to reuse the old graph?
-}
condensate :: SatInfo -> SatInfo
condensate info = case isSolvable info of
    Nothing -> error "isSolvable must be known by this point."
    Just False -> info
    Just True -> info{graph = newGraph}
    where
        newGraph = sat2ToGraph (reducedFormula info) (nVar info)
        {- equivalences2 = equivalences info
        sccS = map snd equivalences2
        firstContradiction = dropWhile (not . opposite) sccS
        satGraph = [graph info]
        newEdges =  concat $ buildEdge <$> satGraph <*> equivalences2 <*> equivalences2
        newGraph = buildG (1, nVar info) newEdges  -}
       
infoAddGraph :: SatInfo -> SatInfo
infoAddGraph info = case isSolvable info of
    Just False -> info
    _ -> info{graph = sat2ToGraph (formula info) (nVar info)}

{-|
Convert Sat2 formula to graph.
-}
sat2ToGraph :: Sat2 -> Int -> Graph
sat2ToGraph info bound =
    let vertList = concatMap (\[x,y]-> [(negate x,y),(negate y,x)]) info
    in  buildG (negate bound, bound) vertList