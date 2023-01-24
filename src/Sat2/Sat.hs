module Sat2.Sat
    {- (
        solve
    ) -}
    where
-- ( buildG, path, scc, Edge, Graph, components, Vertex, Forest, stronglyConnComp )
import qualified Data.Graph as Graph
import Data.Tree
 ( flatten, drawForest, Tree (subForest) )
import Data.List ( sort, union, foldl', partition )
import Data.List.Extra (disjoint)
import Sat2.Common ( buildEdge, intersects, isPositive, anyConnectedPair)
import Sat2.CommonSat ( sortSat, opposite, oppositeRemain)
import Sat2.SatTypes
    ( Sat2,
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
    let result = (subEquivalences . findComponents . infoAddGraph) info
    in case isSolvable result of
        Just True -> result
        Just False -> result
        Nothing -> error "Condensation does not return a valid result."

{-|
TODO: change to Integral
-}
findComponents :: SatInfo -> SatInfo
findComponents info = info{
      components = comp
    , isSolvable = isSolvable}
    where
        comp = map flatten (Graph.components $ graph info)
        --sccs = filter (not . null . subForest) (scc (graph info))
        --components = sortSat $ map flatten sccs
        possibleContradictions = map (oppositeRemain . sort) comp
        isSolvable = case possibleContradictions of
            [] -> Just True
            c -> if anyConnectedPair (graph info) possibleContradictions
                        then Just False
                        else Just True

{-         classRepresentatives = map head components :: [Int]
        eq = filter (isPositive . fst) $ zip classRepresentatives components -}

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
sat2ToGraph :: Sat2 -> Int -> Graph.Graph
sat2ToGraph info bound =
    let vertList = concatMap (\[x,y]-> [(negate x,y),(negate y,x)]) info
    in  Graph.buildG (negate bound, bound) vertList

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
        comp = components info
        comp2 = takeWhile (/=[0]) comp
        sol = concat comp2 -- TODO: this may be incorrect
        --rF = removeSatOpps $ map (subClause (equivalences info)) (formula info)
        -- (singles, rF2) = partition isDefiniteClause rF
        -- eq = equivalences info
        -- sT = addValueList (concat singles) emptySolution >>= addSatToSolutionTree rF2 >>= (eq `addComponentToSolutionTree`) 