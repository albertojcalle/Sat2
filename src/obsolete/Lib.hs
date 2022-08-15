module Lib
    (
    SatInfo,
    Sat2,
    solution,
    solve,
    condensate,
    sat2ToGraph,
    collapseSolution
    ) where

import Data.Graph ( buildG, path, scc, Edge, Graph, components )
import Data.Tree ( flatten, drawForest )
import Data.List ( sort, union)
import Data.List.Extra (disjoint)

{-|
All the information about a 2sat formula.
 -}
data SatInfo = SatInfo {
     solution :: Solution
    ,graph :: Graph
    ,equivalences :: [Equivalence]
    ,maxLiteral :: Int
    ,solvable :: Maybe Bool
    } deriving(Show)
{-|
Default record values for empty 2sat formula.
-}
satInfo :: SatInfo
satInfo = SatInfo{
     solution = []
    ,graph = buildG (0,0) []
    ,equivalences = []
    ,maxLiteral = 0
    ,solvable = Nothing
}
type Sat2 = [[Int]]
{-|
A positive value corresponds to a true literal, and negative corresponds to false.  
 -}
type Solution = [Int]
{-|
Variables that are equivalent to other variables on the original 2sat. On a graph they are
strongly connected components.
-}
type Equivalence = (Int, Scc)
{-
The strongly connected component that causes a contradiction. To be used when proof is needed that a formula is unsolvable.
-}
type Contradiction = [Int]
{-|
Strongly connected component.
-}
type Scc = [Int]
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
Look if two components are connected and build the corresponding edge.
-}
buildEdge :: Graph -> Equivalence -> Equivalence -> [Edge]
buildEdge g (x,xs) (y,ys) = [(x,y) | x /= y, areConnected g xs ys]

{-|
Wether two components xs, ys have connected edges on the original formula graph.
-}
areConnected :: Graph -> Scc -> Scc -> Bool
areConnected g xs ys = or (path g <$> xs <*> ys)

{-|
Whether the opposite element is on the list or not.
List must be sorted.
-}
opposite :: (Eq a, Num a) => [a] -> Bool
opposite ls =  case ls of
    x:y:xs -> (x == negate y && x /= 0) || opposite (y:xs)
    [x]    -> False
    []     -> False

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

isSolution :: [a] -> Bool
isSolution [] = False
isSolution _ =  True

{- drawVertexForest :: p -> Forest Vertex  -> String
drawVertexForest x = 
    let 
    in 
        drawForest  -}

--TODO: FUNCTIONS UNUSED - MAYBE DELETE

{-|
Build a partial solution

Outputs the acyclic graph and the equivalences between variables so that multiple solutions can be found from them.
-}
{- findContradiction x y =
    let
        componentLs = map (sort . flatten) y
        contradiction = any opposite componentLs
    in if contradiction
        then Nothing
        else Just y

findOpposite :: (Eq a, Num a) => [a] -> [a]
findOpposite ls =  case ls of
    x:y:xs -> if x == negate y && x /= 0 then [x,y] else findOpposite (y:xs)
    [x]    -> []
    []     -> []
-}