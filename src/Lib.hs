module Lib
    (
    SatInfo,
    Sat2,
    solution,
    solve,
    condensate,
    setValues,
    cnfToGraph
    ) where

import Data.Graph ( buildG, path, scc, Edge, Graph ) 
import Data.Tree ( flatten, drawForest )
import Data.List ( sort )

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
A particular solution asigns a value T or F to every variable.  
 -}
type Solution = [(Int, Bool)]
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
Supposedly scc uses Tarjan's algorithm, so the output is in reverse topological order. topSort should be avoided in this case as it is simpler just to reverse the order.

TODO: Check topological order of scc output
-}
solve :: Sat2 -> Either Solution Contradiction
solve sat2 =
    let
        (bounds, satGraph) = cnfToGraph sat2
        sccS = map (sort . flatten) $ scc satGraph 
        info = satInfo{
             graph = satGraph
            ,equivalences = zip ([1..] :: [Int]) sccS
            ,maxLiteral = snd bounds}
        --solution = setValues condensation
    in case condensate info of
        Right x -> Right x
        Left x ->  Left [(1, True)]
--TODO: Complete cases.

{-|
Either returns the first contradiction that makes the problem unsolvable or a list of equivalences in the formula. An equivalence is a sorted list of literals which could have the same value.
-}
condensate :: SatInfo -> Either SatInfo Contradiction
condensate info =
    let
        sccS = map snd equivalences2
        equivalences2 = equivalences info
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

setValues g =
    let
        
    in
         [(1,True)]

{-|
Auxiliar function for reading formulas as lists.
-}
cnfToGraph :: Sat2 -> ((Int,Int), Graph)
cnfToGraph c =
    let
        vertList = concatMap (\[x,y]-> [(-x,y),(-y,x)]) c
        uppBound = maximum (map (uncurry max) vertList)
        lowBound = - uppBound
        bounds = (lowBound, uppBound)
    in (bounds, buildG bounds vertList)

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