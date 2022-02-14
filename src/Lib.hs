module Lib
    (
    solve,
    condensate,
    cnfToGraph,
    opposite,
    setValues
    ) where

import Data.Graph ( scc, buildG, Graph, Edge, Forest, Vertex, topSort)
import Data.Tree ( flatten )
import Data.List (sort)

--TODO: Make a function solveSatInfo that finds all 4 values. 

{-|
All the information about a 2sat formula.
 -}
data SatInfo = SatInfo {
     solution :: Solution 
    ,equivalences :: [Equivalence]
    ,maxValue :: Int
    ,solvable :: Bool
    }
type Sat2 = [[Int]]
{-|
A particular solution asigns a value T or F to every variable.  
 -}
type Solution = [(Int, Bool)]
{-|
Variables that are equivalent to other variables within the same list. On a graph they are
strongly connected components.

TODO: It may be used to give partial solutions.
-}
type Equivalence = [Int]
{-
The strongly connected component that causes a contradiction. To be used when proof is needed that a formula is unsolvable.
-}
type Contradiction = [Int]

{-|
Supposedly scc uses Tarjan's algorithm, so the output is in reverse topological order. topSort should be avoided in this case as it is simpler just to reverse the order.

TODO: Check topological order of scc output
TODO: If there is no solution it should return the contradictions that make it unsolvable.
-}
solve :: Sat2 -> Either Solution Contradiction
solve x =
    let
        sccForest = (scc . cnfToGraph) x
        condensation = condensate x sccForest
        --solution = setValues condensation
    in case condensation of
        Right x -> Right x 
        Left x ->  Left [] 
--TODO: Complete cases.

{-|
Either returns the first contradiction that makes the problem unsolvable or a list of equivalences in the formula. An equivalence is a sorted list of literals which could have the same value.
-}
condensate :: Sat2 -> Forest Vertex -> Either [Equivalence] Contradiction
condensate x y =
    let
        sccS = map (sort . flatten) y
        firstContradiction = dropWhile (not . opposite) sccS
    in case firstContradiction of 
        [] -> Left sccS
        x:_  -> Right x

{-|
Whether the opposite element is on the list or not.
List must be sorted.
-}
opposite :: (Eq a, Num a) => [a] -> Bool
opposite ls =  case ls of
    x:y:xs -> (x == negate y && x /= 0) || opposite (y:xs)
    [x]    -> False
    []     -> False

setValues x = case x of
    Nothing -> Nothing
    Just x -> Just (1,True)

{-|
Auxiliar function for reading formulas as lists.
-}
cnfToGraph :: Sat2 -> Graph
cnfToGraph c =
    let
        vertList = concatMap (\[x,y]-> [(-x,y),(-y,x)]) c
        uppBound = maximum (map (uncurry max) vertList)
        lowBound = - uppBound
    in buildG (lowBound, uppBound) vertList

--TODO: FUNCTIONS UNUSED - MAYBE DELETE

{-|
Build a partial solution
TODO
Outputs the acyclic graph and the equivalences between variables so that multiple solutions can be found from them.
-}
findContradiction x y =
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

ruta :: FilePath 
ruta = "/home/alberto/github/2sat/src/Examples/cnf/"