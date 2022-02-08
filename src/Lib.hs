module Lib
    ( ruta,
    someFunc,
    solve,
    condensate,
    cnfToGraph,
    opposite,
    findOpposite,
    setValues
    ) where

import Data.Graph ( scc, buildG, Graph, Edge, Forest, Vertex)
import Data.Tree ( flatten )
import Data.List (sort)

-- All the information about a 2sat formula
--TODO: Make a function solveSatInfo that finds all 4 values. 
data SatInfo = SatInfo {
     solution :: Solution 
    ,equivalences :: Equivalences
    ,maxValue :: Int
    ,solvable :: Bool
    }


type Solution = [(Int, Bool)]
{-|
Variables that are equivalent to other variables within the same list. On a graph they are
strongly connected components.

TODO: It may be used to give partial solutions.
-}
type Equivalences = [[Int]]

{-
The strongly connected component that causes a contradiction. To be used when proof is needed that a formula is unsolvable.
-}
type Contradiction = [Int]

ruta :: FilePath 
ruta = "/home/alberto/github/2sat/src/Examples/cnf/"


someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-|
TODO: Check topological order of scc output

Supposedly scc uses Tarjan's algorithm, so the output is in reverse topological order. topSort should be avoided in this case as it is simpler just to reverse the order.

NOTE: Maybe [] case is good.

TODO: If there is no solution it should return the contradictions that make it unsolvable.

-}
solve :: [[Int]] -> Either Solution Contradiction
solve x =
    let
        sccForest = (scc . cnfToGraph) x
        condensation = condensate x sccForest
        --solution = setValues condensation
    in case condensation of
        Right [] -> Right [] 
        Left x ->  Left [] 
--TODO: Complete cases.

{-|
Build a partial solution
TODO
Outputs the acyclic graph and the equivalences between variables so that multiple solutions can be found from them.
-}

{-|
Builds the condensation of a graph by substituting every strongly connected component with a new variable. The new graph has no cycles.
TODO: check type signature
TODO: substitution of variables

Substitutes all values in a scc for the smaller one since they are all equivalent.
-}
condensate :: [[Int]] -> Forest Vertex -> Either (Forest Vertex) Contradiction
condensate x y =
    let
        sccS = map (sort . flatten) y
        boolSccS = map opposite sccS
        foundContradiction = or boolSccS
        firstContradiction = dropWhile (not . opposite) sccS
        --isContradiction = any opposite componentLs
        --contradiction = elem True $ map opposite componentLs
    in if foundContradiction
        then Right []
        else Left y

-- type Solution = Either [(Int, Bool)] [[Int]]
{-
The solution is either a list of tuples of every variable number with the boolean value, or a list of connected components where a contradiction happened.
TODO: everything
-}

findContradiction x y =
    let
        componentLs = map (sort . flatten) y
        contradiction = any opposite componentLs
        --contradiction = elem True $ map opposite componentLs
    in if contradiction
        then Nothing
        else Just y


{-|
Whether the opposite element is on the list or not.
List must be sorted.
-}
opposite :: (Eq a, Num a) => [a] -> Bool
opposite ls =  case ls of
    x:y:xs -> (x == negate y && x /= 0) || opposite (y:xs)
    [x]    -> False
    []     -> False

findOpposite :: (Eq a, Num a) => [a] -> [a]
findOpposite ls =  case ls of
    x:y:xs -> if x == negate y && x /= 0 then [x,y] else findOpposite (y:xs)
    [x]    -> []
    []     -> []

{-|

-}
setValues x = case x of
    Nothing -> Nothing
    Just x -> Just (1,True)

{-|
Auxiliar function for reading formulas as lists.
-}
cnfToGraph :: [[Int]] -> Graph
cnfToGraph c =
    let
        vertList = concatMap (\[x,y]-> [(-x,y),(-y,x)]) c
        uppBound = maximum (map (uncurry max) vertList)
        lowBound = - uppBound
        -- since both x and -x are present there is no need to search for the lower bound 
        --lowBound = minimum (map minimum c)
        --NOTE: interesting error
        -- vertList = map (\[x,y]-> (x,y)) c
    in buildG (lowBound, uppBound) vertList