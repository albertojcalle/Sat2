module Lib
    ( someFunc,
    solve,
    condensate,
    cnfToGraph,
    setValues
    ) where

import Data.Graph ( scc, buildG, Graph, Edge, Forest, Vertex)
import Data.Tree ( flatten )
import Data.List (sort)
type Solution = (Int, Bool)

{-
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

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-
TODO: Check topological order of scc output

Supposedly scc uses Tarjan's algorithm, so the output is in reverse topological order. topSort should be avoided in this case as it is simpler just to reverse the order.

NOTE: Maybe [] case is good.

-}
solve :: [[Int]] -> Maybe Solution
solve x =
    let
        g = cnfToGraph x
        sccForest = scc g
        condensation = condensate x sccForest
        solution = setValues condensation
    in case solution of
        Nothing -> Nothing
        Just x -> Just x

{-
Builds the condensation of a graph by substituting every strongly connected component with a new variable. The new graph has no cycles.
TODO: check type signature
TODO: substitution of variables
-}
condensate :: [[Int]] -> Forest Vertex -> Maybe (Forest Vertex)
condensate x y =
    let
        componentLs = map (sort . flatten) y
        contradiction = any opposite componentLs
        --contradiction = elem True $ map opposite componentLs
    in Just y

{-
Whether the opposite element is on the list or not.
List must be sorted.
-}
opposite :: (Eq a, Num a) => [a] -> Bool
opposite ls =  case ls of
    x:y:xs -> (x == negate y && x /= 0) || opposite (y:xs)
    [x]    -> False
    []     -> False


{-
-}

setValues x = case x of
    Nothing -> Nothing
    Just x -> Just (1,True)