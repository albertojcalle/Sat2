module Lib
    ( someFunc,
    solve,
    condensate,
    cnfToGraph,
    setValues
    ) where

import Data.Graph ( scc, buildG, Graph, Edge)
import Data.Tree ( flatten )
type Solution = (Int, Bool)

{-
-}
cnfToGraph :: [[Int]] -> Graph
cnfToGraph c = 
    let
        lowBound = minimum (map minimum c)
        uppBound = maximum (map maximum c)
        vertList = concatMap (\[x,y]-> [(-x,y),(-y,x)]) c
{-
NOTE: interesting error
-}
--        vertList = map (\[x,y]-> (x,y)) c
    in buildG (lowBound, uppBound) vertList

someFunc :: IO ()
someFunc = putStrLn "someFunc"

{-
TODO: Check topological order of scc output

Supposedly scc uses Tarjan's algorithm, so the output is in reverse topological order. topSort should be avoided in this case as it is simpler just to reverse the order.

NOTE: Maybe [] case is good.

-}
solve :: Graph -> Maybe Solution
solve g = 
    let 
        sccForest = scc g
        condensation = condensate sccForest
        solution = setValues condensation
    in case solution of 
        Nothing -> Nothing
        Just x -> Just x

{-
Builds the condensation of a graph by substituting every strongly connected component with a new variable. The new graph has no cycles.
TODO: check type signature
TODO: find opposite values
TODO: substitution of variables
-}

condensate x = 
    let 
        componentList = map flatten x
        contradiction = False --change
    in Just x



{-
-}

setValues x = case x of 
    Nothing -> Nothing
    Just x -> Just (1,True)