-- Module accesible from every other source file.
module Common (
    opposite,
    areConnected,
    buildEdge
) where

import Data.Graph ( path, Graph )
import SatTypes ( Scc )



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
Wether two components xs, ys have connected edges on the original formula graph.
-}
areConnected :: Graph -> Scc -> Scc -> Bool
areConnected g xs ys = or (path g <$> xs <*> ys)

{-|
Look if two components are connected and build the corresponding edge.
-}
buildEdge :: Graph -> (Int, Scc) -> (Int, Scc) -> [(Int,Int)]
buildEdge g (x,xs) (y,ys) = [(x,y) | x /= y, areConnected g xs ys]

--NOTE: NOT USED -----------------------------------------------------------------------------------
isSolution :: [a] -> Bool
isSolution [] = False
isSolution _ =  True