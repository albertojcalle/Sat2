module Sat2.Common (
    areConnected,
    anyConnectedPair,
    reversePair,
    buildEdge,
    intersects,
    subG,
    isPositive,
    isNegative,
    allEqual
) where

import Data.Graph ( path, Graph, Vertex )
import Sat2.SatTypes ( Value (..) )

{-|
Wether two components xs, ys have connected edges on the original formula graph.
-}
areConnected :: Graph -> [Int] -> [Int] -> Bool
areConnected g xs ys = or (path g <$> xs <*> ys)

anyConnectedPair :: Graph -> [[(Vertex, Vertex)]] -> Bool
anyConnectedPair g = any $ any (uncurry (path g))

{-|
Look if two components are connected and build the corresponding edge.
-}
buildEdge :: Graph -> (Int, [Int]) -> (Int, [Int]) -> [(Int,Int)]
buildEdge g (x,xs) (y,ys) = [(x,y) | x /= y, areConnected g xs ys]

intersects :: Eq a => [a] -> [a] -> Bool
intersects xs ys = any (`elem` ys) xs

{-|
Substring given two index.
-}
subG :: Int -> Int -> String -> String
subG start end word
  | start < 0 && end < 0  = sub (l + start + 1) (l + end + 1) word
  | otherwise = sub start end word
  where l = length word

sub :: Int -> Int -> String -> String
sub start end s = take (end-start+1) ( drop (start-1) s )

isPositive :: (Eq a, Num a) => a -> Bool
isPositive x = 1 == signum x

isNegative :: (Eq a, Num a) => a -> Bool
isNegative x = (-1) == signum x

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual [x] = True
allEqual (x:y:xs) = (x == y) && allEqual (y:xs)

reversePair :: (a,a) -> (a,a)
reversePair (x,y) = (y,x)
