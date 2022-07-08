module SatTypes (
    SatInfo (..),
    satInfo,
    Lit,
    Sat2,
    Scc,
    Solution,
    Equivalence,
    Contradiction
    ) where

import Data.Graph ( buildG, path, scc, Edge, Graph, components )
{-|
All the possible information about a 2sat formula.
 -}
data SatInfo = SatInfo {
     solution :: Solution
    ,formula :: Sat2
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
    ,formula = []
    ,graph = buildG (0,0) []
    ,equivalences = []
    ,maxLiteral = 0
    ,solvable = Nothing
}
type Lit = Int
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