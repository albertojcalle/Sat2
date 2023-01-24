module Sat2.SatTypes (
    SatInfo (..),
    satInfo,
    Lit,
    Sat,
    Sat2,
{-     Scc,
    Equivalence, -}
    Solution,
    SolutionTree,
    Value(..),
    Contradiction
    ) where

import Data.Graph ( Graph, buildG )
import qualified Data.Map as Map
{-|
All the possible information about a 2sat formula.
 -}
data SatInfo = SatInfo {
    formula :: Sat2
    ,reducedFormula :: Sat2
    ,solution :: Solution
    ,solutionTree :: SolutionTree
    ,contradiction :: Contradiction
    ,graph :: Graph
    ,components :: [[Int]]
    ,nVar :: Int
    ,nClauses :: Int
    ,isSolvable :: Maybe Bool
    } deriving(Show)
{-|
Default record values for empty 2sat formula.
-}
satInfo :: SatInfo
satInfo = SatInfo{
    formula = []
    ,reducedFormula = [] -- to deprecate
    ,solution = []
    ,solutionTree = Map.empty
    ,contradiction = []
    ,graph = buildG (0,0) []
    ,components = []
    ,nVar = 0
    ,nClauses = 0
    ,isSolvable = Nothing
}
type Lit = Int
type Sat2 = [[Int]]
type Sat = [[Int]]
{-|
A positive value corresponds to a true literal, and negative corresponds to false.  
 -}
type Solution = [Int]

data Value = NONE | TRUE | FALSE | BOTH deriving (Show, Eq)

type SolutionTree = Map.Map Lit Value

{- {-|
Variables that are equivalent to other variables on the original 2sat. On a graph they are
strongly connected components.
-}
type Equivalence = (Int, Scc) -}
{-
The strongly connected component that causes a contradiction. To be used when proof is needed that a formula is unsolvable.
-}
type Contradiction = [Int]

{- {-|
Strongly connected component.
-}
type Scc = [Int] -}