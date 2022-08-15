module SpecCommon (
    prop_opposite,
    prop_areConnected

) where

import Test.QuickCheck(quickCheck, verbose)
import Common (opposite, areConnected)
import Test.QuickCheck.All (quickCheckAll)

-- Types needed
import SatTypes(Scc)
import Data.Graph (Graph)


--quickCheck prop_opposite
prop_opposite :: (Eq a, Num a) => [a] -> Bool
prop_opposite ls =
    opposite ls == opposite (map negate ls)

prop_areConnected :: Data.Graph.Graph -> SatTypes.Scc -> SatTypes.Scc -> Bool
prop_areConnected g xs ys =
    areConnected g xs ys == areConnected g ys xs
    
