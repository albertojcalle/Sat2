module SpecCommon (
    prop_opposite,
    prop_areConnected

) where

import Test.QuickCheck(quickCheck, verbose)
import Sat2.Common (areConnected)
import Sat2.CommonSat ( opposite )
import Test.QuickCheck.All (quickCheckAll)

-- Types needed
import Sat2.SatTypes(Scc)
import Data.Graph (Graph)


--quickCheck prop_opposite
prop_opposite :: (Eq a, Num a) => [a] -> Bool
prop_opposite ls =
    opposite ls == opposite (map negate ls)

prop_areConnected :: Data.Graph.Graph -> Sat2.SatTypes.Scc -> Sat2.SatTypes.Scc -> Bool
prop_areConnected g xs ys =
    areConnected g xs ys == areConnected g ys xs
    
