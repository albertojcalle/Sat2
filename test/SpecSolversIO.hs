module SpecSolversIO (
) where

import Test.QuickCheck
import SolversIO
import Test.Hspec
import Control.Monad (forM)
{- solveProp =  -}

{- spec_tarjanSolveIO file = do 
  let r1 = tarjanSolvableIO file
  let r2 = miosSolvableIO file
  describe "some test" (SpecWith a) $ do
      it "Checks if a cnf is solvable with same output as Mios."
                                r1 `shouldReturn` r2  -}