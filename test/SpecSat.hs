module SpecSat (
) where

import Test.QuickCheck(quickCheck, verbose)
import Sat ( checkSolution )
import Control.Monad (forM_)
import Text.Printf (printf)
import SolversIO ( miosSolve ) 
import Test.Hspec ( describe, it, shouldReturn, SpecWith, around, runIO )

spec_checkSolution :: FilePath -> SpecWith ()
spec_checkSolution file =
  describe "checkSolution" $ do
    it "Checks solution by substitution" $ do
      info <- miosSolve file
      return (checkSolution info) `shouldReturn` True

spec_checkSolutionPath :: [FilePath] -> SpecWith ()
spec_checkSolutionPath files =
  describe "checkSolution multiple" $ do
    forM_ files $ \file -> 
      it "Checks solution by substitution" $ do
        info <- miosSolve file
        return (checkSolution info) `shouldReturn` True