module SpecSat (
  spec_checkSolution,
  spec_tarjanSolve
) where

import Test.QuickCheck(quickCheck, verbose)
import Sat ( checkSolution )
import Control.Monad (forM_)
import Text.Printf (printf)
import SolversIO ( miosSolve, tarjanSolve ) 
import Test.Hspec ( describe, it, shouldReturn, SpecWith, around, runIO )
import SatTypes (SatInfo(isSolvable))
import SpecSolversIO (spec_tarjanSolveIO)

{- spec_checkSolution :: FilePath -> SpecWith ()
spec_checkSolution file =
  describe "checkSolution" $ do
    it "Checks solution by substitution" $ do
      info <- tarjanSolve file
      return (checkSolution info) `shouldReturn` True -}

spec_checkSolution :: [FilePath] -> SpecWith ()
spec_checkSolution files =
  describe "checkSolution multiple" $ do
    forM_ files $ \file -> 
      it (printf "checkSolution with mios of '%s'" file) $ do
        info <- miosSolve file
        let solvable = isSolvable info
        return (Just (checkSolution info)) `shouldReturn` solvable


{-|
The solution given by tarjanSolve is checked in the original formula.
 -}
spec_tarjanSolve :: [FilePath] -> SpecWith ()
spec_tarjanSolve files =
  describe "checkSolution multiple" $ do
    forM_ files $ \file -> 
      it (printf "Checks tarjanSolve solution is coherent '%s'" file) $ do
        info <- tarjanSolve file
        let solvable = isSolvable info
        return (Just (checkSolution info)) `shouldReturn` solvable