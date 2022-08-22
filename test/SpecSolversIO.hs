module SpecSolversIO (
  spec_tarjanSolveIO
) where

import SolversIO ( miosSolvableIO, tarjanSolvableIO)
import Test.Hspec ( describe, it, shouldReturn, SpecWith, around, runIO )
import Control.Monad ( forM_ )
import Text.Printf (printf)
import Data.Graph (path)
import System.Directory

{- spec_tarjanSolveIO :: FilePath -> SpecWith ()
spec_tarjanSolveIO file =
  describe "Tarjan solver" $ do
    it "Checks if a cnf is solvable" $ do
      solution <- miosSolvableIO file
      tarjanSolvableIO file  `shouldReturn` solution -}


spec_tarjanSolveIO :: [FilePath] -> SpecWith ()
spec_tarjanSolveIO files =
  describe "Tarjan check isSolvable vs Mios" $ do
    forM_ files $ \file -> 
      it (printf "Checks if '%s' is solvable" file) $ do
        solution <- miosSolvableIO file
        tarjanSolvableIO file  `shouldReturn` solution



