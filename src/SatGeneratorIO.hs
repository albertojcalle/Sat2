module SatGeneratorIO where

import System.Directory
  ( doesFileExist,
    getCurrentDirectory,
    getDirectoryContents,
    listDirectory,
    renameFile,
    setCurrentDirectory,
  )
import System.FilePath.Posix (makeRelative)
import System.IO (FilePath)
import SatGenerator (write2Sat)
import SatTypes ( SatInfo(solution) )
import SolversIO ( miosSolve )
import GHC.Float (float2Int, int2Float)
import Control.Monad (filterM)

{-|
Generates randomly with a preset seed some DIMACS CNF files. Then checks if file is solvable or not and moves to corresponding directories:
    - SAT/UNSAT
    - writes execution time: 1,2,5,10 of mios algorithm
    - comments if formula is solvable
-}
generatorMain :: Int -> Int -> FilePath -> IO ()
generatorMain clauses n path = do
  setCurrentDirectory path
  mapM_ funct vars
  paths <- getDirectoryContents "."
  files <- filterM doesFileExist paths
  mapM_ splitSatIO files
  where
    x = GHC.Float.int2Float clauses
    suelo = floor (x * 0.9)
    techo = ceiling (x * 1.1)
    space = 5 * (10 ^ float2Int (logBase 10 (1000 :: Float) -2))
    vars = [suelo, (suelo + space) .. techo]
    funct y = write2Sat path n y clauses



splitSatIO :: FilePath -> IO ()
splitSatIO file = do
  solution <- solution <$> miosSolve file
  let solvable = not $ null solution
      output =
        if solvable
          then "./SAT/" ++ file
          else "./UNSAT/" ++ file
  renameFile file output

classifyFolder :: FilePath -> IO ()
classifyFolder path = do
  setCurrentDirectory path
  paths <- getDirectoryContents path
  files <- filterM doesFileExist paths
  mapM_ splitSatIO files




-- |
-- TODO: Add c comment with solvable max variable, time of execution etc.
addInfoCnf = undefined