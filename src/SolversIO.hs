module SolversIO
  ( miosSolve,
    tarjanSolve,
    tarjanSolvableIO,
    outputPath,
  )
where

import Control.Monad (filterM)
import Data.List ()
import GHC.Base (IO (IO))
import GHC.Float (float2Int, int2Float)
import SAT.Mios (CNFDescription (CNFDescription), solveSAT)
import SAT.Mios.Util.DIMACS (fromFile)
import Sat (solve)
import SatGenerator (write2Sat)
import SatTypes
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
import System.TimeIt ()

outputPath :: FilePath
outputPath = "/home/alberto/github/2sat/test/Examples/SatGenerator/"


{-| Basic converter of a cnf file to internal module type. Does not solve the formula. Checks if given information is coherent. -}
cnfToSatInfo :: FilePath -> IO SatInfo
cnfToSatInfo file =
  do
    input <- fromFile file
    case input of
      Nothing -> error $ "Bad cnf file at path" ++ file
      Just tuple -> do
        let ((nVar, nClauses), clauses) = tuple
            maxLiteral = maximum $ maximum clauses
        if isSat2 clauses
          then return satInfo { formula = clauses
                              , nVar = nVar
                              , nClauses = nClauses
                              , maxLiteral = maxLiteral}
          else error $ "Input cnf is not sat2 formula at path" ++ file

{-|
Check if there are two literals in each clause.
TODO: add warning when ther are less than 2 literals -}
isSat2 :: Foldable t => [t a] -> Bool
isSat2 clauses
  | 1 == minimum (map length clauses) = error "hola"
  | 2 == maximum (map length clauses) = True
  | otherwise = True

miosSolve :: FilePath -> IO SatInfo
miosSolve file = do
  input <- cnfToSatInfo file
  let nV = nVar input
      nC = nClauses input
      cl = formula input
      description = CNFDescription nV nC file
  solution <- solveSAT description cl
  let info = satInfo {formula = cl, solution = solution}
  return info

tarjanSolve :: FilePath -> IO SatInfo
tarjanSolve file = do
  info <- cnfToSatInfo file
  return $ solve info

tarjanSolveIO :: FilePath -> IO ()
tarjanSolveIO file = do
  info <- tarjanSolve file
  case isSolvable info of
    Just True -> putStrLn $ "Solution: " ++ show (solution info)
    Just False -> putStrLn $ "Contradiction: " ++ show (contradiction info)
    Nothing -> error "No valid result."

tarjanSolvableIO :: FilePath -> IO()
tarjanSolvableIO file = do 
    info <- tarjanSolve file
    print $ isSolvable info


{-| Checks if file is solvable or not and moves to corresponding directories:
    - SAT/UNSAT
    - execution time: 1,2,5,10 ...
Adds comment in DIMACS format. -}
splitSatIO :: FilePath -> IO ()
splitSatIO file = do
  solution <- solution <$> miosSolve file
  let solvable = not $ null solution
  let output =
        if solvable
          then "./SAT/" ++ file
          else "./UNSAT/" ++ file
  renameFile file output

classify :: FilePath -> IO ()
classify path = do
  setCurrentDirectory path
  paths <- getDirectoryContents path
  files <- filterM doesFileExist paths
  mapM_ splitSatIO files

generatorMain :: Int -> Int -> IO ()
generatorMain clauses n = do
  setCurrentDirectory outputPath
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
    funct y = write2Sat outputPath n y clauses



-- |
-- TODO: Add c comment with solvable max variable, time of execution etc.
addInfoCnf = undefined