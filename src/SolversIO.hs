module SolversIO
  ( 
    cnfToSatInfo,
    readExamples,
    listCNF,
    tarjanSolve,
    miosSolve,
    tarjanSolvableIO,
    miosSolvableIO
  )
where

import System.Directory
    ( doesFileExist, getDirectoryContents, setCurrentDirectory, makeAbsolute )
import Control.Monad (filterM, forM)

import SAT.Mios (CNFDescription (CNFDescription), solveSAT)
import SAT.Mios.Util.DIMACS (fromFile)
import Sat (solve)
import SatTypes
    ( satInfo,
      SatInfo(..) )
import Common (subG)

{-| 
Basic converter of a cnf file to internal module type. Does not solve the formula. Checks if given information is coherent. -}
cnfToSatInfo :: FilePath -> IO SatInfo
cnfToSatInfo file =
  do
    input <- fromFile file
    case input of
      Nothing -> error $ "Bad cnf file at path" ++ file
      Just tuple -> do
        let ((nVar, nClauses), clauses) = tuple
            maxL = (abs . maximum . map maximum) clauses
            minL = (abs . minimum . map minimum) clauses
            bound = max maxL minL
        if bound /= nVar then error "Incorrect number of variables in file." else
          if isSat2 clauses
            then return satInfo { formula = clauses
                                , nVar = nVar
                                , nClauses = nClauses}
            else error $ "Input cnf is not sat2 formula at path" ++ file

{-|
Check if there are two literals in each clause.
TODO: add warning when ther are less than 2 literals -}
isSat2 :: Foldable t => [t a] -> Bool
isSat2 clauses
  | 1 == minimum (map length clauses) = error "Less than 2 literals."
  | 2 == maximum (map length clauses) = True
  | otherwise = True


miosSolve :: FilePath -> IO SatInfo
miosSolve file = do
  input <- cnfToSatInfo file
  let nV = nVar input
      nC = nClauses input
      cl = formula input
      description = CNFDescription nV nC file
  result <- solveSAT description cl
  return satInfo { formula = cl
                     , solution = result
                     , nVar = nV
                     , nClauses = nC
                     , isSolvable = Just $ (not . null) result
                     }

tarjanSolve :: FilePath -> IO SatInfo
tarjanSolve file = do
  info <- cnfToSatInfo file
  return $ Sat.solve info

tarjanSolveIO :: FilePath -> IO ()
tarjanSolveIO file = do
  info <- tarjanSolve file
  case isSolvable info of
    Just True -> putStrLn $ "Solution: " ++ show (solution info)
    Just False -> putStrLn $ "Contradiction: " ++ show (contradiction info)
    Nothing -> error "No valid result."

solvableIO :: FilePath -> (FilePath -> IO SatInfo) -> IO (Maybe Bool)
solvableIO file solver = do
    info <- solver file
    return $ isSolvable info

solvable :: SatInfo -> (SatInfo -> SatInfo) -> Maybe Bool
solvable info solver = isSolvable (solver info)

tarjanSolvable :: SatInfo -> Maybe Bool
tarjanSolvable info = solvable info Sat.solve

{- miosSolvable :: SatInfo -> Maybe Bool
miosSolvable info = solvable info miosSolve -}

tarjanSolvableIO :: FilePath -> IO (Maybe Bool)
tarjanSolvableIO file = solvableIO file tarjanSolve

miosSolvableIO :: FilePath -> IO (Maybe Bool)
miosSolvableIO file = solvableIO file miosSolve

readExamples :: FilePath -> IO [SatInfo]
readExamples path = do
      setCurrentDirectory path
      paths <- getDirectoryContents "."
      files <- filterM doesFileExist paths
      forM files cnfToSatInfo

listCNF :: FilePath -> IO [FilePath]
listCNF path = do
      paths <- getDirectoryContents path
      let onlyCNF = filter (\x ->  ".cnf" == subG (-4) (-1) x)
      return $ map (path ++) (onlyCNF paths)