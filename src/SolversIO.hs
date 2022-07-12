module SolversIO (
    miosSolve,
    tarjanSolve,
    outputPath
) where

import GHC.Float (int2Float, float2Int)
import Data.List ()
import Sat (solve)
import SatTypes 
import System.FilePath.Posix (makeRelative)
import System.IO (FilePath)
import System.Directory ( getDirectoryContents, listDirectory, doesFileExist , renameFile,
 getCurrentDirectory, setCurrentDirectory)
import Control.Monad (filterM)
import System.TimeIt ()

import SAT.Mios ( solveSAT, CNFDescription(CNFDescription) )
import SAT.Mios.Util.DIMACS ( fromFile )
import SatGenerator (write2Sat)
import GHC.Base (IO(IO))

{- ruta :: FilePath
ruta = "./test/Examples/web/cnf/uf20-01.cnf"
 -}--ruta = "./test/Examples/UNSAT/sat002.cnf" 
--ruta = "./test/Examples/UNSAT/sat001.cnf"

outputPath :: FilePath
outputPath = "/home/alberto/github/2sat/test/Examples/SatGenerator/"


miosSolve :: FilePath -> IO SatInfo
miosSolve file = do
    input <- fromFile file
    case input of
        Nothing -> error "Bad cnf file conversion."
        Just tuple -> do
            let
                ((nVar, nClauses), clauses) = tuple
                description = CNFDescription nVar nClauses file
            solution <- solveSAT description clauses
            let info = satInfo{formula = clauses, solution = solution} 
            return info
            


tarjanSolve :: FilePath -> IO ()
tarjanSolve file =  do
    input <- fromFile file
    case input of
        Nothing -> error "Bad cnf file conversion."
        Just tuple -> do
            let
                ((nVar, nClauses), clauses) = tuple
                isSat2 = 2 == maximum (map length clauses)
                info = satInfo{formula = clauses, maxLiteral = nVar}
            if isSat2
                then
                    case solve info of
                        Left s -> putStrLn ( "Solution: " ++ show s)
                        Right c -> putStrLn ( "Contradiction: " ++ show c)
                else
                    error "Input Cnf file is not a sat2 formula."




{-|
Checks solvable or not and moves to corresponding directories:
    - SAT/UNSAT
    - execution time: 1,2,5,10 ...
Adds comment in DIMACS format.
-}
splitSatIO :: FilePath -> IO ()
splitSatIO file = do
    solution <- solution <$> miosSolve file
    let solvable =  not $ null solution
    let output = if solvable
        then "./SAT/" ++ file
        else "./UNSAT/" ++ file
    renameFile file output


classify :: FilePath -> IO ()
classify path = do
    setCurrentDirectory outputPath
    paths <- getDirectoryContents outputPath
    files <- filterM doesFileExist paths
    mapM_ splitSatIO files

generatorMain :: Int -> Int-> IO ()
generatorMain clauses n = do
    setCurrentDirectory outputPath
    mapM_ funct vars
    paths <- getDirectoryContents "."
    files <- filterM doesFileExist paths
    mapM_ splitSatIO files
    where
        x = GHC.Float.int2Float clauses
        suelo = floor(x*0.9)
        techo = ceiling(x*1.1)
        space = 5*(10 ^  float2Int (logBase 10 (1000 :: Float)-2))
        vars = [suelo,(suelo+space)..techo]
        funct y = write2Sat outputPath n y clauses

{-|
Basic converter of a cnf file to internal module type. Does not solve the formula. Checks if given information is coherent.
-}
cnfToSatInfo :: FilePath -> IO SatInfo
cnfToSatInfo file =
    do
        input <- fromFile file
        case input of
            Nothing -> error $ "Bad cnf file at path"  ++ file
            Just tuple -> do
                let
                    ((nVar,nClauses), clauses) = tuple
                if isSat2 clauses
                    then
                        return satInfo{formula = clauses}
                    else
                        error $ "Input cnf is not sat2 formula at path" ++ file

{-|
Check if there are two literals in each clause.
TODO: add warning when ther are less than 2 literals
-}
isSat2 :: Foldable t => [t a] -> Bool
isSat2 clauses = 2 == maximum (map length clauses)