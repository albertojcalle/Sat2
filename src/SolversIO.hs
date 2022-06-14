module SolversIO (
    miosSolve
) where

import GHC.Float (int2Float, float2Int)
import Data.List ()
import Sat (solve)
import SatTypes ( SatInfo, satInfo )
import System.FilePath.Posix (makeRelative)
import System.IO (FilePath)
import System.Directory ( getDirectoryContents, listDirectory, doesFileExist , renameFile,
 getCurrentDirectory, setCurrentDirectory)
import SatTypes (SatInfo(solvable))
import Control.Monad (filterM)
import System.TimeIt ()

import SAT.Mios ( solveSAT, CNFDescription(CNFDescription) )
import SAT.Mios.Util.DIMACS ( fromFile )
import SatGenerator (write2Sat)

ruta :: FilePath
ruta = "./test/Examples/web/cnf/uf20-01.cnf" 
--ruta = "./test/Examples/UNSAT/sat002.cnf" 
--ruta = "./test/Examples/UNSAT/sat001.cnf"

outputPath :: FilePath
outputPath = "/home/alberto/github/2sat/test/Examples/SatGenerator/"

miosSolve :: FilePath -> IO [Int]
miosSolve file = do
    input <- fromFile file
    case input of
        Nothing -> error "Bad cnf file conversion."
        Just tuple -> do
            let 
                ((nVar,nClauses), clauses) = tuple
                description = CNFDescription nVar nClauses file
            solveSAT description clauses

{-|
Checks solvable or not and moves to corresponding directories:
    - SAT/UNSAT
    - execution time: 1,2,5,10 ...
Adds comment in DIMACS format.
-}

splitSatIO :: FilePath -> IO ()
splitSatIO file = do
    solution <- miosSolve file
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




cnfSolve :: FilePath -> IO ()
cnfSolve file =  do 
    input <- fromFile ruta
    case input of
        Nothing -> error "Bad cnf file conversion."
        Just tuple -> do
            let 
                ((nVar,nClauses), clauses) = tuple
                isSat2 = 2 == maximum (map length clauses)
            if isSat2 
                then
                    case solve clauses of
                        Left s -> putStrLn ( "Solution: " ++ show s)
                        Right c -> putStrLn ( "Contradiction: " ++ show c) 
                else 
                    error "Input Cnf file is not a sat2 formula." 

{-|
Basic converter of a cnf file to internal module type. Does not solve the formula. Checks if given information is coherent.
-}
cnfToSatInfo :: FilePath -> IO SatInfo
cnfToSatInfo ruta = 
    do
        input <- fromFile ruta
        case input of
            Nothing -> error $ "Bad cnf file at path"  ++ ruta
            Just tuple -> do
                let 
                    ((nVar,nClauses), clauses) = tuple
                if isSat2 clauses
                    then
                        return satInfo
                    else 
                        error $ "Input cnf is not sat2 formula at path" ++ ruta

{-|
Check if there are two literals in each clause.
TODO: add warning when ther are less than 2 literals
-}
isSat2 clauses = 2 == maximum (map length clauses)