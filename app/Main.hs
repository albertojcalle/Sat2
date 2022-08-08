module Main where


import SatGenerator (write2Sat)
import SolversIO


import Data.List
import GHC.Float (int2Float, float2Int)
import System.FilePath.Posix (makeRelative)
import System.IO (FilePath)
import System.Directory ( getDirectoryContents, listDirectory, doesFileExist , renameFile,
 getCurrentDirectory, setCurrentDirectory)
import SatTypes (SatInfo(isSolvable))
import Control.Monad (filterM)
import System.TimeIt
import Sat
import SatTypes

path :: IO [FilePath]
path = getDirectoryContents "./src/Examples/"

main = putStrLn "a"
{- main :: IO ()
main = do 
    let 
        ruta = outputPath ++ "SAT/2sat-8000c-7200l-5.cnf"  
    sol1 <- miosSolve ruta
    let a = subSat (formula sol1) (solution sol1)
    print a -}


            


