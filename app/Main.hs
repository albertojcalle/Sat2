module Main where


import SatGenerator (write2Sat)
import SolversIO


import Data.List
import GHC.Float (int2Float, float2Int)
import System.FilePath.Posix (makeRelative)
import System.IO (FilePath)
import System.Directory ( getDirectoryContents, listDirectory, doesFileExist , renameFile,
 getCurrentDirectory, setCurrentDirectory)
import SatTypes (SatInfo(solvable))
import Control.Monad (filterM)
import System.TimeIt

path :: IO [FilePath]
path = getDirectoryContents "./src/Examples/"

main :: IO ()
main = print "a"


            


