import Criterion.Main
import SolversIO (tarjanSolve, miosSolve, tarjanSolvableIO) 
import System.Directory ( getCurrentDirectory, getDirectoryContents )
import Data.Map (valid)

pathExamples :: FilePath
pathExamples = "/test/Examples/hard"

main :: IO()
main = testBunchFiles

testBunchFiles :: IO ()
testBunchFiles = do
  path <- getCurrentDirectory
  allFiles <- getDirectoryContents $  path ++ pathExamples
  let validFiles = filter (\x -> head x /= '.') allFiles
  print validFiles
  defaultMain [
    bgroup "Solver" [ bench "tarjan"  $ whnf (map tarjanSolvableIO) validFiles
               , bench "mios"  $ whnf (map miosSolve) validFiles
               ]
    ]

