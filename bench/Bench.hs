import Criterion.Main
import SolversIO (tarjanSolve, miosSolve, tarjanSolvableIO) 
import System.Directory ( getCurrentDirectory, getDirectoryContents )
import Data.Map (valid)

pathExamples :: FilePath
pathExamples = "./test/Examples/hard/"

main :: IO()
main = testBunchFiles

testBunchFiles :: IO ()
testBunchFiles = do
  path <- getCurrentDirectory
  allFiles <- getDirectoryContents pathExamples
  print allFiles
  let validFiles2 = filter (\x -> head x /= '.') allFiles
      validFiles = map (pathExamples ++) validFiles2
  defaultMain [
    bgroup "Solver" [ bench "tarjan"  $ whnfIO (tarjanSolvableIO (head validFiles))
               , bench "mios"  $ whnfIO (miosSolve (head validFiles))
               ]
    ]

