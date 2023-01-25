import Criterion.Main ( bench, bgroup, whnfIO, defaultMain )
import Sat2.SolversIO (tarjanSolve, miosSolve, tarjanSolvableIO) 
import System.Directory ( getCurrentDirectory, getDirectoryContents, setCurrentDirectory )
import Data.Map (valid)

pathExamples :: FilePath
--pathExamples = "./test/Examples/safe/"
pathExamples = "./test/Examples/reallyHard/"

main :: IO()
main = testBunchFiles pathExamples

testBunchFiles :: FilePath -> IO ()
testBunchFiles path = do
  allFiles <- getDirectoryContents path
  print allFiles
  let validFiles2 = filter (\x -> head x == '2') allFiles
      validFiles = map (path ++) validFiles2
  defaultMain [
    bgroup "Solver" [ bench "tarjan"  $ whnfIO (mapM tarjanSolvableIO  validFiles)
               , bench "mios"  $ whnfIO (mapM miosSolve  validFiles)
               ]
    ]

