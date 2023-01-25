module Main where
import Sat2.SolversIO (listCNF)
import GHC.Event (FdKey(keyFd))
import Sat2.SatGeneratorIO (generatorMain)

path :: [Char]
path = "./test/Examples/reallyHard/"
k :: Int
k = 2
n :: Int
n = 1
clauses :: Int
clauses = 100000

main :: IO ()
main = generatorMain path k n clauses

-- stack bench --benchmark-arguments "--output bench.html" && espeak "Process end."
-- stack test --profile && beep
            


