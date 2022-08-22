
import SpecSolversIO (spec_tarjanSolveIO)
import SpecSat ( spec_tarjanSolve )
import Test.Hspec ( hspec )
import SolversIO (readExamples, listCNF)

path :: [Char]
--path = "./test/Examples/safe/"
path = "./test/Examples/SatGenerator/SAT/"

main :: IO ()
main = do
    files <- listCNF path
    --hspec $ spec_tarjanSolveIO files
    hspec $ spec_tarjanSolve files
    --hspec $ spec_checkSolution files
    