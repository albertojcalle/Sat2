import SpecSolversIO (spec_tarjanSolveIO)
import SpecSat ( spec_tarjanSolve, spec_miosSolve )
import Test.Hspec ( hspec )
import Sat2.SolversIO (readExamples, listCNF)

path :: [Char]
path = "./test/Examples/safe/"
--path = "./test/Examples/SatGenerator/SAT/"

main :: IO ()
main = do
    files <- listCNF path
    --hspec $ spec_tarjanSolveIO files
    --hspec $ spec_miosSolve files
    hspec $ spec_tarjanSolve files
    --hspec $ spec_checkSolution files
    