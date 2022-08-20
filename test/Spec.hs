
import SpecSolversIO ( spec_tarjanSolveIO, spec_tarjanSolveIOPath )
import Test.Hspec ( hspec )
import SolversIO (readExamples, listCNF)

path :: [Char]
path = "./test/Examples/SatGenerator/SAT/"


main :: IO ()
main = do
    files <- listCNF path
    hspec $ spec_tarjanSolveIOPath files