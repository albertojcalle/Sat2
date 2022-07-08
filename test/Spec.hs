import System.IO (FilePath)
import System.Directory (getDirectoryContents)
import SAT.Mios ( solveSAT, CNFDescription(CNFDescription) )
import SAT.Mios.Util.DIMACS ( fromFile, toFile)
import SAT.Mios.Util.DIMACS.Writer (toDIMACSString)
import Test.QuickCheck
import SolversIO (outputPath) 

path :: FilePath
path = outputPath

main :: IO ()
main = do 
    result <- processCNF (path ++ "Example100-1.cnf")
    print result


simpleTest :: a
simpleTest = undefined 
    where
        files = getDirectoryContents path


{- prop_Solve_Mios :: FilePath -> Bool
prop_Solve_Mios sat2 =
    cnfSolve sat2 = miosBench sat2 -}


processCNF :: FilePath -> IO (CNFDescription, [[Int]])
processCNF file = do
    input <- fromFile file
    case input of
        Nothing -> error "Bad cnf file conversion."
        Just tuple -> do
            let 
                ((nVar,nClauses), clauses) = tuple
                descript = CNFDescription nVar nClauses file
            return (descript, clauses)