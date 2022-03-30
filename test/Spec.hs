import Examples.Correct
import Examples.Incorrect
import System.IO (FilePath)
import SAT.Mios ( solveSAT, CNFDescription(CNFDescription) )
import SAT.Mios.Util.DIMACS ( fromFile, toFile)
import SAT.Mios.Util.DIMACS.Writer (toDIMACSString)
import Test.QuickCheck

ruta = "/home/alberto/github/2sat/src/Examples/cnf/"

main :: IO ()
main = do 
    result <- miosBench (ruta ++ "uf20-01.cnf")
    print result


{- prop_Solve_Mios :: Sat2 -> Bool
prop_Solve_Mios sat2 =
    cnfSolve sat2 = miosSolve sat2 -}


miosBench :: FilePath -> IO [Int]
miosBench file = do
    input <- fromFile file
    case input of
        Nothing -> error "Bad cnf file conversion."
        Just tuple -> do
            let 
                ((nVar,nClauses), clauses) = tuple
                descript = CNFDescription nVar nClauses file
            solveSAT descript clauses

{-|
Saves lists of clauses to a CNF file.

-}
saveSat2 path name = SAT.Mios.Util.DIMACS.toFile (path ++ name)

--  Examples.Correct.sat001

-- find contradictions
-- should be True for all examples in incorrect
-- any opposite $ map (Data.List.sort . Data.Tree.flatten) $ (Data.Graph.scc . cnfToGraph) Examples.Incorrect.unsat002

-- map (Data.List.sort . Data.Tree.flatten) $ (Data.Graph.scc . cnfToGraph) Examples.Correct.sat001