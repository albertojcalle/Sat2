import Examples.Correct
import Examples.Incorrect
import System.IO (FilePath)
import SAT.Mios ( solveSAT, CNFDescription(CNFDescription) )
import SAT.Mios.Util.DIMACS ( fromFile, toFile)
import SAT.Mios.Util.DIMACS.Writer (toDIMACSString)

ruta = "/home/alberto/github/2sat/src/Examples/cnf/"

main :: IO ()
main = do 
    result <- miosBench (ruta ++ "uf20-01.cnf")
    print result



miosBench :: FilePath -> IO [Int]
miosBench file = do
    input <- fromFile file
    case input of
        Nothing -> error "Bad cnf file conversion."
        Just tuple -> do
            let clauses = snd tuple
            let nVar = (fst . fst) tuple
            let nClauses = (snd . fst) tuple
            let descript = CNFDescription nVar nClauses file
            solveSAT descript clauses

-- convert lists to CNF
-- SAT.Mios.Util.DIMACS.Writer.toFile (ruta ++ "sat001.cnf") Examples.Correct.sat001

-- find contradictions
-- should be True for all examples in incorrect
-- any opposite $ map (Data.List.sort . Data.Tree.flatten) $ (Data.Graph.scc . cnfToGraph) Examples.Incorrect.unsat002

-- map (Data.List.sort . Data.Tree.flatten) $ (Data.Graph.scc . cnfToGraph) Examples.Correct.sat001