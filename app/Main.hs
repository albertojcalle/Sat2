module Main where
import Lib
import SAT.Mios ( solveSAT, CNFDescription(CNFDescription) )
import SAT.Mios.Util.DIMACS ( fromFile )
import System.IO (FilePath)
import System.FilePath.Posix (makeRelative)

ruta :: FilePath
ruta = "src/Examples/original/cnf/uf20-01.cnf" 

main :: IO ()
main = do fileSolve ruta


fileSolve :: FilePath -> IO ()
fileSolve file =  do 
    input <- fromFile ruta
    case input of
        Nothing -> error "Bad cnf file conversion."
        Just tuple -> do
            let 
                ((nVar,nClauses), clauses) = tuple
                description = CNFDescription nVar nClauses ruta
            case solve clauses of
                Left s -> putStrLn ( "Solution: " ++ show s)
                Right c -> putStrLn ( "Contradiction: " ++ show c)


miosSolve :: FilePath -> IO [Int]
miosSolve file = do
    input <- fromFile file
    case input of
        Nothing -> error "Bad cnf file conversion."
        Just tuple -> do
            let 
                ((nVar,nClauses), clauses) = tuple
                description = CNFDescription nVar nClauses file
            solveSAT description clauses