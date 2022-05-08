{- module Main where


import Sat
import qualified OldMenu
import qualified Menu

import SAT.Mios ( solveSAT, CNFDescription(CNFDescription) )
import SAT.Mios.Util.DIMACS ( fromFile )
import System.IO (FilePath)
import System.FilePath.Posix (makeRelative)
import Data.List
import System.Directory

--TODO complete folder search and clasification
path :: IO [FilePath]
path = getDirectoryContents "./src/Examples/"



ruta :: FilePath
--ruta = "./test/Examples/web/cnf/uf20-01.cnf" 
ruta = "./test/Examples/UNSAT/sat002.cnf" 
--ruta = "./test/Examples/UNSAT/sat001.cnf" 

main :: IO ()
main = Menu.menu
   -- do cnfSolve ruta


cnfSolve :: FilePath -> IO ()
cnfSolve file =  do 
    input <- fromFile ruta
    case input of
        Nothing -> error "Bad cnf file conversion."
        Just tuple -> do
            let 
                ((nVar,nClauses), clauses) = tuple
                isSat2 = 2 == maximum (map length clauses)
            if isSat2 
                then
                    case solve clauses of
                        Left s -> putStrLn ( "Solution: " ++ show s)
                        Right c -> putStrLn ( "Contradiction: " ++ show c) 
                else 
                    error "Input Cnf file is not a sat2 formula." 
            


miosSolve :: FilePath -> IO [Int]
miosSolve file = do
    input <- fromFile file
    case input of
        Nothing -> error "Bad cnf file conversion."
        Just tuple -> do
            let 
                ((nVar,nClauses), clauses) = tuple
                description = CNFDescription nVar nClauses file
            solveSAT description clauses -}