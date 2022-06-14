module SatGenerator (
    write2Sat,
    eval2Sat,
    evalKSat
) where

import SatTypes ( Sat2, Lit )
import SAT.Mios.Util.DIMACS ( toFile)
import Control.Monad.Random (Rand, StdGen, evalRand, evalRandIO, mkStdGen, uniform, replicateM_)
import Control.Monad ( replicateM, zipWithM_ ) 

type Cloud = Rand StdGen

write2Sat :: [Char] -> Int -> Int -> Int -> IO ()
write2Sat path n vars clauses = zipWithM_ toFile paths satList
    where
        satList = eval2Sat n vars clauses
        paths =  map makePath [1..n]

        makePath :: (Show a) => a -> FilePath
        makePath x = 
            path ++ "/2sat-" ++ show clauses ++ "c-" ++ show vars ++ "l-"++ show x ++ ".cnf"

eval2Sat :: Int -> Int -> Int -> [Sat2]
eval2Sat = evalKSat 2 2022

evalKSat :: Int -> Int -> Int -> Int -> Int -> [Sat2]
evalKSat k seed n vars clauses = evalRand (genSatList n) (mkStdGen seed)
    where
        genSatList :: Int -> Cloud [Sat2]
        genSatList n = replicateM n $ genKSat k vars clauses

genKSat :: Int -> Int -> Int -> Cloud Sat2
genKSat k vars clauses = replicateM clauses $ genClause k
    where
        genClause :: Int -> Cloud [Lit]
        genClause k = replicateM k genLit

        genLit :: Cloud Lit
        genLit = do
            f <- uniform [1, -1]
            v <- uniform [1..vars]
            return (f*v)