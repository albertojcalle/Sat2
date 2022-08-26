module Sat2.SatGenerator (
    writeKSat,
    evalKSat
) where

import Sat2.SatTypes ( Sat2, Lit )
import SAT.Mios.Util.DIMACS ( toFile)
import Control.Monad.Random (Rand, StdGen, evalRand, evalRandIO, mkStdGen, uniform, replicateM_)
import Control.Monad ( replicateM, zipWithM_ ) 
import Text.Printf (printf)

type Cloud = Rand StdGen

writeKSat :: FilePath -> Int -> Int -> Int -> Int -> IO ()
writeKSat path k n vars clauses = zipWithM_ toFile paths satList
    where
        satList = evalKSat 2 2022 n vars clauses
        paths =  map makePath [1..n]
        makePath x = printf "%s/%dsat-%dc-%dl-%d.cnf" path  k clauses vars  x

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