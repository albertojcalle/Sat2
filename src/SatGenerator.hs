module SatGenerator (
    generateSat,
    generateSat2,
    generateSat2'
) where

import SatTypes
import Control.Monad.Random (Rand, StdGen, evalRand, mkStdGen, uniform)
import Control.Monad (replicateM)

type Cloud = Rand StdGen

generateSat2 :: Int -> Int -> Int -> Sat2
generateSat2 = generateSat 2

{-|
Deterministic version for testing.
-}
generateSat2' :: Int -> Int -> Sat2
generateSat2' = generateSat 2 2022

generateSat :: Int -> Int -> Int -> Int -> Sat2
generateSat k seed vars clauses  = result
    where
        genSat :: Int -> Int -> Cloud Sat2
        genSat clauses k = replicateM clauses $ genClause k

        genClause :: Int -> Cloud [Lit]
        genClause k = replicateM k genLit

        genLit :: Cloud Lit
        genLit = do
            f <- uniform [1, -1]
            v <- uniform [1..vars]
            return (f*v)
        result = evalRand (genSat clauses k) (mkStdGen seed)