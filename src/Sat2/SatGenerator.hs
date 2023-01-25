module Sat2.SatGenerator (
    writeKSat,
    evalKSat,
    genKSat,
    combinations,
    combinations'
) where

import Sat2.SatTypes (Sat, Sat2, Lit )
import SAT.Mios.Util.DIMACS ( toFile)
import Control.Monad.Random (Rand, StdGen, evalRand, evalRandIO, mkStdGen, uniform, replicateM_)
import Control.Monad ( replicateM, zipWithM_ )
import Text.Printf (printf)
import Data.List ( tails )

type Cloud = Rand StdGen

writeKSat :: FilePath -> Int -> Int -> Int -> Int -> IO ()
writeKSat path k n vars clauses = zipWithM_ toFile paths satList
    where
        satList = evalKSat k 2022 n vars clauses
        makePath x = printf "./%dsat-%dc-%dl-%d.cnf" k clauses vars  x
        paths = map makePath [1..n] :: [String]

{-|
K-sat formula with
- no trivial clauses ([-1,1,2], [1,1,2])
- clause repetition is allowed to avoid adding a limit to the number of clauses
-}
evalKSat :: Int -> Int -> Int -> Int -> Int -> [Sat]
evalKSat k seed n vars clauses = evalRand (genSatList n) (mkStdGen seed)
    where
        genSatList :: Int -> Cloud [Sat]
        genSatList n = replicateM n $ genKSat k vars clauses

genKSat :: Int -> Int -> Int -> Cloud Sat
genKSat k vars clauses =
    replicateM clauses $ genClause k
    where
        genClause :: Int -> Cloud [Lit]
        genClause k = do
            --uniform $ combinations k [-vars..vars]
            replicateM k genLit
        genLit :: Cloud Lit
        genLit = do
            f <- uniform [1, -1]
            v <- uniform [1..vars]
            return (f*v)

combinations :: (Eq t, Eq a, Num t, Num a) => t -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations n ls = [ x:ys | (x:xs) <- tails ls, x /= 0, ys <- combinations (n-1) xs]

{-|
Excluding trivial clauses.

Example:
[1,-1,2]

-}
combinations' :: (Eq t, Eq a, Num t, Num a) => t -> [a] -> [[a]]
combinations' 0 _ = [[]]
combinations' n ls = [ x:ys | (x:xs) <- tails ls, x /= 0, ys <- combinations (n-1) xs, -x `notElem` ys]