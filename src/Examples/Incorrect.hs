{-
Incorrect (unsolvable) examples for 2sat formulas to use for testing and benchmarks. 
-}
module Examples.Incorrect
    ( unsat001 
    ) where
unsat001 :: (Integral a) => [[a]]
unsat001 = [[-1,-2],[-1,2],[1,-2],[1,2]]