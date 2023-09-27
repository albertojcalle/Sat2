module Sat2.SatHelpers where

import Sat2.SatTypes ( Lit, SatInfo(formula, nClauses, nVar), satInfo )

listToSat2 :: [[Lit]] -> SatInfo
listToSat2 ls = satInfo{
    formula = ls,
    nClauses = length ls,
    nVar = (maximum . maximum) ((map . map) abs ls)}