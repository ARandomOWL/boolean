module Tuura.SelfDual (
  parseToDNF, isSelfDual, getSelfDuals) where

import Control.Monad
import Data.Either
import Data.List

import Tuura.Boolean

isSelfDual :: Ord a => DNF a -> Bool
isSelfDual func = f == fd
    where f = final func
          fd = (final . dual) func
          dual = simplifyDNF . convertCNFtoDNF . dualDNF
          final = sort . fromDNF

-- List truth tables for all possible self-dual function of n variables
getSelfDuals :: Int -> [[Bool]]
getSelfDuals 0 = [[]]
getSelfDuals n = zipWith (++) truthTop truthBottom
    where cells = 2^n
          halfCells = cells `div` 2
          truthTop = replicateM halfCells [False, True]
          truthBottom = map (reverse . map not) truthTop

parseToCNF :: String -> CNF String
parseToCNF = simplifyCNF . convertToCNF . parseWrapper

parseToDNF :: String -> DNF String
parseToDNF = simplifyDNF . convertCNFtoDNF . simplifyCNF . parseToCNF

dualDNF :: DNF a -> CNF a
dualDNF = CNF . fromDNF
