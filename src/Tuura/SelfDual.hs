module Tuura.SelfDual (
    isSelfDual, getSelfDuals) where

import Control.Monad (replicateM)
import Data.Either (isRight)
import Data.List (sort)

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
getSelfDuals n = map (concat . unpackTuple . reverseSnd) unzipped
    where cells = 2^n
          halfCells = cells `div` 2
          possibles = replicateM halfCells [(False, True), (True,False)]
          unzipped = map unzip possibles
          reverseSnd (a, b) = (a, reverse b)
          unpackTuple (a, b) = [a, b]

parseToCNF :: String -> CNF String
parseToCNF func =
  case parseExpr func of
    Right x -> (simplifyCNF . convertToCNF) x
    Left _  -> error $ "Error parsing " ++ show func

parseToDNF :: String -> DNF String
parseToDNF = simplifyDNF . convertCNFtoDNF . simplifyCNF . parseToCNF

dualDNF :: DNF a -> CNF a
dualDNF = CNF . fromDNF
