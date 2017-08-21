module Tuura.SelfDual (
  parseToDNF, isSelfDual, getSelfDuals) where

import Control.Monad
import Data.Either
import Data.List
import Text.PrettyPrint.HughesPJClass

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

prettyBool :: [[Bool]] -> Doc
prettyBool = vcat . map (text . concat) . (map . map) (show .fromEnum)

parseToCNF :: String -> CNF String
parseToCNF = simplifyCNF . convertToCNF . parseWrapper

parseToDNF :: String -> DNF String
parseToDNF = simplifyDNF . convertCNFtoDNF . simplifyCNF . parseToCNF

dualDNF :: DNF a -> CNF a
dualDNF = CNF . fromDNF
