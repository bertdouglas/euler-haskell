{-
Prime permutations
Problem 49
The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

What 12-digit number do you form by concatenating the three terms in this sequence?
-}

import Data.Numbers.Primes
import Data.List

dsplit :: Int -> [Int]
dsplit n = dsplit' n []
  where
    dsplit' n s
      | n > 0     = dsplit' rest (first:s)
      | otherwise = s
        where
          first = mod n 10
          rest  = div n 10

djoin :: [Int] -> Int
djoin s = djoin' s 0
  where
    djoin' s n
      | s == []   = n
      | otherwise = djoin' t ((n*10)+h)
        where
          h = head s
          t = tail s

four_digit_primes =
  filter (> 999)
  $ takeWhile (< 10000)
  $ primes

three_permute_dupes =
  map head
  $ filter (\x -> (length x) >= 3)
  $ group
  $ sort
  $ map sort
  $ map dsplit
  $ four_digit_primes

candidate_seq =
  filter (\x -> (length x) >= 3)
  $ map (map head)
  $ map group
  $ map sort
  $ map (intersect four_digit_primes)
  $ map (map djoin)
  $ map permutations
  $ three_permute_dupes

-- check to see if a list contains three adjacent values
-- in an arithmetic sequence
is_arith_seq :: [Int] -> Bool
is_arith_seq x =
  (/= [])
  $ filter (>=2)
  $ map length
  $ group
  $ zipWith (-) x (tail x)

main = do
  -- print four_digit_primes
  -- print three_permute_dupes
  -- print candidate_sequences
  -- print $ is_arith_seq [1,1,2,2,3,4,5]
  -- print $ is_arith_seq [1,4,5]
  print $ filter is_arith_seq candidate_seq
  


