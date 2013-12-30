{-
Truncatable primes
Problem 37
The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.
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

check :: [[Int]] -> Bool
check s = 
  and
    $ map isPrime
    $ map djoin
    $ drop 1
    $ reverse
    $ drop 1 s

main = do
  print $ dsplit 1234
  print $ djoin [1,2,3,4]
  print $ (check . inits) [3,7,9,7]
  print $ (check . tails) [3,7,9,7]

  print
    $ sum
    $ take 11
    $ filter (>9)
    $ map djoin
    $ filter (check . tails)
    $ filter (check . inits)
    $ map dsplit
    $ primes

