{-
Circular primes
Problem 35
The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
-}

import Data.Bits
import Data.Char
import Data.Numbers.Primes
import Data.List
import qualified Data.Set as Set


circles :: Int -> [Int]
circles n = circles' digits (length digits) []
  where
    digits = show n
    rotate x = (tail x) ++ [(head x)]
    circles' ds cnt out 
      | cnt > 0  = circles' (rotate ds) (cnt-1) (ds:out)
      | cnt == 0 = map read $ reverse out

solve :: Int -> Int
solve n = solve' 0 plist
  where
    plist = takeWhile (<n) primes
    pset = Set.fromList plist
    is_circular_prime p = all (`Set.member` pset) (circles p)
    solve' cnt (p:ps@pl)
      | pl == []            = cnt
      | is_circular_prime p = solve' (cnt+1) ps
      | otherwise           = solve' (cnt+0) ps

main = do
  print $ circles 1
  print $ circles 13
  print $ circles 197
  print $ solve 100
  print $ solve 1000000
    
{-
About 1.6 seconds on my 7 year old laptop.
It was speeded up by factor of 10 by moving all primes into a set, instead
of calling "Primes.isPrime".
-}
