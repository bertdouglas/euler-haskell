{-
Consecutive prime sum
Problem 50
The prime 41, can be written as the sum of six consecutive primes:

41 = 2 + 3 + 5 + 7 + 11 + 13
This is the longest sum of consecutive primes that adds to a prime below one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most consecutive primes?
-}

import Data.Numbers.Primes
import Data.List
import qualified Data.Set as Set

limit = 10^6

-- find largest consecutive sum with n terms
csum :: Int -> Int -> (Int,Int)
csum m n = solve' n 0 0 prs prs
  where
    prs = takeWhile (< m) primes
    pset = Set.fromList $ prs
    solve' n ahead sum h1 h2
      | found    = (sum,n)
      | end      = (0,n)
      | filling  = solve' n (ahead+1) (sum + (head h1))             (tail h1) h2
      | synced   = solve' n (ahead+0) (sum + (head h1) - (head h2)) (tail h1) (tail h2)
        where
          found   = synced && (Set.member sum pset)
          end     = h1==[]
          filling = ahead < n
          synced  = ahead==n

csums = take 1000 $ map (csum limit) [2..]

main = do
  print $ csum 100 6
  print $ csum 1000 21 
  print csums
