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

-- maximum number of terms
maxn = length $ takeWhile (<limit) $ scanl1 (+) primes

-- all the primes we need to consider
prs = take maxn primes

-- find largest consecutive sum less than m with n terms
csum :: Int -> Int -> Int
csum m n = solve' n 0 0 prs prs
  where
    solve' n ahead sum h1 h2
      | found    = sum
      | end      = 0
      | filling  = solve' n (ahead+1) (sum + (head h1))             (tail h1) h2
      | synced   = solve' n (ahead+0) (sum + (head h1) - (head h2)) (tail h1) (tail h2)
        where
          found   = (isPrime sum) && synced && (sum < m)
          end     = (h1==[]) || (sum >= m)
          filling = ahead < n
          synced  = ahead==n

answer = head $ dropWhile (==0) $ map (csum limit) [maxn,maxn-1..]

main = do
  print maxn
  print $ csum 100 6
  print $ csum 1000 21 
  print answer

{-
Runtime about 10 milliseconds.

Not sure the solution is really general enough, but it gives right result
for this problem.

And my solution is kind of bulky.  Simpler is possible.

I would prefer a solution using cumulative sums, of all tails of the
infinite prime number sequence. 
-}

