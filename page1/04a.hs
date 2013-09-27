--------------------------------------------------------------------------------
-- Largest palindrome product
-- Problem 4
-- A palindromic number reads the same both ways. The largest palindrome made
-- from the product of two 2-digit numbers is 9009 = 91 × 99.
-- 
-- Find the largest palindrome made from the product of two 3-digit numbers.
--------------------------------------------------------------------------------

-- To build:  ghc --make foo.hs
-- To run:    ./foo

import Data.List
import Debug.Trace

nextij :: Integer -> Integer -> Integer -> Integer -> (Integer,Integer)
nextij i j min max =
  if (i+2) <= max
    then (i+2, j   )
    else (min, j+11)

ispal :: Integer -> Bool
ispal i = 
  let
    s = show i
    n = (length s) `div` 2
    (l,r) = splitAt n s
  in
    l == (reverse r)

psearch' :: Integer -> Integer -> Integer -> Integer -> [Integer] -> [Integer]
psearch' i j min max ps =
  let
    (i1,j1) = nextij i j min max
    stop = j > max
    candidate = i*j
    found = ispal candidate
  in
    case (found,stop) of
      (True, False) -> psearch' i1 j1 min max (candidate:ps)
      (False,False) -> psearch' i1 j1 min max ps
      (_     ,True) -> ps

-- search for odd multiple of 11
adjust :: Integer -> Integer
adjust i = 
  let
    m11 = 0 == (mod i 11)
  in
    if (odd i) && m11
      then i
      else adjust $ i-1

psearch :: Integer -> Integer
psearch n =
  let
    max = (10^n) -1
    nf = fromIntegral n
    backoff = (+) 100 $ truncate $ 10**(nf*0.5)
    min = max - backoff
    min' = adjust min
    ps = psearch' min' min' min' max []
  in
    --traceShow (min',max) $
    head $ reverse $ sort ps

answer = map psearch [2,3,4,5,6,7,8]

main = print answer

{-
At first I had a much cruder solution that was slow for the 3-digit case,
and for the 4-digit case it was so slow that I got tired of waiting.  
So I did a little algebra and found some constraints on the factors.

One of the factors will be a multiple of 11

  for N=3 case
    P=100000x10000y1000z100z10yx
    P=100001x10010y1100z
    P=119091x910y100z

  for N=4 case
    P=1000_0000w + 100_0000x10_0000y1_0000z1000z100y10x + w
    P=1000_0001w + 100_0010x10_0100y1_1000z
    P=11909091w + 90910x9100y1000z

Likewise you can show that both factors will be odd.  So the possible
values for factors will be:
  f1 = 1,3,5...
  f2 = 1,33,55...
This reduces search effort by a factor of 44=11*2*2

Then I found an empirical lower bound for the factors:
   10^n - 10^(n*0.5) + 100
See the spreadsheet in file "4a.ods".
There is no proof that the lower bound holds for larger n.  If the lower
bound is too large, then, then the program fails, rather than give the 
wrong solution.  So it won't mislead you.

I pushed n beyond the value of 3 stated in the problem to show that the
solution is somewhat scalable.  Up to n=8 it finishes in seconds.
For n=9 it does not finish in a minute.  

Any further improvement is probably going to require a totally different 
approach.  One idea is to work backward from a list of all possible
palindromes, then check each for factors. The small primes can be
removed quickly.  What remains is a one-dimensional search.
I suspect any big improvement will require some new insight from someone
that knows more about number theory.

Some results collected mostly by hand.
       91 (7*13)               99 (3*3*11)                     9009
      913 (11*83)             993 (3*331)                     906609
     9901 (9901)             9999 (3*3*11*101)               99000099
    99681 (3*149*223)       99979 (11*61*149)               9966006699
   999001 (19*52579)       999999 (3*3*3*7*11*13*37)       999000000999
  9997647 (3*11*302959)   9998017 (9998017)               99956644665999
 99990001 (99990001)     99999999 (3*3*11*73*101*137)    9999000000009999

The cases for odd n and even n are distinct.  The even case seems to follow
a very simple pattern, where one factor is as large as possible.  The 
odd case is not so regular, but both factors seem to converge to the 
square root of the palindrome.

-}

