--------------------------------------------------------------------------------
-- Double-base palindromes
-- Problem 36
-- The decimal number, 585 = 10010010012 (binary), is palindromic in both bases.

-- Find the sum of all numbers, less than one million, which are palindromic in base 10 and base 2.

-- (Please note that the palindromic number, in either base, may not include leading zeros.)
--------------------------------------------------------------------------------

import Data.String.Utils
import Data.List
import Data.Char
import Data.String
import Numeric

bin :: Integer -> String
bin x = showIntAtBase 2 (head . show) x ""
dec :: Integer -> String
dec x = show x

ispal :: String -> Bool
ispal s = left == (reverse right)
  where
    n = length s
    nside = n `div` 2
    nskip = n - (2*nside) 
    left = take nside s
    right = drop (nside+nskip) s

double_palindrome :: Integer -> Bool
double_palindrome x = (ispal $ bin x) && (ispal $ dec x)

solve n = sum $ filter double_palindrome [0..n]

main = do
  print $ filter double_palindrome [0..200]
  print $ solve 1000000

