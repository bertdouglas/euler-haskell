{-
Coded triangle numbers
Problem 42
The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:

1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.

Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?
-}

import System.Environment
import Data.String.Utils
import Data.Char
import Data.List
import Math.NumberTheory.Powers.Squares

-- convert file content to list
to_list :: String -> [String]
to_list s = map read $ split "," s

-- value of a letter
value_c :: Char -> Int
value_c c = (ord c) - (ord 'A') + 1

-- value of a word
value_w :: String -> Integer
value_w w = sum $ map (fromIntegral . value_c) w

is_triangle :: Integer -> Bool
is_triangle x = isSquare (8*x + 1)

solve s = length $ filter is_triangle $ map value_w $ to_list s
  
main = do
  s <- readFile "words.txt"
  print $ take 10 $ to_list s
  print $ map value_c "SKY"
  print $ value_w "SKY"
  print $ solve s


