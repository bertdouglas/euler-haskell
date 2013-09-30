--------------------------------------------------------------------------------
-- Names scores
-- Problem 22
-- Using names.txt (right click and 'Save Link/Target As...'), a 46K text file 
-- containing over five-thousand first names, begin by sorting it into 
-- alphabetical order. Then working out the alphabetical value for each name, 
-- multiply this value by its alphabetical position in the list to obtain a 
-- name score.

-- For example, when the list is sorted into alphabetical order, COLIN, which 
-- is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN
-- would obtain a score of 938 × 53 = 49714.

-- What is the total of all the name scores in the file?
--------------------------------------------------------------------------------


import System.Environment
import Data.String.Utils
import Data.Char
import Data.List

-- convert name string to list
to_list :: String -> [String]
to_list s = map read $ split "," s

-- get value of a character
value_c :: Char -> Integer
value_c c = fromIntegral ((ord c) - (ord 'A') + 1)

-- get value of a name
value_n :: (String,Integer) -> Integer
value_n (s,i) = (*) i $ sum $ map value_c s

-- sort and add indexes
prepare :: [String] -> [(String,Integer)]
prepare l = zip (sort l) [1..]
    
solve s = sum $ map value_n $ prepare $ to_list s

main = do
  s <- readFile "names.txt"
  print $ take 10 $ to_list s
  print $ take 10 $ prepare $ to_list s
  print $ map value_c "COLIN"
  print $ solve s



