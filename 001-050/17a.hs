--------------------------------------------------------------------------------
-- Number letter counts
-- Problem 17
-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
--------------------------------------------------------------------------------

import Data.List
import Data.String.Utils

ones x
  | x==0 = ""
  | x==1 = "one"
  | x==2 = "two"
  | x==3 = "three"
  | x==4 = "four"
  | x==5 = "five"
  | x==6 = "six"
  | x==7 = "seven"
  | x==8 = "eight"
  | x==9 = "nine"
  | x==10 = "ten"
  | x==11 = "eleven"
  | x==12 = "twelve"
  | x==13 = "thirteen"
  | x==14 = "fourteen"
  | x==15 = "fifteen"
  | x==16 = "sixteen"
  | x==17 = "seventeen"
  | x==18 = "eighteen"
  | x==19 = "nineteen"

tens x
  | x==0 = ""
  | x==1 = "ten"
  | x==2 = "twenty"
  | x==3 = "thirty"
  | x==4 = "forty"
  | x==5 = "fifty"
  | x==6 = "sixty"
  | x==7 = "seventy"
  | x==8 = "eighty"
  | x==9 = "ninety"

letter_form n = unwords $ words $ unwords s4
  where
    d1000 = div n 1000
    m1000 = mod n 1000
    d100 = div m1000 100
    m100 = mod n 100
    d10 = div m100 10
    m10 = mod n 10
    s0 = if d1000 >= 1
      then [ones d1000] ++ ["thousand"]
      else []
    s1 = if d100 >= 1
      then s0 ++ [ones d100] ++ ["hundred"]
      else s0
    s2 = if (m100 /= 0) && ((d100 /= 0) || (d1000 /=0))
      then s1 ++ ["and"]
      else s1
    s3 = if m100 >= 20
      then if m10 > 0
        then s2 ++ [(tens d10) ++ "-" ++ (ones m10)]
        else s2 ++ [tens d10]
      else s2 ++ [ones m100]
    s4 = if n==0 
      then s3 ++ ["zero"]
      else s3

count s = length s2
  where
    s1 = replace "-" "" s
    s2 = replace " " "" s1

solve = sum $ map (count . letter_form) [1..1000]

main = do
  print $ letter_form 0
  print $ letter_form 9
  print $ letter_form 10
  print $ letter_form 11
  print $ letter_form 19
  print $ letter_form 20
  print $ letter_form 21
  print $ letter_form 99
  print $ letter_form 100
  print $ letter_form 101
  print $ letter_form 999
  print $ letter_form 1000
  print $ letter_form 1001
  print $ letter_form 342
  print $ letter_form 115
  print $ count $ letter_form 342
  print $ count $ letter_form 115
  print solve








