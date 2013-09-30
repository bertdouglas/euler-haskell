--------------------------------------------------------------------------------
-- Maximum path sum I
-- Problem 18
-- By starting at the top of the triangle below and moving to adjacent numbers on 
-- the row below, the maximum total from top to bottom is 23.

--    3
--   7 4
--  2 4 6
-- 8 5 9 3

-- That is, 3 + 7 + 4 + 9 = 23.

-- Find the maximum total from top to bottom of the triangle below:

-- NOTE: As there are only 16384 routes, it is possible to solve this problem 
-- by trying every route. However, Problem 67, is the same challenge with a 
-- triangle containing one-hundred rows; it cannot be solved by brute force, 
-- and requires a clever method! ;o)
--------------------------------------------------------------------------------

import Data.List
import Data.String.Utils


triangle =
  ["                              75                              "
  ,"                            95  64                            "
  ,"                          17  47  82                          "
  ,"                        18  35  87  10                        "
  ,"                      20  04  82  47  65                      "
  ,"                    19  01  23  75  03  34                    "
  ,"                  88  02  77  73  07  63  67                  "
  ,"                99  65  04  28  06  16  70  92                "
  ,"              41  41  26  56  83  40  80  70  33              "
  ,"            41  48  72  33  47  32  37  16  94  29            "
  ,"          53  71  44  65  25  43  91  52  97  51  14          "
  ,"        70  11  33  28  77  73  17  78  39  68  17  57        "
  ,"      91  71  52  38  17  14  91  43  58  50  27  29  48      "
  ,"    63  66  04  68  89  53  67  30  73  16  69  87  40  31    "
  ,"  04  62  98  27  23  09  70  98  73  93  38  53  60  04  23  "
  ]


-- convert triangle from text to list of ints
to_list tri = reverse $ map do_line tri
  where
    do_line :: [Char] -> [Int]
    do_line s = map read $ splitWs $ strip s
        


-- Combine a pair of rows into a single new row
-- The first row "a" has length n, and the second "b" has length n-1
-- The value of the new row is the maximum sum through all paths
-- to each position in the new row.

-- example 
--   input:
--      8   5   9   3
--        2   4   6
--   output:
--       10  13  15
max_row a b = max_row' a b []
  where
    max_row' :: [Int] -> [Int] -> [Int] -> [Int]
    max_row' (a0:a1:a2) (b0:b1) c
      | (a2/=[]) && (b1/=[]) = max_row' (a1:a2) b1 c1
      | (a2==[]) && (b1==[]) = reverse c1
        where
          c1 = (max (a0+b0) (a1+b0)):c

main = do
  print $ max_row [8,5,9,3] [2,4,6]
  print $ to_list triangle
  print $ foldl1 max_row $ to_list triangle


