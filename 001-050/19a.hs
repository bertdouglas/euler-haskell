{-
Counting Sundays
Problem 19
You are given the following information, but you may prefer to do some research for yourself.

1 Jan 1900 was a Monday.
Thirty days has September,
April, June and November.
All the rest have thirty-one,
Saving February alone,
Which has twenty-eight, rain or shine.
And on leap years, twenty-nine.
A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?
-}

import qualified Data.Vector.Unboxed as DVU
import Data.List

is_leap_year :: Int -> Bool
is_leap_year y
  | d 400     = True
  | d 100     = False
  | d 4       = True
  | otherwise = False
    where
      d n = 0 == (mod y n)

-- test cases for leap years
leap_year_true  = [1600,1660,1724,1788,1848,1912,1972,2032,2092,2156,2220,2280,2344,2348]
leap_year_false = [1698,1699,1700,1750,1800,1810,1900,1901,1973,2100,2107,2200,2203,2289]

month_len_v :: DVU.Vector Int
month_len_v = DVU.fromList
  [31  --  0 jan
  ,28  --  1 feb
  ,31  --  2 mar
  ,30  --  3 apr
  ,31  --  4 may
  ,30  --  5 jun
  ,31  --  6 jul
  ,31  --  7 aug
  ,30  --  8 sep
  ,31  --  9 oct
  ,30  -- 10 nov
  ,31  -- 11 dec
  ]

-- length of a month
month_len y m
  | feb && leap = days + 1
  | otherwise   = days
    where
      feb  = (1==m)
      leap = is_leap_year y
      days = month_len_v DVU.! (m-1)

-- step to the next date
next_dt (y,m,d)
  | d < dmax  = (y,   m,   d+1)
  | m < mmax  = (y,   m+1, 1  )
  | otherwise = (y+1, 1,   1  )
    where
      dmax = month_len y m
      mmax = 12

-- enumerate all dates between two specified dates
enum_dt :: (Int,Int,Int) -> (Int,Int,Int) -> [(Int,Int,Int)]
enum_dt dt dtmax = enum_dt' dt []
  where
    enum_dt' dt out
      | dt/=dtmax = enum_dt' (next_dt dt) (dt:out)
      | otherwise = (dt:out)

solve = dt4
  where
    -- all dates, start on a Monday
    dt1 = enum_dt (1900,1,1) (2000,12,31)
    -- add day of week
    dt2 = zipWith (\(y,m,d) dow -> (y,m,d,dow)) dt1 $ cycle [0..6]
    -- remove those too early
    dt3 = filter (\(y,_,_,_) -> y/=1900) dt2
    -- first day of month is sunday
    dt4 = filter (\(_,_,d,dow) -> (d==1) && (dow==6)) dt3

main = do
  print month_len_v
  print $ map is_leap_year leap_year_true
  print $ map is_leap_year leap_year_false
  print solve
  print $ length solve
