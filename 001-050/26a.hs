{-
Reciprocal cycles
Problem 26
A unit fraction contains 1 in the numerator. The decimal representation of the unit fractions with denominators 2 to 10 are given:

1/2	= 	0.5
1/3	= 	0.(3)
1/4	= 	0.25
1/5	= 	0.2
1/6	= 	0.1(6)
1/7	= 	0.(142857)
1/8	= 	0.125
1/9	= 	0.(1)
1/10	= 	0.1
Where 0.1(6) means 0.166666..., and has a 1-digit recurring cycle. It can be seen that 1/7 has a 6-digit recurring cycle.

Find the value of d < 1000 for which 1/d contains the longest recurring cycle in its decimal fraction part.
-}

import Data.List

limit = 1000

rcycle :: Int -> [(Int,Int)]
rcycle d = rcycle' d 1 []
  where
    rcycle' :: Int -> Int -> [(Int,Int)] -> [(Int,Int)]
    rcycle' d r qr
      | have_repeat = qr
      | (r >= d)    = rcycle' d (r1*10) ((q1,r):qr)
      | (r < d)     = rcycle' d (r*10)  ((0,r):qr)
        where
          have_repeat = (qr /= []) && (elem (head qr) (tail qr))
          q1 = div r d
          r1 = mod r d

clength qr = i+1
  where
    h = head qr
    t = tail qr
    Just i = elemIndex h t

solve d = solve' d 0 0
  where
    solve' d dmax lmax
      | d == 0 = (dmax,lmax)
      | d > 0  = solve' (d-1) dmax1 lmax1
        where
          l = clength $ rcycle d
          new_max = l > lmax
          lmax1 = if new_max then l else lmax
          dmax1 = if new_max then d else dmax

main = do
  print $ reverse $ rcycle 6
  print $ clength $ rcycle 6
  print $ reverse $ rcycle 7
  print $ clength $ rcycle 7
  print $ reverse $ rcycle 101
  print $ clength $ rcycle 101
  print $ solve (limit-1)
  
