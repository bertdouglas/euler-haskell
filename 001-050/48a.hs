{-
Self powers
Problem 48
The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.

Find the last ten digits of the series, 1^1 + 2^2 + 3^3 + ... + 1000^1000.
-}

last_ten_digits i = i `mod` (10^10)

spow n = spow' n n 1
  where
    spow' n i out
      | out==0 = out
      | i == 0 = out
      | i > 0  = spow' n (i-1) (last_ten_digits (n*out))
    

solve n = last_ten_digits $ sum $ map spow [1..n]

main = do
  print $ solve 10
  print $ solve 1000

