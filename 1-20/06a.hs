--------------------------------------------------------------------------------
-- Sum square difference
-- Problem 6
-- The sum of the squares of the first ten natural numbers is,
-- 1^2 + 2^2 + ... + 10^2 = 385
--
-- The square of the sum of the first ten natural numbers is,
-- (1 + 2 + ... + 10)^2 = 55^2 = 3025

-- Hence the difference between the sum of the squares of the first ten 
-- natural numbers and the square of the sum is 3025 − 385 = 2640.

-- Find the difference between the sum of the squares of the first one hundred 
-- natural numbers and the square of the sum.
--------------------------------------------------------------------------------

limit = 100

sum_n_1 n = n * (n+1)            `div` 2

sum_n_2 n = n * (n+1) * (2*n +1) `div` 6

solve = square_of_sum - sum_of_squares
  where
    sum_of_squares = sum_n_2 limit
    square_of_sum = (sum_n_1 limit) ^2

main = print solve
