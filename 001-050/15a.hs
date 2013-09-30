--------------------------------------------------------------------------------
-- Lattice paths
-- Problem 15
-- Starting in the top left corner of a 2×2 grid, and only being able to move
--  to the right and down, there are exactly 6 routes to the bottom right corner.

-- How many such routes are there through a 20×20 grid?
--------------------------------------------------------------------------------

import Math.Combinatorics.Exact.Binomial

solve grids = choose (2*grids) grids

main = do
  print $ solve 2
  print $ solve 20

{-
This is equivalent to Pascal's triangle, just turned sideways, constrained
to even numbered rows, with only paths to the center position allowed.
-}

