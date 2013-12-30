{-
Coin sums
Problem 31
In England the currency is made up of pound, £, and pence, p, and there are eight coins in general circulation:

1p, 2p, 5p, 10p, 20p, 50p, £1 (100p) and £2 (200p).
It is possible to make £2 in the following way:

1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
How many different ways can £2 be made using any number of coins?
-}

import Data.List

coins :: [Int]
coins = [200,100,50,20,10,5,2,1]

czero :: [Int]
czero = [0,0,0,0,0,0,0,0]

cone :: [Int]
cone = [0,0,0,0,0,0,0,1]

cvalue :: [Int] -> Int
cvalue x = sum $ zipWith (*) x coins

cshift :: [Int] -> [Int]
cshift x = (tail x) ++ [0]

cadd :: [Int] -> [Int] -> [Int]
cadd x y = zipWith (+) x y

cclear :: [Int] -> [Int] -> [Int]
cclear x a = zipWith (*) x $ map (1-) a

cstep :: [Int] -> [Int]
cstep x = cstep' x cone
  where 
    cstep' x a
      | a == czero = czero
      | v1 <= 200  = x1
      | otherwise  = cstep' (cclear x a) (cshift a)
        where 
          x1 = cadd x a
          v1 = cvalue x1

solve =
  length
    $ filter (200==)
    $ takeWhile (/=0)
    $ map cvalue
    $ iterate cstep cone

main = do
  print solve

