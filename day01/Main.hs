module Day6.Main where

import AOC

main = do {
x <- getInputInts "data.txt";
print $ [solution1 x, solution2 x]}

-- pure stuff
solution1 = countIncrements
solution2 x = countIncrements $ sumWindow 3 x

countIncrements x = sum $ map bool2int $ isIncrementing x

sumWindow n x =  if (length xs) >= n
  then (sum $ take n x) : sumWindow n (drop 1 x)
  else []

isIncrementing x = zipWith (<) x (drop 1 x
