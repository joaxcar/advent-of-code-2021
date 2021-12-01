-- parse data
getInputRaw path = do readFile path
getInputLines path = do lines <$> getInputRaw path
getInputInts path = do map (\x -> read x ::Int) <$> getInputLines path

-- run IO
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

bool2int True = 1
bool2int False = 0

isIncrementing x = zipWith (<) x (drop 1 x
