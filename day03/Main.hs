import Data.List
import AOC

main = do
  input <- getInputLines "test/data.txt"
  let prog =  (map char2int) <$> input
  print $ [solve1 prog, solve2 prog]

solve1 x = solver gamma epsilon x
solve2 x = solver ogr csr x
solver f1 f2 x =foldl (*) 1 $ map bin2num [f1 x, f2 x] 

gamma = (map mostCommonBit) . transpose
epsilon = (map leastCommonBit) . transpose
-- oxygen generator rating
ogr = bitCriteria gamma
-- CO2 scrubber rating
csr = bitCriteria epsilon

-- Måste fixa thresh...den ser för trist ut
mostCommonBit x =  if thresh x <= sum x then 1 else 0
  where thresh x = case (length x) `rem` 2 of
                  1 -> ((length x) `div` 2) + 1
                  0 -> (length x) `div` 2
leastCommonBit = flipBit <$> mostCommonBit

bitCriteria func list = bitbitCriteria' func 0 list
bitbitCriteria' _ _ (x:[]) = x
bitbitCriteria' func n x = bitbitCriteria' func (n + 1) (filter (\y -> ((head $ drop n y) == target x)) x)
  where target x = head $ drop n $ func x

-- Helper functions, move to AOC

bin2num x = bin2num' 1 0 $ reverse x
bin2num' _ a [] = a
bin2num' n a (x:xs) = bin2num' (n * 2) (a + (x * n)) xs

flipBit :: Int -> Int
flipBit x = case x of
            0 -> 1
            _ -> 0
            
char2int :: Char -> Int
char2int x = read [x]
