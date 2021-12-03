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

-- Thresh är för tråkig. fixa!
mostCommonBit x =  if tresh x <= sum x then 1 else 0
  where tresh x = case (length x) `rem` 2 of
                  1 -> ((length x) `div` 2) + 1
                  0 -> (length x) `div` 2
leastCommonBit = flipBit <$> mostCommonBit

bitCriteria func list = bitCriteria' func 0 list
bitCriteria' _ _ (x:[]) = x
bitCriteria' func n x = bitCriteria' func (inc n) (filterByCrit x)
  where 
    inc x = x + 1
    filterByCrit x = filter (\y -> activeBit y == target x) x
    target x = activeBit $ func x
    activeBit x = head $ drop n x

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
