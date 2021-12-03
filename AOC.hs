module AOC where

getInputRaw path = readFile path
getInputLines path = lines <$> getInputRaw path
getInputInts path = map (\x -> read x ::Int) <$> getInputLines path

bool2int True = 1
bool2int False = 0

-- Convert integer to binary string (with padding) and reverse
num2bin l n = padWith 0 l $ num2bin' [] n
num2bin' a 0 = reverse a
num2bin' a n = num2bin' ((rem n 2) : a) (div n 2)

bin2num x = bin2num' 1 0 $ reverse x
bin2num' _ a [] = a
bin2num' n a (x:xs) = bin2num' (n * 2) (a + (x * n)) xs

-- Padding for a string
padWith c l x = if (length x) >= l then x else padWith c l (c:x)

-- Flip binary string
flipBit :: Int -> Int
flipBit x = case x of
            0 -> 1
            _ -> 0

-- Parse int from char
char2int :: Char -> Int
char2int x = read [x]
