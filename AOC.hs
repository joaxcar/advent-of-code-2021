module AOC where

getInputRaw path = readFile path
getInputLines path = lines <$> getInputRaw path
getInputInts path = map (\x -> read x ::Int) <$> getInputLines path

bool2int True = 1
bool2int False = 0
