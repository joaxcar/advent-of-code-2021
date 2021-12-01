module AOC where

getInputRaw path = do readFile path
getInputLines path = do lines <$> getInputRaw path
getInputInts path = do map (\x -> read x ::Int) <$> getInputLines path

bool2int True = 1
bool2int False = 0
