module Main where
import AOC

import Data.Function (on)
import qualified Data.Map as Map

main = do
  input <- getInputRaw "data.txt"
  let prog = getRight $ parse cmds "" input
  let solve1 = length . concat $ map (filterEasyNums . snd) prog
  let maps = map (\(x, y) ->  (flipMap $ mapTo Map.empty x, y)) prog
  let nums = map ((\(x, y) -> read (translate x y) :: Int)) maps
  print $ (solve1, sum nums)

-- First part
filterEasyNums = filter (\x -> length x == 3 || length x == 2 || length x == 4 || length x == 7)

-- Second part
translate m y = map (m Map.! ) y

flipMap m = Map.foldrWithKey (\x y m -> Map.insert y x m) Map.empty m

mapTo m (one : seven : four : n1 : n2 : n3 : n4 : n5 : n6 : eight: rest) = foldr picker (Map.fromList [('1', one), ('7',seven), ('4',four), ('8',eight)]) (n1:n2:n3:n4:n5:n6:[])
mapTo m _ = m

picker x m
  | length x == 5 = picker5 x m
  | length x == 6 = picker6 x m
  | otherwise = m

picker5 x m 
  | allButX x (m Map.! '7') 0 = Map.insert '3' x m
  | allButX x (m Map.! '4') 1 = Map.insert '5' x m
  | otherwise = Map.insert '2' x m

picker6 x m
  | allButX x (m Map.! '4') 0 = Map.insert '9' x m
  | allButX x (m Map.! '7') 0 = Map.insert '0' x m
  | otherwise = Map.insert '6' x m

allButX [] x n = length x == n
allButX (y:ys) x n = allButX ys (filter (/= y) x) n

-- parser
ws = many1 letter <* char ' '

line = do
  input <- count 10 (ws)
  string "| "
  output <- sepBy (many1 letter) (char ' ')
  return (sortBy (compare `on` length) $ map sort input, map sort output)

cmds = sepBy line newline
