module Main where
import qualified Data.Map as M
import Data.Map (Map)

import AOC

-- input target area: x=102..157, y=-146..-90
-- part 1: sum from 0 to abs of y min minus 1
main = do
  let move = mover target 157 (-146)
  let part1 = sum [1..145]
  let part2 = length $ filter id $ map move starts
  print (part1, part2)

starts = [((x,y), (0,0)) | x <- [7..157], y <- [-146..145]]

target = M.fromList [((x,y),1) | x <- [102..157], y <- [-146..(-90)]]

mover :: Map (Int, Int) Int -> Int -> Int -> ((Int,Int), (Int, Int)) -> Bool
mover t mx my ((vx,vy), (x,y)) = 
  if x > mx || y < my
  then False
  else if M.member (x,y) t
  then True
  else mover t mx my ((toZ vx, vy - 1), (x + vx, y + vy))

toZ x
  | x == 0 = 0
  | x > 0 = x - 1
  | x < 0 = x + 1
-- Domain model

