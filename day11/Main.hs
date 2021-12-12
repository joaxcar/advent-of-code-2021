module Main where
import Data.List
import qualified Data.Map as M

import AOC

main = do
  input <- getInputLines "data.txt"
  let prog = M.fromList $ zip ([(x, y) | x <- [0..9], y <- [0..9]]) $ map char2int $ concat input
  print (solve1 prog, solve2 prog)

-- Part 1
solve1 l = sum $ map countZero $ take 101 $ step l
-- Part 2
solve2 l = length $ takeUntil allFlash [] $ step l
allFlash g = countZero g == (length $ M.keys g)
takeUntil f1 l (g:gs) = if f1 g then l else takeUntil f1 (g:l) gs 
countZero g = length $ M.filter (==0) g

-- Logic
step g = 
  let nextGrid = charge g
      loaded = getPosOf 10 nextGrid
      nextNext = flash nextGrid loaded
  in  g : (step nextNext)

flash g [] = g
flash g l = let afterFlash = foldl (setAt 0) g l
                bumpers = concatMap close l
                afterBump = foldl bumpNonZero afterFlash bumpers
                loaded = getPosOf 10 afterBump
            in flash afterBump loaded

close (x, y) = filter (\(xx, yy) -> xx >= 0 && xx < 10 && yy >= 0 && yy < 10) ((x-1,y):(x-1,y-1):(x-1,y+1):(x,y):(x,y-1):(x,y + 1):(x+1,y):(x+1,y-1):(x+1,y+1):[])

getPosOf n g = map fst $ M.toList $ M.filterWithKey (\_ v -> v >= n) g
charge = fmap (+1)

bumpNonZero g p = if getAt g p == 0 then g else setAtWith g p (+1) 
setAtWith g p f = M.adjust f p g
getAt g p = case g M.!? p of
  Nothing -> 0
  Just a -> a
setAt n g p = M.insert p n g
