module Main where
import Data.List
import qualified Data.Map as M
import Data.Map (Map)

import AOC

main = do
  input <- getInputRaw "data.txt"
  let prog = gr $ parse cmds "" input
  print $ M.toList $ f prog

gr (Right a) = a
gr (Left _) = (M.empty, [])

f d =
  let folds =  snd d
      grids = fst d
  in foldl fld grids folds
plot l =
  let allp = [(x,y) | x <- [0..38], y<-[0..5]]
      map = M.toList $ M.union (M.fromList l) (M.fromList allp)
      sort 
fld g ('y', n) =
  let flips = M.filterWithKey (\(x, y) _ -> y > n) g
      noFlip = M.filterWithKey (\(x, y) _ -> y < n) g
      flopp = M.mapKeys hori flips
      hori (x, y) = (x, n - (y - n))
  in M.union noFlip flopp
  
fld g ('x', n) = 
  let flips = M.filterWithKey (\(x, y) _ -> x > n) g
      noFlip = M.filterWithKey (\(x, y) _ -> x < n) g
      flopp = M.mapKeys hori flips
      hori (x, y) = (n - (x - n), y)
  in M.union noFlip flopp
-- parser
dot = do
  x <- num
  string ","
  y <- num
  string "\n"
  return ((x, y), 1)

num = (read :: String -> Int) <$> many1 digit
istr = do
  string "fold along "
  dir <- choice [char 'x', char 'y']
  string "="
  pos <- num
  return (dir, pos)

cmds = do
  dots <- many1 dot
  string "\n"
  is <- sepBy istr newline <* eof
  return (M.fromList dots, is)
