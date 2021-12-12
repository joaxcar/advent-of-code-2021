module Main where
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Map (Map)
import Data.Char

import AOC

main = do
  input <- getInputRaw "data.txt"
  let prog = getRight $ parse cmds "" input
  let mapOf = foldl addToMap M.empty prog
  print $ length . (filter nothing). concat $ map (paths mapOf [] [Start]) (mapOf M.! Start) 

addToMap m (from, to) = M.insertWith addTo from ([to]) m
nothing Nothing = False
nothing _ = True

paths m p l End = [Just (End:l)]
paths m p l n = 
  -- Part 2
  let vals = filter (max2 p) $ m M.! n
  -- Part 1
  --let vals = filter (not . (`elem` p)) $ m M.! n
  in case vals of
    [] -> [Nothing]
    _ -> case n of
         Start -> [Nothing]
         (S x) -> if max2 p (S x) then concat $ map (paths m ((x):p) ((S x):l)) vals else [Nothing]
         (L x) -> concat $ map (paths m p ((L x):l)) vals

max2 p (S n) = not (elem n p) || (all ((< 2) . length) $ group . sort $ p)
max2 _ _ = True

addTo :: [Node] -> [Node] -> [Node]
addTo (x:xs) y = x:y
 
-- parser
data Node = Start | End | S String | L String deriving (Show, Ord, Eq)
data Path = P Node deriving (Show)
tr s = case s of
       "end" -> End
       "start" -> Start
       st -> if isLower $ head st then S st else L st 

path = choice [try $ string "start", try $ string "end", many1 letter]
con = do
  from <- path
  string "-"
  to <- path
  let fromN = tr from
  let toN = tr to
  return [(fromN, toN), (toN, fromN)]

cmds = concat <$> sepBy con newline
