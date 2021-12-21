module Main where
import qualified Data.Map as M
import Data.Map (Map)

import AOC

main = do
  datastream <- pure (map tr) <*> getInputRaw "test.txt"
  let (m:_:rest) = lines datastream
  let s = M.fromList $ zip [0..length m] m
  print $ M.foldr (+) 0 $ fmap char2int $ enhance s 2 rest

enhance s n ps = doit (div n 2) (\x -> update '1' s $ update '0' s x) $ enlarge n ps

enlarge n ps =
  let padding1 = take 50 $ repeat '0'
      padded1 = map (\x->(padding1 ++ x ++ padding1)) ps
      width = length $ head padded1
      padding2 = take width $ repeat '0'
      padded2 = (take 50 $ repeat padding2) ++ padded1 ++ (take 50 $ repeat padding2)
  in M.fromList $ zip [(x,y) | x <- [0..(width-1)] , y <- [0..(width-1)]]  $ concat padded2

doit 0 f ps = ps
doit n f ps = doit (n-1) f (f ps) 

update v s m = M.fromList $ map (update' s) $ M.keys m 
  where
    update' s p = (p, s M.! (getAt v m p)) 

getAt v m p =
  let ps = area p
      str = map (getP v m) ps
      val = binStr2num str
  in val

getP v m p = case M.lookup p m of
  Just a -> a
  Nothing -> v

tr '.' = '0'
tr '#' = '1'
tr a = a

area (x, y) = [(x+a,y+b) | a<-[-1..1] , b <- [-1..1]
