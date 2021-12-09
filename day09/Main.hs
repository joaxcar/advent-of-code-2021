module Main where
import AOC
import qualified Data.Vector as V
import Data.Function (on)
import qualified Data.Map as Map
import Data.Maybe

main = do
  input <- getInputLines "test.txt"
  let grid = V.fromList $ map (V.fromList) $ map (map char2int) input
  let xs = length grid
  let ys = length $ grid V.! 1
  let sinks = filter (\(x, y) -> x ) $ [(findSink grid (x, y), (x, y)) | x <- [0..xs], y <- [0..ys] ]
  let solve1 = sumIt $ map ((getAt grid) . snd) sinks
  let solve2 = foldr (*) 1 $ take 3 $ reverse . sort $ map (length . (basin Map.empty grid) . snd) $ sinks
  print (solve1, solve2)


sumIt l = (+) (length l) $ sum l

findSink v p = let close = map (getAt v) $ neighbours p
                   pos = getAt v p
                in all (== True) $ map (pos <) close

neighbours (x, y) = let left = ((x - 1), y)
                        right = ((x + 1), y)
                        top = (x, (y - 1))
                        bottom = (x, (y + 1))
                      in (left:right:top:bottom:[])


-- First part
-- First part
index v (x, y) = do 
    xdir <- v V.!? x
    val <- xdir V.!? y
    return val 
    
getAt v p = 
  case index v p of
      Just a -> a
      _ ->  9
      
{- Bad non monadic version
getAt v (x, y) = 
  let xdir = v V.!? x
      value = case xdir of
        Just a -> a V.!? y
        _ -> Nothing
  in case value of
    Just a -> a
    _ -> 9
-}

-- Second part
basin m v (x, y) = let close = filter (\x -> (getAt v x) < 9) $ neighbours (x, y)
                       newMap = Map.insert (x, y) 1 m
                   in case getAt v (x, y) of
                      9 -> m
                      _ -> foldl (\m2 x-> if nonEmpty $ Map.lookup x m2 then basin (Map.insert x 1 m2) v x else m2) newMap close

nonEmpty Nothing = True
nonEmpty _ = False
