import Data.List
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Sequence (Seq( (:<|) ), fromList, (|>))

import AOC

main = do
  input <- getInputRaw "test.txt"
  let prog =  getRight $ parse cmds "" input
  let shoal = initShoal prog
  print $ sumS $ shoal `getAt` 256

-- Model
data Shoal = S (Seq Int) (Int, Int) deriving (Show)

-- Not very pretty but works
initShoal prog = toShoal $ map (\x -> (length x) - 1) $ group . sort $ concat [[0..8],prog]
  where
    toShoal l = S (fromList $ take 7 l) (pair $ drop 7 l)
    pair (x : y : _) = (x, y)

-- Helpers
getAt shoal day = head $ drop day (step shoal)
sumS (S seq (x, y)) = (sum seq) + (x + y)

-- Infinite list
step state = state : step (next state)
   where
    next (S (x :<| rest) (s, e)) = S (rest |> (x + s)) (e, x)

-- parser
num = (\x -> read x :: Int) <$> many1 digit 
cmds = sepBy num (char ',')
