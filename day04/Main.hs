import AOC
import Data.Functor

getInputRaw path = readFile path
getInputLines path = lines <$> getInputRaw path

main = do
  input <- getInputRaw "test/data.txt"
  --let prog =  (map char2int) <$> input
  let prog = getRight $ parse cmds "" input
  print $ run2 prog

data State = S {calls :: [Int], boards :: [[[Int]]]} deriving Show

run (S [] _) = 0
run (S (x:xs) boards) = 
  let a = callOut x boards
      winner = bingo a
  in case winner of
     [] -> run (S xs a)
     [board] -> score x board

run2 (S [] _) = 0
run2 (S (x:xs) boards) = 
  let a = callOut x boards
      loosers = nobingo a
  in case loosers of
     [board] ->  run $ S xs [board]
     h -> run2 $ S xs h

callOut x bs = map ( map (filter (\y -> y /= x))) bs
bingo bs = filter (\y -> (filter (\x -> x == []) y) /= []) bs   
nobingo bs = filter (\y -> (filter (\x -> x == []) y) == []) bs   
score x b = (x * sum (concat b)) `div` 2

-- parser
num = \x -> read x :: Int
grid = many1 numRow
numRow = do
   hej <- many1 numPart
   optional newline
   return hej
numPart = do
   optional (many (char ' '))
   row <- num <$> many1 digit
   return row
cmds = do
  steps <- sepBy (num <$> many1 digit) (char ',')
  count 2 newline
  grids <- sepBy grid newline <* eof
  return $ S steps (map (\x -> concat [x , transpose x]) grids)

getRight :: Either ParseError Main.State -> Main.State
getRight (Right a) = a
getRight (Left _) = S [] [[[]]]
