import Data.List
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor
import Data.Function (on)
import Data.List (sortBy)

getInputRaw path = readFile path
getInputLines path = lines <$> getInputRaw path

main = do
  input <- getInputRaw "test.txt"
  let prog =  getRight  $ parse cmds "" input
  let r = range prog
  print $ head . sort $ map (dist prog) r


num = (\x -> read x :: Int) <$> many1 digit
   
range :: [Int] -> [Int]
range l = let n1 = foldr1 min l
              n2 = foldr1 max l
          in [n1..n2]
dist l n = sum $ map (dp2 n) l 
dp x y = if x < y then y - x else x - y 
dp2 x y = if x < y then pp (y - x) 0 else pp (x - y) 0
  where pp 0 t = t
        pp n t = pp (n-1) (t+n)
-- parser 
cmds = sepBy num (char ',')

getRight (Right a) = a
getRight (Left _) = []
