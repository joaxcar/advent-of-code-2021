import Data.List()
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor
import qualified Data.Map.Strict as Map

getInputRaw path = readFile path
getInputLines path = lines <$> getInputRaw path

main = do
  input <- getInputRaw "data.txt"
  --let prog =  (map char2int) <$> input
  let prog = getRight $ parse cmds "" input
  print $ length $ moreThanOne $ plot Map.empty prog

moreThanOne x = Map.filter (> 1) x

plot m [] = m
plot m (x:xs) = plot newMap xs
  where
        newMap = foldr add m $ expand x
        add x s = Map.insertWith (+) x 1 s

expand (L p1 p2) = expand_ (L p1 p2) [p1, p2]

expand_ (L (x1, y1) (x2, y2)) l = 
   let next = (step x1 x2, step y1 y2) in
   if next == (x2, y2) then l else expand_ (L next (x2, y2)) (next:l)
step n1 n2
   | n1 == n2 = n1
   | n1 < n2 = n1 + 1
   | n1 > n2 = n1 - 1

manhattanOnly = filter manhattan
   where manhattan (L (x1, y1) (x2, y2)) = x1 == x2 || y1 == y2


type Point = (Int, Int)
data Line = L Point Point deriving (Show, Eq)

-- parser
num = (\x -> read x :: Int) <$> many1 digit


point = do
   x <- num
   char ','
   y <- num
   return (x, y)

line = do
   p1 <- point
   string " -> "
   p2 <- point
   return $ L p1 p2
   
cmds = sepBy line newline

getRight (Right a) = a
getRight (Left _) = []
