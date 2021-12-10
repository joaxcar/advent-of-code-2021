module Main where
import AOC
import qualified Data.Vector as V
import Data.Function (on)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Sequence as S 

main = do
  input <- getInputLines "data.txt"
  let res1 = sum $ map (score . snd) $ filter (res) $ map (valid []) $ f input
  let res2 = winner $ sort $ map ((scorer 0) . snd)$ filter (fst) $ map (valid2 []) $ f input
  print (res1, res2)

f = map2 mapper
map2 = map . map

data Opener = OOne | OTwo | OThree | OFour deriving (Show)
data Closer = COne | CTwo | CThree | CFour deriving (Show)
data Action = A Opener | B Closer deriving (Show)
data Outcome =  Success | Incomplete [Opener] | Fail Closer
res (Fail x) = Fa
mapper :: Char -> Action
mapper x = case x of
  '(' -> A OOne
  '[' -> A OTwo
  '{' -> A OThree
  '<' -> A OFour
  ')' -> B COne
  ']' -> B CTwo
  '}' -> B CThree
  '>' -> B CFour

score COne = 3
score CTwo = 57
score CThree = 1197
score CFour = 25137

score2 OOne = 1
score2 OTwo = 2
score2 OThree = 3
score2 OFour = 4

scorer n [] = n
scorer n (x:xs) = scorer (n*5 + score2 x) xs 

winner l = head $ drop (div (length l) 2) l 

valid [] [] = Success
valid l [] = Incomplete l
valid l (A x : xs) = valid (x:l) xs 
valid (t:ts) (B x : xs) = if match t x then valid ts xs else Fail x

valid2 l [] = (True, l)
valid2 l (A x : xs) = valid2 (x:l) xs 
valid2 (t:ts) (B x : xs) = if match t x then valid2 ts xs else (False, [])

match OOne COne = True
match OTwo CTwo = True
match OThree CThree = True
match OFour CFour = True
match _ _ = False
