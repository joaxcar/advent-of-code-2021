module Main where
import AOC

main = do
  input <- getInputLines "data.txt"
  let res1 =  sum . scoreFails $ map (valid []) $ f input
  let res2 = winner $ sort $ scoreIncomplete $ map (valid []) $ f input
  print (res1, res2)

f = map2 mapper
map2 = map . map

data Opener = OOne | OTwo | OThree | OFour deriving (Show)
data Closer = COne | CTwo | CThree | CFour deriving (Show)
data Action = A Opener | B Closer deriving (Show)
data Outcome =  Success | Incomplete [Opener] | Fail Closer
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

scoreFails = map score1
scoreIncomplete l = map scoreIt $ filter isIncomplete l
  where isIncomplete (Incomplete _) = True
        isIncomplete _ = False 
        scoreIt (Incomplete x) = scorer 0 x
        scorer n [] = n
        scorer n (x:xs) = scorer (n*5 + score2 x) xs 
winner l = head $ drop (div (length l) 2) l 

score1 (Fail COne) = 3
score1 (Fail CTwo) = 57
score1 (Fail CThree) = 1197
score1 (Fail CFour) = 25137
score1 _ = 0

score2 OOne = 1
score2 OTwo = 2
score2 OThree = 3
score2 OFour = 4

valid [] [] = Success
valid l [] = Incomplete l
valid l (A x : xs) = valid (x:l) xs 
valid (t:ts) (B x : xs) = if match t x then valid ts xs else Fail x


match OOne COne = True
match OTwo CTwo = True
match OThree CThree = True
match OFour CFour = True
match _ _ = False
