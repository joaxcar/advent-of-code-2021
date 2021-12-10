module Main where
import AOC

main = do
  input <- getInputLines "test.txt"
  let res1 =  sum . scoreFails $ map (validate []) $ f input
  let res2 = winner $ sort $ scoreIncomplete $ map (validate []) $ f input
  print (res1, res2)

f = map2 mapper
map2 = map . map

data Symbol = One | Two | Three | Four deriving (Show, Eq)
data Action = Open Symbol | Close Symbol deriving (Show)
data Outcome =  Success | Incomplete [Symbol] | Fail { sym :: Symbol }

mapper :: Char -> Action
mapper x = case x of
  '(' -> Open One
  '[' -> Open Two
  '{' -> Open Three
  '<' -> Open Four
  ')' -> Close One
  ']' -> Close Two
  '}' -> Close Three
  '>' -> Close Four

validate [] [] = Success
validate l [] = Incomplete l
validate l (Open x : xs) = validate (x:l) xs 
validate (t:ts) (Close x : xs) = if t == x then validate ts xs else Fail x

scoreFails l = map (scoreFail . sym) $ filter isFail l
  where
    isFail (Fail _) = True
    isFail _ = False 
    scoreFail s = case s of 
                  One -> 3
                  Two -> 57
                  Three -> 1197
                  Four -> 25137

scoreIncomplete l = map scoreIt $ filter isIncomplete l
  where 
    isIncomplete (Incomplete _) = True
    isIncomplete _ = False 
    scoreIt (Incomplete x) = scorer 0 x
    scorer n [] = n
    scorer n (x:xs) = scorer (n * 5 + scoreSym x) xs 
    scoreSym s = case s of
                 One -> 1
                 Two -> 2
                 Three -> 3
                 Four -> 4

winner l = head $ drop (div (length l) 2) l 
