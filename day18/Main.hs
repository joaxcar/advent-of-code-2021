module Main where
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Map (Map)
import Data.Char
import qualified Data.Text as T

import AOC

main = do
  input <- getInputRaw "test.txt"
  let nums = lines input
  --let test = "[[[[6,0],[7,7]],[[6,6],[[16,17],0]]],[3,9]]"
  let test = "[[[[6,0],[5,[111,232]]],[[6,6],[[16,17],0]]],[3,9]]"
  print $ solve $ lines input
  let pairs =concat $ [[[x,y], [y,x]] | (x:ys) <- tails nums, y <- ys]
  print $ maximum $ map solve pairs

solve l = countS $ getRight2 $ parse squid1 "" $ foldl1 (\x y->runner turn $ addS x y) l
getRight2 (Right a) = a
getRight2 _ = S (L 1) (L 3)
turn l = run [] $ runner (pair 0 []) l
runner f x = if f x == x then x else runner f (f x)

pair _ r [] = reverse r
pair 4 r ('[':xs) = expload r xs
pair 4 r (n1:',':'[':xs) | elem n1 numlist= expload (',':n1:r) xs
pair 4 r (n1:n2:',':'[':xs) | elem n1 numlist && elem n2 numlist = expload (',':n2:n1:r) xs
pair 4 r (n1:n2:n3:',':'[':xs) | elem n1 numlist && elem n2 numlist && elem n3 numlist = expload (',':n3:n2:n1:r) xs
pair n r ('[':xs) = pair (n+1) ('[':r) xs
pair n r (']':xs) = pair (n-1) (']':r) xs
pair n r (x:xs) = pair n (x:r) xs

run r [] = reverse r
run r (x:y:z:xs) | elem x numlist && elem y numlist && elem z numlist = concat [reverse r, split [x,y,z], xs]
run r (x:y:xs) | elem x numlist && elem y numlist = concat [reverse r, split [x,y], xs]

run r (x:xs) = run (x:r) xs

expload r l =
  let (n1, n2, rest) = pick 0 ([], [],l)
      (r1, newn1, rest1) = up 0 ([],n1, r)
      (r2, newn2, rest2) = up 1 ([],n2, rest)
  in concat [
    reverse rest1,
    newn1,
    r1,
    "0",
    reverse r2,
    newn2,
    rest2
    ]
      
up d (r, n, (x:rest)) | not (elem x numlist) = up d ((x:r), n, rest)

up 1 (r, n, (x:y:z:rest)) =
  if elem y numlist && elem z numlist
  then (r, addR n (x:y:z:[]), rest)
  else if elem y numlist
  then (r, addR n (x:y:[]), z:rest)
  else (r, addR n [x], y:z:rest)
up 0 (r, n, (x:y:z:rest)) =
  if elem y numlist && elem z numlist
  then (r, addR n (z:y:x:[]), rest)
  else if elem y numlist
  then (r, addR n (y:x:[]), z:rest)
  else (r, addR n [x], y:z:rest) 
up 1 (r, n, (x:y:rest)) =
  if elem y numlist
  then (r, addR n (x:y:[]), rest)
  else (r, addR n [x], y:rest)
up 0 (r, n, (x:y:rest)) =
  if elem y numlist
  then (r, addR n (y:x:[]), rest)
  else (r, addR n [x], y:rest)

up _ (r, n, []) = (r, "", [])
numlist = "0123456789"

pick _ (n1,n2,(']':rest)) = (n1, n2, rest)
pick n (n1, n2, (',':xs)) = pick 1 (n1, n2, xs)
pick 0 (n1, n2, (x:xs)) = pick 0 (n1 ++ [x], n2, xs)
pick 1 (n1, n2, (x:xs)) = pick 1 (n1, n2 ++ [x],xs)

addS n t = concat ["[",n, ",",t,"]"]

addR n t = 
  let n1 = read n :: Int
      n2 = read t :: Int 
  in show (n1 + n2) 
split n =
  let n1 = read n :: Int
      res = [div n1 2, div n1 2 + rem n1 2]
  in show res
countS (L a) = a
countS (S a b) = 3 * (countS a) + 2 * (countS b) 
data SquidNum = L Int | S {lft :: SquidNum, rht :: SquidNum} deriving (Show)
rnum = do
  d <- read <$> many1 digit
  return $ L d
open = char '['
close = char ']'
snum = do
  open
  num1 <- rnum <|> snum
  char ','
  num2 <- rnum <|> snum
  close
  return $ (S num1 num2)
squid = sepBy snum newline <* eof
squid1 = snum
