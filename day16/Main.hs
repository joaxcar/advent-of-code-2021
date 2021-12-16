module Main where
import Data.List
import qualified Data.Map as M
import Data.Map (Map)

import AOC

data Packet = L {ver :: Int, pid :: Int, val :: Int} | O {ver :: Int, pid :: Int, op :: String, sub :: [Packet]} | NotP deriving (Show)

main = do
  input <- getInputRaw "data.txt"
  let test = concatMap toBin input
  let prog = parse packet "" test
  print prog

-- part 1 math
sumver (O v _ _ s) = v + (sum $ map sumver $ s)  
sumver (L v _ _ ) = v
-- part2 math
sumV [] = 0
sumV (x:xs) = val x + sumV xs
prodV [] = 1
prodV (x:xs) = val x * prodV xs
maxV p = head . reverse . sort $ map val p
minV p = head . sort $ map val p
gtV (v1:v2:[]) = if val v1 > val v2 then 1 else 0
ltV (v1:v2:[]) = if val v1 < val v2 then 1 else 0
eqV (v1:v2:[]) = if val v1 == val v2 then 1 else 0

-- parser
num = read :: String -> Int
bit = oneOf "01"
nib = count 3 bit
byte = count 4 bit

packet = try litteral 
      <|> try summer
      <|> try multer
      <|> try minner
      <|> try maxer
      <|> try greater
      <|> try lesser
      <|> equeller

litteral = do
  ver <- pure binStr2num <*> nib
  pid <- pure binStr2num <*> string "100"
  num <- number
  return (L ver pid (binStr2num num))
  
number = do
  i <- bit
  case i of
    '0' -> do
      part <- byte
      return part
    '1' -> do
      part <- byte
      rest <- number
      return (part ++ rest)

summer = do
  ver <- pure binStr2num <*> nib
  pid <- pure binStr2num <*> string "000"
  packets <- typeOnePack <|> typeTwoPack
  return (L ver pid (sumV packets))

multer = do
  ver <- pure binStr2num <*> nib
  pid <- pure binStr2num <*> string "001"
  packets <- typeOnePack <|> typeTwoPack
  return (L ver pid (prodV packets))

minner = do
  ver <- pure binStr2num <*> nib
  pid <- pure binStr2num <*> string "010"
  packets <- typeOnePack <|> typeTwoPack
  return (L ver pid (minV packets))

maxer = do
  ver <- pure binStr2num <*> nib
  pid <- pure binStr2num <*> string "011"
  packets <- typeOnePack <|> typeTwoPack
  return (L ver pid (maxV packets))

greater = do
  ver <- pure binStr2num <*> nib
  pid <- pure binStr2num <*> string "101"
  packets <- typeOnePack <|> typeTwoPack
  return (L ver pid (gtV packets))

lesser = do
  ver <- pure binStr2num <*> nib
  pid <- pure binStr2num <*> string "110"
  packets <- typeOnePack <|> typeTwoPack
  return (L ver pid (ltV packets))

equeller = do
  ver <- pure binStr2num <*> nib
  pid <- pure binStr2num <*> string "111"
  packets <- typeOnePack <|> typeTwoPack
  return (L ver pid (eqV packets))

typeOnePack = do
  char '0'
  packLen <- pure binStr2num <*> count 15 bit
  packetPart <- count packLen bit
  return $ getRight $ parse (many1 packet) "" packetPart

typeTwoPack = do
  char '1'
  packs <- pure binStr2num <*> count 11 bit
  packets <- count packs packet
  return packets

getRight2 (Left _) = NotP
getRight2 (Right a) = a

toBin c = case c of
   '0' -> "0000"
   '1' -> "0001"
   '2' -> "0010"
   '3' -> "0011"
   '4' -> "0100"
   '5' -> "0101"
   '6' -> "0110"
   '7' -> "0111"
   '8' -> "1000"
   '9' -> "1001"
   'A' -> "1010"
   'B' -> "1011"
   'C' -> "1100"
   'D' -> "1101"
   'E' -> "1110"
   'F' -> "1111"
