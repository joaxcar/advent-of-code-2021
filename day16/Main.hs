module Main where
import Data.List
import qualified Data.Map as M
import Data.Map (Map)

import AOC 

main = do
  input <- getInputRaw "data.txt"
  let test = concatMap toBin input
  let prog = getRight2 $ parse packet "" test
  print $ (sumver prog, eval prog)

-- part 1 math
sumver (O v _ _ s) = v + (sum $ map sumver $ s)  
sumver (L v _ _ ) = v

-- part 2
eval (L _ _ v) = v
eval (O _ _ f s) = f (map eval s)

data Packet = L {ver :: Int, pid :: Int, val :: Int} | O {ver :: Int, pid :: Int, op :: [Int] -> Int, sub :: [Packet]} | NotP 

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
  parts <- many (char '1' *> byte)
  end <- (char '0' *> byte)
  return (L ver pid (binStr2num ((concat parts) ++ end)))
  
summer = operator "000" (foldr (+) 0)
multer = operator "001" (foldr (*) 1)
minner = operator "010" (head . sort)
maxer = operator "011" (head . reverse . sort)
greater = operator "101" (\ (v1:v2:[]) -> if v1 > v2 then 1 else 0)
lesser = operator "110" (\ (v1:v2:[]) -> if v1 < v2 then 1 else 0)
equeller = operator "111" (\ (v1:v2:[]) -> if v1 == v2 then 1 else 0)

operator pid f = do
  ver <- pure binStr2num <*> nib
  pid <- pure binStr2num <*> string pid
  packets <- typeOnePack <|> typeTwoPack
  return (O ver pid f packets)

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
