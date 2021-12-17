module AOC(module Prelude, module AOC, module Text.Parsec, module Text.Parsec.String , module Data.List) where

import Data.List
import Text.Parsec hiding (uncons)
import Text.Parsec.String (Parser)

getInputRaw path = readFile path
getInputLines path = lines <$> getInputRaw path
getInputInts path = map (\x -> read x ::Int) <$> getInputLines path

bool2int True = 1
bool2int False = 0

-- Convert integer to binary string (with padding) and reverse
num2bin l n = padWith 0 l $ num2bin' [] n
num2bin' a 0 = reverse a
num2bin' a n = num2bin' ((rem n 2) : a) (div n 2)

bin2num x = bin2num' 1 0 $ reverse x
bin2num' _ a [] = a
bin2num' n a (x:xs) = bin2num' (n * 2) (a + (x * n)) xs

-- Padding for a string
padWith c l x = if (length x) >= l then x else padWith c l (c:x)

-- Flip binary string
flipBit :: Int -> Int
flipBit x = case x of
            0 -> 1
            _ -> 0

-- Parse int from char
char2int :: Char -> Int
char2int x = read [x]


hex2bin c = case c of
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
   

getRight (Right a) = a
getRight (Left _) = []
