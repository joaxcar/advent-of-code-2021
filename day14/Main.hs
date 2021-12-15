{-# LANGUAGE OverloadedStrings #-}
module Main where
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Map (Map)
import Data.Char
import qualified Data.Text as T

import AOC hiding (getRight)

main = do
  input <- getInputRaw "data.txt"
  let (str, r) = getRight $ parse cmds "" input
  let pairs = (zip str (drop 1 str))
  let scr = foldr (\x y -> M.insertWith (+) x 1 y) M.empty str
  let mapp = foldr (\x y -> M.insertWith (+) x 1 y) M.empty pairs
  let ps = M.keys mapp
  --print $ step 30 (head $ reverse str) r str
  --print $ (map ((map length) . group . sort )) $ take 4 $ step1 (head $ reverse str2) r str2
  print $ (\x -> (head $ reverse x) - (head x)) $ sort $ map snd $ M.toList $ runner 40 scr r mapp ps

runner 0 s r m p = s
runner n s r m p =
  let newb = foldr (scoreb r m) s p
      --newp = concat $ map (expa r m) p
      newp = concat $ map (expa r m) p
      nemm = foldr (\(pp, mul) m -> M.insertWith (+) pp mul m) M.empty newp
  in (runner (n-1) newb r nemm (M.keys nemm))

scoreb r m p s =
  let ss = r M.! p
      multi = multip m p
  in M.insertWith (+) ss multi s 

getRight (Right a) = a
getRight (Left _) = ("", M.empty)

multip m v = case M.lookup v m of
             Nothing -> 1
             Just a -> a

expa r m (x, y) =
  let nf = r M.! (x, y)
      mul = multip m (x, y)
  in [((x, nf), mul), ((nf, y), mul)]

-- parser
con = do
  ins1 <- letter
  ins2 <- letter
  string " -> "
  res <- letter
  return ((ins1, ins2), res)

cmds = do
  ins <- many1 letter
  string "\n\n"
  rules <- sepBy con newline <* eof
  return (ins, M.fromList rules)
