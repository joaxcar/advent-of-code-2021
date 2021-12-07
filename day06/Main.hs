import Data.List
import Text.Parsec
import Text.Parsec.String (Parser)
import Data.Functor
import Data.Sequence (Seq( (:<|) ), fromList, (|>))

getInputRaw path = readFile path
getInputLines path = lines <$> getInputRaw path

main = do
  input <- getInputRaw "d.txt"
  let prog =  getRight  $ parse cmds "" input
  let hej = init progtoFisht $ map (\x -> (length x) - 1) $ group . sort $ concat [[0..8],prog]
  print $ sumF $ head$ drop 255 (step hej)

sumF (F seq (x, y)) = (sum seq) + (x + y)
toFish l = F (fromList $ take 7 l) (pair $ drop 7 l)
  where pair (x : y : _) = (x, y)

data Fish = F (Seq Int) (Int, Int) deriving (Show)

step (F (x :<| rest) (s, e)) =
   let update = F (rest |> (x + s)) (e, x)
   in update : step update
   

-- parser
num = (\x -> read x :: Int) <$> many1 digit
   
cmds = sepBy num (char ',')

getRight (Right a) = a
getRight (Left _) = []
