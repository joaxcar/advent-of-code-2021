import qualified Control.Monad.State as S
import Text.Parsec
import Text.Parsec.String (Parser)

main = do
  input <- readFile "test/test.txt"
  let prog = getRight $ parse cmds "" input
  let res1 = solution $ S.evalState (moveShip prog) (ShipState rules1 (Ship (0, 0) 0))
  let res2 = solution $ S.evalState (moveShip prog) (ShipState rules2 (Ship (0, 0) 0))
  print [res1, res2]

solution (x, y) = x * y

-- Domain model
data Op = Down | Up | Forward deriving (Show, Eq, Ord)
data Cmd = Cmd {op :: Op, val :: Int} deriving (Show, Eq, Ord)
data Ship = Ship {pos :: (Int, Int), aim :: Int}  deriving (Show, Eq, Ord)
type Rule = Ship -> Cmd-> Ship
data Rules = Rules {down :: Rule, up :: Rule, forward :: Rule} 
data ShipState = ShipState Rules Ship

-- business logic
rules1 = Rules 
    (\(Ship (x, y) aim) (Cmd Down n) -> (Ship (x, y + n) aim))
    (\(Ship (x, y) aim) (Cmd Up n) -> (Ship (x, y - n) aim))
    (\(Ship (x, y) aim) (Cmd Forward n) -> (Ship (x + n, y) aim))

rules2 = Rules 
    (\(Ship pos aim) (Cmd Down n) -> (Ship pos (aim + n)))
    (\(Ship pos aim) (Cmd Up n) -> (Ship pos (aim - n)))
    (\(Ship (x, y) aim) (Cmd Forward n) -> (Ship (x + n, y + n * aim) aim))

moveShip :: [Cmd] -> S.State ShipState (Int, Int)
moveShip [] = do
  ShipState _ (Ship pos aim) <- S.get
  return pos
moveShip (cmd : rest) = do
  ShipState rules ship <- S.get
  case op cmd of
    Down -> S.put $ ShipState rules $ (down rules) ship cmd
    Up -> S.put $ ShipState rules $ (up rules) ship cmd
    Forward -> S.put $ ShipState rules $ (forward rules) ship cmd
  moveShip rest

-- parser
command = do
  op <- opType <$> many1 letter
  char ' '
  amount <- read <$> many1 digit
  return (Cmd op amount)

cmds = sepBy command (char '\n') <* eof

opType t
  | t == "down" = Down
  | t == "up"= Up
  | t == "forward"= Forward

getRight (Right a) = a
getRight (Left _) = []
