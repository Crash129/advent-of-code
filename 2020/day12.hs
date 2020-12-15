import Common ( readInput, parse )
import Text.ParserCombinators.ReadP
import Data.Char ( isDigit )
import Data.Bifunctor (bimap)

data Cardinal = N | E | S | W deriving (Show, Enum)
data Direction = L | R deriving Show
data Action = Move Cardinal | Turn Direction | Forward deriving Show
data Ship = Ship { position :: Position
                 , facing :: Cardinal } 
                 deriving Show

type Position = (Int, Int)
type Waypoint = Position
type Instruction = (Action, Int)

action :: Char -> Action
action c = 
    case c of
        'N' -> Move N
        'E' -> Move E
        'S' -> Move S
        'W' -> Move W
        'L' -> Turn L
        'R' -> Turn R
        'F' -> Forward
        
instruction :: ReadP Instruction
instruction = do
    a <- action <$> get
    v <- read <$> munch1 isDigit
    return (a,v)

instructions :: ReadP [Instruction]
instructions = do sepBy1 instruction (char '\n')

turnLeft :: Cardinal -> Cardinal
turnLeft dir =
    case dir of
        N -> W
        _ -> pred dir

turnRight :: Cardinal -> Cardinal
turnRight dir =
    case dir of
        W -> N
        _ -> succ dir

rotateLeft :: Position -> Position
rotateLeft (n,e) = (e, n * (-1))

rotateRight :: Position -> Position
rotateRight (n,e) = (e * (-1), n)

moveShip :: Ship -> Instruction -> Ship
moveShip (Ship (north, east) facing) (Move c, value) =
    case c of
        N -> Ship (north + value, east) facing
        E -> Ship (north, east + value) facing
        S -> Ship (north - value, east) facing
        W -> Ship (north, east - value) facing
moveShip (Ship pos facing) (Turn d, value) =
    let numTurns = value `div` 90
        turn = case d of
                    L -> turnLeft
                    R -> turnRight
        facing' = iterate turn facing !! numTurns
    in Ship pos facing'
moveShip ship (Forward, value) =
    moveShip ship (Move $ facing ship, value)

moveShipWaypoint :: (Ship, Waypoint) -> Instruction -> (Ship, Waypoint)
moveShipWaypoint (ship, (wn, we)) (Move c, value) =
    case c of
        N -> (ship, (wn + value, we))
        E -> (ship, (wn, we + value))
        S -> (ship, (wn - value, we))
        W -> (ship, (wn, we - value))
moveShipWaypoint (ship, waypoint) (Turn d, value) =
    let numRots = value `div` 90
        rot = case d of
                    L -> rotateLeft
                    R -> rotateRight
        waypoint' = iterate rot waypoint !! numRots
    in (ship, waypoint')
moveShipWaypoint (Ship (north, east) facing, (wn, we)) (Forward, value) =
    let ship = Ship (north + wn * value, east + we * value) facing
        waypoint = (wn, we)
    in (ship, waypoint)
        
manhattan :: Position -> Int
manhattan (a,b) = abs a + abs b

step1 :: [Instruction] -> Int
step1 instr = 
    let ship = foldl moveShip (Ship (0,0) E) instr
    in manhattan $ position ship

step2 :: [Instruction] -> Int
step2 instr = 
    let ship = Ship (0,0) E
        waypoint = (1,10)
        ship' = foldl moveShipWaypoint (ship, waypoint) instr
    in manhattan $ position $ fst ship'

main = do
    i <- readInput 12
    let x = parse instructions i
    print $ step1 x
    print $ step2 x