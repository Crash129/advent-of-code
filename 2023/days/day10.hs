import InputParser
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Maybe (fromJust)

getConnectedPositions :: Pos -> Char -> [Pos]
getConnectedPositions (posX, posY) tile
    | tile == '|' = [(posX, posY-1), (posX, posY+1)]
    | tile == '-' = [(posX-1, posY), (posX+1, posY)]
    | tile == 'L' = [(posX+1, posY), (posX, posY-1)]
    | tile == 'J' = [(posX-1, posY), (posX, posY-1)]
    | tile == '7' = [(posX-1, posY), (posX, posY+1)]
    | tile == 'F' = [(posX+1, posY), (posX, posY+1)]
    | tile == 'S' = [(posX+1, posY), (posX-1, posY), (posX, posY+1), (posX, posY-1)]
    | otherwise = []

getLargestLoopSize :: Positions -> Int
getLargestLoopSize poses =
    let adj = map2D getConnectedPositions poses
        start = fromJust $ find2D 'S' poses
        loop visited curNode
            | not (null visited) && curNode == start = S.size visited
            | curNode `S.member` visited = 0
            | otherwise = 
                case (adj V.! y) V.! x of
                    [] -> 0
                    nextNodes -> maximum $ map (loop $ S.insert curNode visited) nextNodes
                where (x,y) = curNode
    in loop S.empty start

part1 :: Positions -> Int
part1 = (`div` 2) . getLargestLoopSize

part2 :: Positions -> Int
part2 _ = 0

main :: IO ()
main = do
    input <- parseInput2D 10
    print $ part1 input
    print $ part2 input

test1 :: IO ()
test1 = do
  input <- parseTest2D 10 1
  let result = part1 input
  print result
  print $ result == 4

test2 :: IO ()
test2 = do
  input <- parseTest2D 10 1
  let result = part2 input
  print result
  print $ result == 0