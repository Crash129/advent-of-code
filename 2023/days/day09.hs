import InputParser
import Text.Megaparsec.Char (hspace1)

diffs :: Num c => [c] -> [c]
diffs xs = zipWith (-) (tail xs) xs

part1 :: [[Int]] -> Int
part1 histories = sum $ map nextValue histories
    where nextValue xs
                | all (== 0) xs = 0
                | otherwise = last xs + nextValue (diffs xs)

part2 :: [[Int]] -> Int
part2 histories = sum $ map nextValue histories
    where nextValue xs
                | all (== 0) xs = 0
                | otherwise = head xs - nextValue (diffs xs)

pInput :: Parser [Int]
pInput = number `sepBy` hspace1

main :: IO ()
main = do
    input <- parseInputLines 9 pInput
    print $ part1 input
    print $ part2 input

test1 :: IO ()
test1 = do
  input <- parseTestLines 9 1 pInput
  let result = part1 input
  print result
  print $ result == 114

test2 :: IO ()
test2 = do
  input <- parseTestLines 9 1 pInput
  let result = part2 input
  print result
  print $ result == 2