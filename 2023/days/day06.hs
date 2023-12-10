{-# LANGUAGE OverloadedStrings #-}
import InputParser
import Text.Megaparsec (many, some)
import Text.Megaparsec.Char (hspace1, digitChar)

part1 :: [(Int, Int)] -> Int
part1 races = product $ map numPossibleWaitingDurations races

part2 :: (Int, Int) -> Int
part2 = numPossibleWaitingDurations

numPossibleWaitingDurations :: (Int, Int) -> Int
numPossibleWaitingDurations (t, d) = length $ filter (> d) $ map (\x -> x * (t-x)) [1,2..(t-1)]

pInput :: Parser [(Int, Int)]
pInput = do
  _ <- string "Time:" <* hspace
  times <- number `sepBy` hspace1
  _ <- newline <* string "Distance:" <* hspace
  distances <- number `sepBy` hspace1
  return $ zip times distances

pInput2 :: Parser (Int, Int)
pInput2 = do
  _ <- string "Time:" <* hspace
  time <- read . concat <$> many (some digitChar <* hspace)
  _ <- newline <* string "Distance:" <* hspace
  distance <- read . concat <$> many (some digitChar <* hspace)
  return (time, distance)

main :: IO ()
main = do
    input <- parseInput 6 pInput
    print $ part1 input
    input2 <- parseInput 6 pInput2
    print $ part2 input2

test1 :: IO ()
test1 = do
  input <- parseTest 6 1 pInput
  print $ part1 input == 288

test2 :: IO ()
test2 = do
  input <- parseTest 6 1 pInput2
  print $ part2 input