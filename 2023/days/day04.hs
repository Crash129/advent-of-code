{-# LANGUAGE OverloadedStrings #-}
import InputParser
    ( number,
      parseInputLines,
      parseTestLines,
      space,
      string,
      sepBy,
      sepEndBy,
      Parser )
import qualified Data.Set as S
import Text.Megaparsec.Char (hspace1)

countMatchingNumbers :: (S.Set Int, S.Set Int) -> Int
countMatchingNumbers = S.size . uncurry S.intersection

part1 :: [(S.Set Int, S.Set Int)] -> Int
part1 cards = sum $ map (\x -> 2 ^ (x-1)) $ filter (> 0) $ map countMatchingNumbers cards

addToFirstN :: Int -> Int -> [Int] -> [Int]
addToFirstN n x xs = f xs n
  where
    f :: [Int] -> Int -> [Int]
    f [] _ = []
    f (y:ys) 0 = y : ys
    f (y:ys) m = (y + x) : f ys (m - 1)

part2 :: [(S.Set Int, S.Set Int)] -> Int
part2 cards = f cards [1, 1..]
  where
    f :: [(S.Set Int, S.Set Int)] -> [Int] -> Int
    f [] _ = 0
    f (c:cs) (x:xs) = x + f cs (addToFirstN cnt x xs) where cnt = countMatchingNumbers c
    f _ _ = error "this shouldn't happen"


pInput :: Parser (S.Set Int, S.Set Int)
pInput = do
    _ <- string "Card" <* hspace1 <* number <* string ":" <* space
    winning <- S.fromList <$> number `sepEndBy` hspace1
    _ <- string "|" <* hspace1
    foo <- S.fromList <$> number `sepBy` hspace1
    return (winning, foo)

main :: IO ()
main = do
    input <- parseInputLines 4 pInput
    print $ part1 input
    print $ part2 input

test1 :: IO ()
test1 = do
  input <- parseTestLines 4 1 pInput
  print $ part1 input == 13

test2 :: IO ()
test2 = do
  input <- parseTestLines 4 1 pInput 
  print $ part2 input
  print $ part2 input == 30