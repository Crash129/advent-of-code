import InputParser
import Data.List

countIncreases xs =
    length $ filter (uncurry (>)) $ zip (tail xs) xs

part1 = countIncreases

{- 
> tails [1..6]
[[1,2,3,4,5,6],[2,3,4,5,6],[3,4,5,6],[4,5,6],[5,6],[6],[]]

> transpose $ take 3 $ tails [1..6]
[[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6],[6]] 
-}

part2 measures =
    let windows n xs = takeWhile ((==) n . length) $ transpose $ take n $ tails xs
        sums = map sum $ windows 3 measures
    in countIncreases sums

main = do
    measures <- parseInputLines 1 number
    print $ part1 measures
    print $ part2 measures

test1 = do
  measures <- parseTestLines 1 1 number 
  print $ part1 measures == 7

test2 = do
  measures <- parseTestLines 1 1 number 
  print $ part2 measures == 5