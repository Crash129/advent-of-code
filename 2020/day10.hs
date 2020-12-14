import Common ( readInput, parse )
import Text.ParserCombinators.ReadP
import Data.Char ( isDigit )
import Data.List ( sort )

adapter :: ReadP Int
adapter = do 
    j <- munch1 isDigit
    return $ read j

adapters :: ReadP [Int]
adapters = do
    js <- sepBy adapter (char '\n')
    return $ sort js

step1 :: [Int] -> Int
step1 as = let
    maxAdapter = maximum as
    as' = [0] ++ as ++ [maxAdapter + 3]
    diffs = zipWith (flip (-)) as' (tail as')
    countSatisfy f = length . filter f
    in countSatisfy (== 1) diffs * countSatisfy (== 3) diffs

main = do
    i <- readInput 10
    let p = parse adapters i
    print $ step1 p
    -- print $ step2 p