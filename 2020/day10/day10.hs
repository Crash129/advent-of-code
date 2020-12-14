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

parse :: ReadP a -> String -> a
parse r s = fst $ last $ readP_to_S r s

step1 :: [Int] -> Int
step1 as = let
    maxAdapter = maximum as
    as' = [0] ++ as ++ [maxAdapter + 3]
    diffs = zipWith (flip (-)) as' (tail as')
    countSatisfy f = length . filter f
    in countSatisfy (== 1) diffs * countSatisfy (== 3) diffs

main = do
    i <- readFile "input"
    let p = parse adapters i
    print $ step1 p
    -- print $ step2 p