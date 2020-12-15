import Common ( readInput )
import Data.List ( inits, tails )

hasSum :: [Int] -> Int -> Bool
hasSum a s = not $ null [True | x <- a, y <- a, x + y == s]

step1 :: [Int] -> Int
step1 a = 
    let (preamble, a') = splitAt 25 a
        f prev (x:xs)
            | hasSum prev x = f (x:init prev) xs
            | otherwise = x
    in f preamble a'

step2 :: [Int] -> Int
step2 a = 
    let invalid = step1 a
        subsets = (concatMap inits . tails) $ filter (< invalid) a
        cont = head $ filter ((== invalid) . sum) subsets
    in minimum cont + maximum cont

main = do
    i <- readInput 9
    let x = map read $ lines i
    print $ step1 x
    print $ step2 x