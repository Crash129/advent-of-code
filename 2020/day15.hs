import Common ( readInput, parse )
import Data.Char ( isDigit )
import Text.ParserCombinators.ReadP
import qualified Data.Map.Strict as M

memory :: [Int] -> Int -> Int
memory start x =
    let f turn seen (n:ns) =
            case ns of
                [] -> g turn seen n
                _ -> f (turn + 1) (M.insert n turn seen) ns
        g turn seen n 
            | turn == x = n
            | otherwise = g (turn + 1) (M.insert n turn seen) $ maybe 0 (turn -) (M.lookup n seen)
    in f 1 M.empty start

step1 :: [Int] -> Int
step1 start = memory start 2020

step2 :: [Int] -> Int
step2 start = memory start 30000000

main = do
    i <- readInput 15
    let start = parse (do sepBy1 (read <$> munch1 isDigit) (char ',')) i
    print $ step1 start
    print $ step2 start