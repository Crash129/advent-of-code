import Text.ParserCombinators.ReadP
import Data.List (maximumBy, sort)
import Data.Function (on)

type Seat = (String, String)
type Bounds = (Int, Int)

getSeatID :: Seat -> Int
getSeatID (row, col) = 
    let r = binaryPartition 'B' row
        c = binaryPartition 'R' col
    in r * 8 + c

lowerHalf :: Bounds -> Bounds
lowerHalf (l,u) = (l, u  - floor ((fromIntegral u - fromIntegral l) / 2)) 

upperHalf :: Bounds -> Bounds
upperHalf (l,u) = (l + ceiling ((fromIntegral u - fromIntegral l) / 2), u) 

binaryPartition :: Char -> String -> Int
binaryPartition uc s = 
    let m = 2 ^ length s - 1
        f b c = if c == uc -- upper half character
            then upperHalf b
            else lowerHalf b
    in fst $ foldl f (0, m) s

seatParser :: ReadP Seat
seatParser = do
    r <- count 7 $ char 'F' +++ char 'B'
    c <- count 3 $ char 'L' +++ char 'R'
    return (r, c)

seatsParser :: ReadP [Seat]
seatsParser = do 
    sepBy seatParser (char '\n')

parse :: ReadP a -> String -> a
parse r s = fst $ last $ readP_to_S r s

step1 :: [Seat] -> Int
step1 s = getSeatID $ maximumBy (flip compare `on` fst) s

step2 :: [Seat] -> Int
step2 s =
    let s' = sort $ map getSeatID s
        f acc x = if x == (acc + 1) then x else acc
    in 1 + foldl1 f s'

main = do
    i <- readFile "input"
    putStrLn $ "Step1: " ++ show (step1 $ parse seatsParser i)
    putStrLn $ "Step2: " ++ show (step2 $ parse seatsParser i)