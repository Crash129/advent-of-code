import Common ( readInput, parse )
import Text.ParserCombinators.ReadP
import qualified Data.Set as S

sumFirstN :: Int -> Int
sumFirstN n = (n * (n+1)) `div` 2

sumInterval :: Int -> Int -> Int
sumInterval start end = sumFirstN end - sumFirstN (start - 1)

binaryToInt :: [Bool] -> Int
binaryToInt = foldl (\acc x -> acc * 2 + fromEnum x) 0

seatParser :: ReadP Int
seatParser = do 
    b <- many1 $ 
        (False <$ (char 'F' +++ char 'L')) +++ 
        (True  <$ (char 'B' +++ char 'R'))
    return $ binaryToInt b

seatsParser :: ReadP (S.Set Int)
seatsParser = do 
    s <- sepBy seatParser (char '\n')
    return $ S.fromList s

step1 :: S.Set Int -> Int
step1 = S.findMax

step2 :: S.Set Int -> Int
step2 s =
    let f (min',max',sum') x = (min min' x, max max' x, sum' + x)
        (min', max', sum') = S.foldl f (maxBound::Int, minBound::Int, 0) s
    in sumInterval min' max' - sum'

main = do
    i <- readInput 5
    let seats = parse seatsParser i
    putStrLn $ "Step1: " ++ show (step1 seats)
    putStrLn $ "Step2: " ++ show (step2 seats)