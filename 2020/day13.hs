import Common ( readInput, parse )
import Text.ParserCombinators.ReadP
import Data.Char ( isDigit )

bus :: ReadP Int
bus = do (read <$> munch1 isDigit) +++ (0 <$ char 'x')

buses :: ReadP (Int, [Int])
buses = do
    start <- read <$> munch1 isDigit
    char '\n'
    ids <- sepBy1 bus (char ',')
    return (start, ids)

gcdExt :: Int -> Int -> (Int, Int)
gcdExt _ 0 = (1, 0)
gcdExt a b =
    let (q, r) = quotRem a b
        (s, t) = gcdExt b r
    in  (t, s - q * t)

-- Chinese Remainder Theorem
-- [(Divisor, Remainder)]
crt :: [(Int, Int)] -> Int
crt x = 
    let m = product $ map fst x
        f (d,r) = r * snd (gcdExt d (m `div` d)) * (m `div` d)
        sum' = sum $ map f x
    in sum' `mod` m

step1 :: (Int, [Int]) -> Int
step1 (start, ids) = 
    let ids' = filter (0 /=) ids
        f id = (id * ceiling (fromIntegral start / fromIntegral id), id)
        (departure, id) = minimum $ map f ids'
    in (departure - start) * id

step2 :: (Int, [Int]) -> Int
step2 (_, ids) = 
    let offsets = filter ((/= 0) . fst) $ zip ids [0..]
    in crt (map (\(x,y) -> (x, y * (-1))) offsets)

main = do
    i <- readInput 13
    let p = parse buses i
    print $ step1 p
    print $ step2 p