import qualified Data.Set as S
import Data.List ( elemIndices )

data Map = Map { width :: Int
               , trees :: [S.Set Int]
               } deriving (Show)

data Angle = Angle { right :: Int
                   , down :: Int
                   } deriving (Show)

takeEvery :: Int -> [a] -> [a]
takeEvery n x = map snd $ filter (\x -> fst x `mod` n == 0) $ zip [0..] x

solve :: Map -> Angle -> Int
solve m a =
   let 
       x = zip [1..] $ tail $ takeEvery (down a) (trees m)
       f acc (i,t) = if S.member (right a * i `mod` width m) t
           then acc + 1 
           else acc
    in foldl f 0 x

step1 :: Map -> Int
step1 s = solve s (Angle 3 1)

step2 :: Map -> Int
step2 s = product $ map (solve s) [Angle 1 1, Angle 3 1, Angle 5 1, Angle 7 1, Angle 1 2]

findTreeIndices :: String -> S.Set Int
findTreeIndices l = S.fromDistinctAscList $ elemIndices '#' l

readInput :: String -> Map
readInput i =
    let 
        l = lines i
        w = length $ head l
        t = map findTreeIndices l
    in Map w t

main = do
    i <- readFile "input"
    let m = readInput i
    putStrLn $ "Step1: " ++ show (step1 m)
    putStrLn $ "Step2: " ++ show (step2 m)