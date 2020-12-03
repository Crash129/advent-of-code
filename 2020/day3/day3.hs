import qualified Data.Set as S
import Data.List ( elemIndices )

takeEvery :: Int -> [a] -> [a]
takeEvery n x = map snd $ filter (\x -> fst x `mod` n == 0) $ zip [0..] x

solve :: (Int, [S.Set Int]) -> Int -> Int -> Int
solve (l, s) r d = 
   let 
       s' = zip [1..] $ tail $ takeEvery d s
       f acc (i,s) = if S.member (r * i `mod` l) s 
           then acc + 1 
           else acc
    in foldl f 0 s'

step1 :: (Int, [S.Set Int]) -> Int
step1 s = solve s 3 1

-- d > 1 not working yet
step2 :: (Int, [S.Set Int]) -> Int
step2 s = sum $ map (uncurry (solve s)) [(1,1), (3,1), (5,1), (7,1), (1,2)]

findTreeIndices :: String -> S.Set Int
findTreeIndices l = S.fromDistinctAscList $ elemIndices '#' l

readInput :: String -> (Int, [S.Set Int])
readInput i =
    let 
        l = lines i
        len = length $ head l
        t = map findTreeIndices l
    in (len, t)

main = do
    i <- readFile "input"
    putStrLn $ "Step1: " ++ show (step1 (readInput i))
    putStrLn $ "Step2: " ++ show (step2 (readInput i))