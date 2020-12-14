import Common ( readInput )
import qualified Data.IntSet as S

step1 :: [Int] -> Int
step1 l = 
    let s = S.fromList l
    in product $ filter (`S.member` s) $ map (2020 -) l

step2 :: [Int] -> Int
step2 l = 
    let s = S.fromList l
        xy = [(2020 - (x + y), x, y) 
            | x <- l, y <- l, y < x, x + y < 2020]
        (x,y,z) = head $ filter (\(xy, _, _) -> S.member xy s) xy
    in x * y * z

main = do
    i <- readInput 1
    let l = map read $ lines i
    print $ step1 l
    print $ step2 l
