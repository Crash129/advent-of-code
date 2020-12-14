{-# LANGUAGE TupleSections #-}

import Common ( readInput, parse )
import Text.ParserCombinators.ReadP
import Data.Char ( isAlphaNum, isDigit )
import Data.Bits
import qualified Data.Map as M
import Data.Bifunctor ( Bifunctor(first, second) )

type Mask = String
type Assignment = (Int, Int)
type Block = (Mask, [Assignment])
type Program = [Block]

assignment :: ReadP Assignment
assignment = do 
    string "mem["
    addr <- read <$> munch1 isDigit
    string "] = "
    val <- read <$> munch1 isDigit
    return (addr, val)

block :: ReadP Block
block = do
    string "mask = "
    mask <- munch1 isAlphaNum
    char '\n'
    as <- sepBy1 assignment (char '\n')
    return (mask, as)

program :: ReadP Program
program = do sepBy1 block (char '\n')

valueMaskFn :: Mask -> (Int -> Int)
valueMaskFn m = 
    let m' = filter ((/=) 'X' . snd) $ zip [0..] $ reverse m
        f (i,x) = case x of
                '0' -> flip clearBit i
                '1' -> flip setBit i
        in foldl (.) id $ map f m'

addrMaskFn :: String -> (Int -> [Int])
addrMaskFn mask = 
    let indexed = filter ((/=) '0' . fst) $ zip (reverse mask) [0..]
        f [] a = [a]
        f ((m,i):xs) a = 
            case m of
                '0' -> f xs a
                '1' -> f xs (setBit a i)
                'X' -> f xs (setBit a i) ++ f xs (clearBit a i)
    in f indexed

maskValues :: Block -> Block
maskValues (mask, as) = 
    let f = valueMaskFn mask
        as' = map (second f) as
    in  (mask, as')

maskAddrs :: Block -> Block
maskAddrs (mask, as) = 
    let f = addrMaskFn mask
        -- as' = [([Addrs], Value)]
        as' = map (first f) as
        as'' = concatMap (\(addrs, val) -> map (, val) addrs) as'
    in (mask, as'')

transform :: Program -> (Block -> Block) -> [Assignment]
transform p f = concatMap (snd . f) p

run :: [Assignment] -> M.Map Int Int
run as = 
    let f m [] = m
        f m (a:as) = f (uncurry M.insert a m) as 
    in f M.empty as 

step1 :: Program -> Int
step1 p =
    let mem = run $ transform p maskValues
    in M.foldl (+) 0 mem

step2 :: Program -> Int
step2 p =
    let mem = run $ transform p maskAddrs
    in M.foldl (+) 0 mem

main = do
    i <- readInput 14
    let p = parse program i
    print $ step1 p
    print $ step2 p
