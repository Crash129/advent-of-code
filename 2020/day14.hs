import Common ( readInput, parse )
import Text.ParserCombinators.ReadP
import Data.Char ( isAlphaNum, isDigit )
import Data.Bits
import qualified Data.Map as M

type Mask = Int -> Int
type Assignment = (Int, Int)
type Program = [Assignment]

readMask :: String -> Mask
readMask m = 
    let m' = filter ((/=) 'X' . snd) $ zip [0..] $ reverse m
        f (i,x) = case x of
                '0' -> flip clearBit i
                '1' -> flip setBit i
        in foldl (.) id $ map f m'

assignment :: Mask -> ReadP Assignment
assignment m = do 
    string "mem["
    addr <- read <$> munch1 isDigit
    string "] = "
    val <- read <$> munch1 isDigit
    return (addr, m val)

assignments :: ReadP Program
assignments = do
    string "mask = "
    mask <- readMask <$> munch1 isAlphaNum
    char '\n'
    let assignment' = assignment mask
    sepBy1 assignment' (char '\n')

program :: ReadP Program
program = do
    as <- sepBy1 assignments (char '\n')
    return $ concat as

run :: Program -> M.Map Int Int
run p =
    let f m [] = m
        f m (a:as) = f (uncurry M.insert a m) as 
    in f M.empty p

step1 :: Program -> Int
step1 p = M.foldl (+) 0 $ run p

main = do
    i <- readInput 14
    let p = parse program i
    print $ step1 p
    -- print $ step2 p
