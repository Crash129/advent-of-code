import Text.ParserCombinators.ReadP
import Data.Char

data Policy = 
    Policy Int Int Char String
    deriving (Show)

occurrences :: Char -> String -> Int
occurrences x str = 
    length $ filter (== x) str

isBetween :: Int -> Int -> Int -> Bool
isBetween x y z = 
    x <= y && y <= z

isCharAtPos :: String -> Int -> Char -> Bool
isCharAtPos str i c = 
    length str > i && (str !! i) == c

step1 :: Policy -> Bool
step1 (Policy x y c str) = 
    isBetween x (occurrences c str) y

step2 :: Policy -> Bool
step2 (Policy x y c str) = 
    isCharAtPos str x c /= isCharAtPos str y c

countValid :: [Policy] -> (Policy -> Bool) -> Int
countValid ps f = 
    length $ filter f ps

policyParser :: ReadP Policy
policyParser = do
    x <- read <$> many1 (satisfy isDigit)
    char '-'
    y <- read <$> many1 (satisfy isDigit)
    skipSpaces
    c <- get
    char ':'
    skipSpaces
    str <- many1 $ satisfy isAlphaNum
    return $ Policy x y c str

parsePolicy :: String -> Policy
parsePolicy str = 
    fst $ last $ readP_to_S policyParser str

readPolicies :: String -> [Policy]
readPolicies str =
    map parsePolicy $ lines str

main = do
    i <- readFile "input"
    putStrLn $ "Step1: " ++ show (countValid (readPolicies i) step1)
    putStrLn $ "Step2: " ++ show (countValid (readPolicies i) step2)
