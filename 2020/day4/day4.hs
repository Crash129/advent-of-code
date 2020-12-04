import Text.ParserCombinators.ReadP
import qualified Data.Map as M
import Data.Char ( isAlphaNum, isDigit, isLower, isHexDigit )

type Passport = M.Map String String

type Field = (String, String)

requiredKeys = [ "byr"
            , "iyr"
            , "eyr"
            , "hgt"
            , "hcl"
            , "ecl"
            , "pid" ]

toInt :: String -> Int
toInt s = read s :: Int

(>=<) :: Int -> Int -> (Int -> Bool)
(>=<) x y z = x <= z && z <= y

parseHeight :: ReadP (Int, String)
parseHeight = do
    h <- munch isDigit
    u <- munch isLower
    return (read h, u)

isHeightValid :: String -> Bool
isHeightValid s = let 
        x = fst $ last $ readP_to_S parseHeight s
        h = fst x
        u = snd x 
        in case u of
            "cm" -> (150 >=< 193) h
            "in" -> (59 >=< 76) h
            _ -> False

parseHairColor :: ReadP ()
parseHairColor = do
    char '#'
    count 6 $ satisfy isHexDigit
    return ()

isHairColorValid :: String -> Bool
isHairColorValid s = not $ null $ readP_to_S parseHairColor s

isFieldValid :: String -> String -> Bool
isFieldValid k v = case k of
    "byr" -> (1920 >=< 2002) $ toInt v
    "iyr" -> (2010 >=< 2020) $ toInt v
    "eyr" -> (2020 >=< 2030) $ toInt v
    "hgt" -> isHeightValid v
    "hcl" -> isHairColorValid v
    "ecl" -> v `elem` words "amb blu brn gry grn hzl oth"
    "pid" -> (length v == 9) && all isDigit v
    "cid" -> True
    _ -> False

hasAllRequiredKeys :: Passport -> Bool
hasAllRequiredKeys p = all ((== True) . (`M.member` p)) requiredKeys

allFieldsValid :: Passport -> Bool
allFieldsValid p = all (==True) $ M.mapWithKey isFieldValid p

parseField :: ReadP Field
parseField = do
    k <- munch1 isAlphaNum
    char ':'
    v <- munch1 (\c -> c /= ' ' && c /= '\n')
    return (k, v)

parsePassport :: ReadP Passport
parsePassport = do 
    x <- sepBy parseField $ char ' ' +++ char '\n'
    return $ M.fromList x

parsePassports :: ReadP [Passport]
parsePassports = do sepBy parsePassport $ count 2 (char '\n')

step1 :: [Passport] -> Int
step1 p = length $ filter (==True) $ map hasAllRequiredKeys p

step2 :: [Passport] -> Int
step2 p = length $ filter (==True) $ map (\p -> hasAllRequiredKeys p && allFieldsValid p) p

main = do
    i <- readFile "input"
    let p = fst $ last $ readP_to_S parsePassports i
    putStrLn $ "Step 1: " ++ show (step1 p)
    putStrLn $ "Step 2: " ++ show (step2 p)