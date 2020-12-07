import Text.ParserCombinators.ReadP
import Data.Char(isAlpha, isDigit)
import qualified Data.Map as M

-- TODO cache partial results in countGold / countBags

type Content = (Int, String)
type Rule = (String, [Content])

word :: ReadP String
word = do 
    x <- munch1 isAlpha
    skipSpaces
    return x

bag :: ReadP String
bag = do 
    x <- count 2 word
    return $ unwords x

content :: ReadP Content
content = do
    skipSpaces
    n <- munch1 isDigit
    skipSpaces
    b <- bag
    string "bag"
    optional $ char 's'
    return (read n, b)

rule :: ReadP Rule
rule = do
    b <- bag
    string "bags contain"
    r <- sepBy content (char ',') +++ ([] <$ string " no other bags")
    char '.'
    return (b, r)

rules :: ReadP (M.Map String [Content])
rules = do
    r <- sepBy1 rule (char '\n')
    return $ M.fromList r

parse :: ReadP a -> String -> a
parse r s = fst $ last $ readP_to_S r s

countGold :: M.Map String [Content] -> String -> Int
countGold _ "shiny gold" = 0
countGold m b = let
    f [] = 0
    f [(n,"shiny gold")] = n
    f [(n,b)] = n * countGold m b
    f ((n,"shiny gold"):xs) = f xs + n
    f ((n,b):xs) = f xs + n * countGold m b
    in case M.lookup b m of
        Just x -> f x
        _ -> error $ "bag " ++ b ++ " not found"

countBags :: M.Map String [Content] -> String -> Int
countBags m b = let
    f [] = 1
    f [(n,b)] = 1 + n * countBags m b
    f ((n,b):xs) = f xs + n * countBags m b
    in case M.lookup b m of
        Just x -> f x
        _ -> error $ "bag " ++ b ++ " not found"

step1:: M.Map String [Content] -> Int
step1 m = let
    f acc b _ = acc + if countGold m b > 0 then 1 else 0
    in M.foldlWithKey f 0 m

step2:: M.Map String [Content] -> Int
step2 m = countBags m "shiny gold" - 1

main = do
    i <- readFile "input"
    let r = parse rules i
    print $ step1 r
    print $ step2 r