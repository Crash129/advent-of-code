import Common ( readInput, parse )
import Data.Char ( isDigit )
import Text.ParserCombinators.ReadP
import Text.Show.Functions ()

type Rule = Int -> Bool
data Notes = Notes { rules :: [[Rule]]
                   , yourTicket :: [Int]
                   , nearbyTickets :: [[Int]]}
                   deriving (Show)

range :: ReadP Rule
range = do
    a <- read <$> munch1 isDigit
    char '-'
    b <- read <$> munch1 isDigit
    return $ a >=< b

rule :: ReadP [Rule]
rule = do
    skipMany $ satisfy $ not . isDigit
    a <- range
    string " or "
    b <- range
    return [a, b]

csv :: ReadP [Int]
csv = do sepBy1 (read <$> munch1 isDigit) (char ',')

notes :: ReadP Notes
notes = do
    rules <- sepBy1 rule $ char '\n'
    count 2 $ char '\n'
    string "your ticket:\n"
    your <- csv
    count 2 $ char '\n'
    string "nearby tickets:\n"
    nearby <- sepBy1 csv $ char '\n'
    return $ Notes rules your nearby

(>=<) :: Int -> Int -> (Int -> Bool)
(>=<) x y z = x <= z && z <= y

isFieldValid :: [Rule] -> Int -> Bool
isFieldValid rules x = or $ rules <*> pure x

step1 notes =
    let rules' = concat $ rules notes
        nearby = concat $ nearbyTickets notes
        invalid = filter (not . isFieldValid rules') nearby
    in sum invalid

main = do
    i <- readInput 16
    let n = parse notes i
    print $ step1 n