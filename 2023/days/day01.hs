{-# LANGUAGE OverloadedStrings #-}
import InputParser
import Data.Maybe (catMaybes)
import Text.Megaparsec (many, MonadParsec (lookAhead))
import Text.Megaparsec.Char (digitChar, alphaNumChar)

data ParseContext = Digits | Numeral deriving (Show, Eq)

data ParsedInt = ParsedInt
  { parsedContext :: ParseContext
  , parsedValue :: Int
  } deriving (Show)

isParsedFromDigits :: ParsedInt -> Bool
isParsedFromDigits p = parsedContext p == Digits

catParsedFromDigits :: [ParsedInt] -> [Int]
catParsedFromDigits = map parsedValue . filter isParsedFromDigits

part1 :: [[ParsedInt]] -> Int
part1 input = let
    onlyParsedFromDigits = map catParsedFromDigits input
    numbers = map (\xs -> (head xs * 10) + last xs) onlyParsedFromDigits
    in sum numbers

part2 :: [[ParsedInt]] -> Int
part2 input = let
    parsedValues = map (map parsedValue) input
    numbers = map (\xs -> (head xs * 10) + last xs) parsedValues
    in sum numbers

pNumeral :: Parser Int
pNumeral = do 
    digit <- lookAhead $ choice
        [ 1 <$ string "one"
        , 2 <$ string "two"
        , 3 <$ string "three"
        , 4 <$ string "four"
        , 5 <$ string "five"
        , 6 <$ string "six"
        , 7 <$ string "seven"
        , 8 <$ string "eight"
        , 9 <$ string "nine" ]
    _ <- alphaNumChar -- lookAhead and only consume one char afterwards to mitigate overlaps
    return digit

pDigit :: Parser Int
pDigit = do
    d <- digitChar
    return $ read [d]

pInput :: Parser [ParsedInt]
pInput = do
    maybeParsedInts <- many $ choice
        [ Just . ParsedInt Digits <$> pDigit
        , Just . ParsedInt Numeral <$> pNumeral
        , Nothing <$ satisfy isAlpha ]
    return $ catMaybes maybeParsedInts

main :: IO ()
main = do
    input <- parseInputLines 1 pInput
    print $ part1 input
    print $ part2 input

test1 :: IO ()
test1 = do
  input <- parseTestLines 1 1 pInput
  print $ part1 input == 142

test2 :: IO ()
test2 = do
  input <- parseTestLines 1 2 pInput 
  print $ part2 input == 281