{-# LANGUAGE OverloadedStrings #-}
import InputParser

data CubeColor = Red | Green | Blue deriving (Enum, Show, Eq)

data Game = Game
    { gameIndex :: Int
    , gameRounds :: [(CubeColor, Int)] } deriving (Show)

part1 :: [Game] -> Int
part1 games = let
    maxCubeCount col
        | col == Red = 12
        | col == Green = 13
        | col == Blue = 14
        | otherwise = 0
    isValidGame = all (\(col, cnt) -> cnt <= maxCubeCount col) . gameRounds
    validGames = filter isValidGame games
    in sum $ map gameIndex validGames

part2 :: [Game] -> Int
part2 games = sum $ map power games
    where 
        maxByColor col = maximum . map snd . filter (\(c, _) -> col == c) . gameRounds
        power game = product $ map (`maxByColor` game) [Red, Green, Blue]

pShownCubes :: Parser (CubeColor, Int)
pShownCubes = do
    cnt <- number
    _ <- space
    col <- choice 
        [ Red <$ string "red"
        , Green <$ string "green"
        , Blue <$ string "blue" ]
    return (col, cnt)

pInput :: Parser Game
pInput = do
    _ <- string "Game "
    gIdx <- number
    _ <- string ": "
    cubes <- pShownCubes `sepBy` choice [string ", ", string "; "]
    return $ Game gIdx cubes

main :: IO ()
main = do
    input <- parseInputLines 2 pInput
    print $ part1 input
    print $ part2 input

test1 :: IO ()
test1 = do
  input <- parseTestLines 2 1 pInput
  print $ part1 input == 8

test2 :: IO ()
test2 = do
  input <- parseTestLines 2 2 pInput 
  print $ part2 input == 2286