{-# LANGUAGE OverloadedStrings #-}

import InputParser

data Command 
    = Forward Int
    | Up Int
    | Down Int
  deriving Show

commandParser :: Parser Command
commandParser = do
  cmd <- choice
    [ Forward <$ string "forward"
    , Up <$ string "up"
    , Down <$ string "down" ]
  space
  cmd <$> number

applyCommand :: (Int, Int) -> Command -> (Int, Int)
applyCommand (hor, dep) cmd =
  case cmd of
    Forward x -> (hor + x, dep)
    Up x -> (hor, dep - x)
    Down x -> (hor, dep + x)

applyAimCommand :: (Int, Int, Int) -> Command -> (Int, Int, Int)
applyAimCommand (hor, dep, aim) cmd =
  case cmd of
    Forward x -> (hor + x, dep + aim * x, aim)
    Up x -> (hor, dep, aim - x)
    Down x -> (hor, dep, aim + x)

part1 :: [Command] -> Int
part1 = uncurry (*) . foldl applyCommand (0, 0)

part2 :: [Command] -> Int
part2 = (\(hor, dep, _) -> hor * dep) . foldl applyAimCommand (0, 0, 0)

main :: IO ()
main = do
  cmds <- parseInputLines 2 commandParser
  print $ part1 cmds
  print $ part2 cmds

test1 :: IO ()
test1 = do
  cmds <- parseTestLines 2 1 commandParser
  print $ part1 cmds == 150

test2 :: IO ()
test2 = do
  cmds <- parseTestLines 2 1 commandParser
  print $ part2 cmds == 900