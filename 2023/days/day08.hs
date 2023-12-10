{-# LANGUAGE OverloadedStrings #-}
import InputParser
import qualified Data.Map as M
import Text.Megaparsec (some)
import Text.Megaparsec.Char (alphaNumChar)
import Data.Char (ord)
import Control.Monad.State
import Test.QuickCheck

data Move = MoveLeft | MoveRight

type Key = Int

data NavState = NavState
  { network :: M.Map Key (Key, Key)
  , steps :: Int
  , position :: Key
  }

applyMove :: Move -> State NavState ()
applyMove m = do
  (NavState netw s pos) <- get
  let jumps = case M.lookup pos netw of
        Just a -> a
        Nothing -> error $ "invalid position without defined jumps: " ++ show pos
  let newPos = case m of
        MoveLeft -> fst jumps
        MoveRight -> snd jumps
  put $ NavState netw (s+1) newPos

applyMovesUntil :: (NavState -> Bool) -> [Move] -> State NavState ()
applyMovesUntil cond (m:ms) = do
  applyMove m
  curState <- get
  if cond curState
    then return ()
    else do
      applyMovesUntil cond ms
applyMovesUntil _ _ = error "no moves left"

encodeKey :: [Char] -> Key
encodeKey [a,b,c] = (ord a - ord 'A') * 27^2 + (ord b - ord 'A') * 27 + (ord c - ord 'A')
encodeKey x = error $ "invalid key to hash given: " ++ x

isLastCharOfEncKey :: Char -> Key -> Bool
isLastCharOfEncKey x k = k `mod` 27 == (ord x - ord 'A')

part1 :: ([Move], M.Map Key (Key, Key)) -> Key
part1 (ins, net) = steps $ execState (applyMovesUntil ((== encodeKey "ZZZ") . position) (cycle ins)) (NavState net 0 $ encodeKey "AAA")

part2 :: ([Move], M.Map Key (Key, Key)) -> Key
part2 (ins, net) = foldl1 lcm $ map getStepsToFinish starts
  where starts = filter (isLastCharOfEncKey 'A') $ M.keys net
        getStepsToFinish start = steps $ execState (applyMovesUntil (isLastCharOfEncKey 'Z' . position) (cycle ins)) (NavState net 0 start)

pNetwork :: Parser (Key, (Key, Key))
pNetwork = do
    node <- some alphaNumChar
    _ <- string " = ("
    left <- some alphaNumChar
    _ <- string ", "
    right <- some alphaNumChar
    _ <- char ')'
    return (encodeKey node, (encodeKey left, encodeKey right))

pMove :: Parser Move
pMove = choice
  [ MoveLeft <$ char 'L'
  , MoveRight <$ char 'R'
  ]

pInput :: Parser ([Move], M.Map Key (Key, Key))
pInput = do
    instructions <- some pMove
    _ <- newline <* newline
    netw <- pNetwork `sepBy` newline
    return (instructions, M.fromList netw)

main :: IO ()
main = do
    input <- parseInput 8 pInput
    print $ part1 input
    print $ part2 input

test1 :: IO ()
test1 = do
  input <- parseTest 8 1 pInput
  print $ part1 input == 6

test2 :: IO ()
test2 = do
  input <- parseTest 8 2 pInput
  print $ part2 input == 6


-- QuickCheck

genValidKey :: Gen [Char]
genValidKey = do
  a <- choose ('A', 'Z')
  b <- choose ('A', 'Z')
  c <- choose ('A', 'Z')
  return [a, b, c]

propEncKey :: Property
propEncKey = forAll genValidKey $ \key ->
      let encoded = encodeKey key
      in isLastCharOfEncKey (last key) encoded

checkProps :: IO ()
checkProps = quickCheck propEncKey