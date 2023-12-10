{-# LANGUAGE TupleSections #-}
import InputParser
import qualified Data.Map as M
import Text.Megaparsec (some)
import Text.Megaparsec.Char (hspace1, alphaNumChar)
import Data.Maybe (fromMaybe)
import Data.Char (ord)
import Data.List (sortBy)

maybeHead :: [a] -> Maybe a
maybeHead []     = Nothing
maybeHead (a:_)  = Just a

charOccurrences :: [Char] -> M.Map Char Int
charOccurrences = M.fromListWith (+) . map (, 1)

compareFirstElementBy :: Ord a => (a -> a -> Ordering) -> (a, b) -> (a, b) -> Ordering
compareFirstElementBy cmp (x, _) (y, _) = cmp x y

compareHands :: [Char] -> [Char] -> Ordering
compareHands a b
    | cmpRanks /= EQ = cmpRanks
    | otherwise = fromMaybe EQ fstUnequalCard
    where cmpRanks = compare (rankHand a) (rankHand b)
          fstUnequalCard = maybeHead $ filter (/= EQ) $ zipWith (compareCards 11) a b

rankHand :: [Char] -> Int
rankHand hand
    | M.size cardCounts == 1   = 6 -- Five of a kind
    | cntSameCards == 4        = 5 -- Four of a kind
    | M.size cardCounts == 2   = 4 -- Full house
    | cntSameCards == 3        = 3 -- Three of a kind
    | M.size cardCounts == 3   = 2 -- Two pair
    | M.size cardCounts == 4   = 1 -- One pair
    | otherwise                = 0 -- High card
    where cardCounts = charOccurrences hand
          cntSameCards = maximum cardCounts

compareHandsJoker :: [Char] -> [Char] -> Ordering
compareHandsJoker a b
    | cmpRanks /= EQ = cmpRanks
    | otherwise = fromMaybe EQ fstUnequalCard
    where cmpRanks = compare (rankHandJoker a) (rankHandJoker b)
          fstUnequalCard = maybeHead $ filter (/= EQ) $ zipWith (compareCards 1) a b

rankHandJoker :: [Char] -> Int
rankHandJoker hand
    | cntJoker == 5                      = 6 -- Five of a kind (only Jokers)
    | M.size cardCountsWithoutJoker == 1 = 6 -- Five of a kind
    | cntSameCards == 4                  = 5 -- Four of a kind
    | M.size cardCountsWithoutJoker == 2 = 4 -- Full house
    | cntSameCards == 3                  = 3 -- Three of a kind
    | M.size cardCountsWithoutJoker == 3 = 2 -- Two pair
    | M.size cardCountsWithoutJoker == 4 = 1 -- One pair
    | otherwise                          = 0 -- High card
    where cardCounts = charOccurrences hand
          cardCountsWithoutJoker = M.delete 'J' cardCounts
          cntJoker = fromMaybe 0 $ M.lookup 'J' cardCounts
          cntSameCards = cntJoker + maximum cardCountsWithoutJoker

compareCards :: Int -> Char -> Char -> Ordering
compareCards jokerValue a b = compare (rankCard jokerValue a) (rankCard jokerValue b)

rankCard :: Int -> Char -> Int
rankCard jokerValue card
    | card >= '2' && card <= '9' = ord card - ord '0'
    | card == 'T' = 10
    | card == 'J' = jokerValue
    | card == 'Q' = 12
    | card == 'K' = 13
    | card == 'A' = 14
    | otherwise = error "this shouldn't happen"

part1 :: [([Char], Int)] -> Int
part1 hands = sum $ zipWith (*) [1..] $ map snd $ sortBy (compareFirstElementBy compareHands) hands

part2 :: [([Char], Int)] -> Int
part2 hands = sum $ zipWith (*) [1..] $ map snd $ sortBy (compareFirstElementBy compareHandsJoker) hands

pInput :: Parser ([Char], Int)
pInput = do
  hand <- some alphaNumChar
  hspace1
  bid <- number
  return (hand, bid)

main :: IO ()
main = do
    input <- parseInputLines 7 pInput
    print $ part1 input
    print $ part2 input

test1 :: IO ()
test1 = do
  input <- parseTestLines 7 1 pInput
  print $ part1 input == 6440

test2 :: IO ()
test2 = do
  input <- parseTestLines 7 1 pInput
  print $ part2 input == 5905