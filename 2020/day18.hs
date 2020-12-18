import Common ( readInput, parse )
import Data.Char ( isDigit )
import Text.ParserCombinators.ReadP

literal :: ReadP Int
literal = read <$> munch1 isDigit

plus :: ReadP (Int -> Int -> Int)
plus = (+) <$ char '+'

mult :: ReadP (Int -> Int -> Int)
mult = (*) <$ char '*'

operator :: ReadP (Int -> Int -> Int)
operator = plus +++ mult

parens :: ReadP a -> ReadP a
parens = between (char '(') (char ')')

func :: ReadP (Int -> Int)
func = do  
    op <- operator
    b <- parens exprLeftToRight <++ literal
    return $ op b

exprLeftToRight :: ReadP Int
exprLeftToRight = do
    start <- parens exprLeftToRight <++ literal
    fs <- many func
    return $ foldl (flip (.)) id fs start

plusTerm :: ReadP Int
plusTerm = do
    a <- parens exprAdvanced <++ literal
    char '+'
    b <- term
    return $ a + b

mulFunc :: ReadP (Int -> Int)
mulFunc = mult <*> term

term :: ReadP Int
term = plusTerm <++ parens exprAdvanced <++ literal

exprAdvanced :: ReadP Int
exprAdvanced = do
    start <- term
    fs <- many mulFunc
    return $ foldl (flip (.)) id fs start

step1 :: String -> Int
step1 input = sum $ parse (sepBy1 exprLeftToRight (char '\n')) input

step2 :: String -> Int
step2 input = sum $ parse (sepBy1 exprAdvanced (char '\n')) input

main = do
    input <- filter (/=' ') <$> readInput 18
    print $ step1 input
    print $ step2 input
