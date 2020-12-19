import Common ( readInput, parse )
import Data.Char ( isDigit )
import Text.ParserCombinators.ReadP

literal :: ReadP Int
literal = read <$> munch1 isDigit

plus :: ReadP (Int -> Int -> Int)
plus = (+) <$ char '+'

mult :: ReadP (Int -> Int -> Int)
mult = (*) <$ char '*'

parens :: ReadP a -> ReadP a
parens = between (char '(') (char ')')

exprLeftToRight :: ReadP Int
exprLeftToRight = chainl (parens exprLeftToRight <++ literal) (plus +++ mult) 0

plusTerm :: ReadP Int
plusTerm = do
    a <- parens exprAdvanced <++ literal
    char '+'
    b <- term
    return $ a + b

term :: ReadP Int
term = plusTerm <++ parens exprAdvanced <++ literal

exprAdvanced :: ReadP Int
exprAdvanced = chainl term mult 0

step1 :: String -> Int
step1 input = sum $ parse (sepBy1 exprLeftToRight (char '\n')) input

step2 :: String -> Int
step2 input = sum $ parse (sepBy1 exprAdvanced (char '\n')) input

main = do
    input <- filter (/=' ') <$> readInput 18
    print $ step1 input
    print $ step2 input
