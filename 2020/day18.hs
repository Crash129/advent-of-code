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

term :: ReadP (Int -> Int)
term = do  
    op <- operator
    b <- parens expression <++ literal
    return $ op b

parens :: ReadP a -> ReadP a
parens = between (char '(') (char ')')

expression :: ReadP Int
expression = do
    start <- parens expression <++ literal
    ts <- many term
    return $ foldl (flip (.)) id ts start

step1 :: String -> Int
step1 input = sum $ parse (sepBy1 expression (char '\n')) input

main = do
    input <- filter (/=' ') <$> readInput 18
    print $ step1 input