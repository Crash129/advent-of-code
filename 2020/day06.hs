import Common ( readInput, parse )
import Text.ParserCombinators.ReadP
import Data.Char(isAlpha)
import qualified Data.Set as S

-- TODO solve by bit shifting

type Person = S.Set Char
type Group = [Person]

personParser :: ReadP Person
personParser = do 
    x <- munch1 isAlpha
    return $ S.fromList x

groupParser :: ReadP Group
groupParser = do sepBy personParser $ char '\n'

groupsParser :: ReadP [Group]
groupsParser = do sepBy groupParser $ count 2 $ char '\n'

step1 :: [Group] -> Int
step1 gs = sum $ map (S.size . S.unions) gs

step2 :: [Group] -> Int
step2 gs = sum $ map (S.size . foldl1 S.intersection) gs

main = do
    i <- readInput 6
    let gs = parse groupsParser i 
    print $ step1 gs
    print $ step2 gs