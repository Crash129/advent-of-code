import Text.ParserCombinators.ReadP
import Data.Char ( isDigit )
import qualified Data.Map as M
import qualified Data.Set as S

data Token = Nop | Acc | Jmp deriving (Show, Enum, Eq)
data Result = InfLoop State | Finish State

type Instruction = (Token, Int -> Int)
type Program = M.Map Int Instruction
type State = (Int, Int)

instruction :: ReadP Instruction
instruction = do
    i <- (Nop <$ string "nop") +++ (Acc <$ string "acc") +++ (Jmp <$ string "jmp")
    skipSpaces
    f <- ((+) <$ char '+') +++ (flip (-) <$ char '-')
    x <- munch1 isDigit
    return (i, f (read x))

instructions :: ReadP Program
instructions = do
    is <- sepBy instruction $ char '\n'
    return $ M.fromList $ zip [0..] is

parse :: ReadP a -> String -> a
parse r s = fst $ last $ readP_to_S r s

step :: Program -> State -> State
step p (acc, pc) = 
    let (t,f) = case M.lookup pc p of
                Just i -> i
                _ -> error $ "program counter out of bounds: " ++ show pc
    in case t of
            Nop -> (acc, pc + 1)
            Acc -> (f acc, pc + 1)
            Jmp -> (acc, f pc)

run :: Program -> Result
run p =
    let step' s (acc, pc)
            | pc == M.size p = Finish (acc, pc)
            | S.member pc' s = InfLoop (acc, pc)
            | otherwise = step' (S.insert pc s) (acc', pc')
            where (acc', pc') = step p (acc, pc)
    in step' S.empty (0,0)

step1 :: Program -> Int
step1 p = 
    case run p of
        InfLoop (acc,_) -> acc
        _ -> error "No infinite loop found"

step2 :: Program -> Int
step2 p = 
    let ks = M.keys $ M.filter (\(t,_) -> t == Jmp) p
        nop k = M.insert k (Nop, (+) 0) p
        f [] = error "No terminating program found"
        f (k:ks) = 
             case run (nop k) of
                Finish (acc,_) -> acc
                InfLoop _ -> f ks
    in f ks

main = do
    i <- readFile "input"
    let p = parse instructions i
    print $ step1 p
    print $ step2 p