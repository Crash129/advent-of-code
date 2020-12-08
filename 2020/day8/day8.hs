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

run :: Program -> State -> State
run p (acc, pc) = 
    let (t,f) = case M.lookup pc p of
                Just i -> i
                _ -> error $ "program counter out of bounds: " ++ show pc
    in case t of
            Nop -> (acc, pc + 1)
            Acc -> (f acc, pc + 1)
            Jmp -> (acc, f pc)

runProgram :: Program -> Result
runProgram p =
    let run' s (acc, pc)
            | pc == M.size p = Finish (acc, pc)
            | S.member pc' s = InfLoop (acc, pc)
            | otherwise = run' (S.insert pc s) (acc', pc')
            where (acc', pc') = run p (acc, pc)
    in run' S.empty (0,0)

step1 :: Program -> Int
step1 p = 
    case runProgram p of
        InfLoop (acc,_) -> acc
        _ -> error "No infinite loop found"

step2 :: Program -> Int
step2 p = 
    let js = M.filter (\(t,_) -> t == Jmp) p
        replaceNthJump n = M.insert k (Nop, (+) 0) p where (k,_) = M.elemAt n js
        f i = case runProgram (replaceNthJump i) of
                Finish (acc,_) -> acc
                InfLoop _ -> f (i+1)
    in f 0

main = do
    i <- readFile "input"
    let p = parse instructions i
    print $ step1 p
    print $ step2 p