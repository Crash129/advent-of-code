module Common where
import Text.ParserCombinators.ReadP ( readP_to_S, ReadP )

parse :: ReadP a -> String -> a
parse r s = fst $ last $ readP_to_S r s

readInput :: Int -> IO String
readInput i = do readFile $ "input/" ++ show i