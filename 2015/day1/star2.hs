import System.Environment
import System.IO
import Data.List

main = do
    args <- getArgs
    contents <- readFile (head args)
    let count = basementPos contents
    print count

basementPos :: String -> Int
basementPos x = basementPos1 0 0 x where
    basementPos1 accumulator position (x:xs)
        | accumulator < 0 = position
        | x == '(' = basementPos1 (accumulator + 1) (position + 1) xs
        | x == ')' = basementPos1 (accumulator - 1) (position + 1) xs
        | otherwise = 0
