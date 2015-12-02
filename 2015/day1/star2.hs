import System.Environment

main :: IO ()
main = do
    args <- getArgs
    contents <- readFile (head args)
    let count = basementPos contents
    print count

basementPos :: String -> Int
basementPos x = basementPos1 0 0 x where
    basementPos1 :: Int -> Int -> String -> Int
    basementPos1 _ _ [] = 0
    basementPos1 accumulator position (y:ys)
        | accumulator < 0 = position
        | y == '(' = basementPos1 (accumulator + 1) (position + 1) ys
        | y == ')' = basementPos1 (accumulator - 1) (position + 1) ys
        | otherwise = 0
