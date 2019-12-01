import System.Environment
import Data.List

main :: IO ()
main = do
    (arg:_) <- getArgs
    contents <- readFile arg
    print (computeHousesVisited contents)

data Position = Position Int Int deriving (Eq)

computeHousesVisited :: String -> Int
computeHousesVisited contents =
    length (nub (computePositions contents))

computePositions :: [Char] -> [Position]
computePositions moves = scanl move (Position 0 0) moves

move :: Position -> Char -> Position
move (Position x y) '>' = Position (x + 1) y
move (Position x y) '^' = Position x (y + 1)
move (Position x y) 'v' = Position x (y - 1)
move (Position x y) '<' = Position (x - 1) y
move position _ = position
