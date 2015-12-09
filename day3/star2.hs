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

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = as : splitEvery n bs 
  where (as,bs) = splitAt n xs

computePositions :: [Char] -> [Position]
computePositions moves = 
    let splitMoves = splitEvery 2 moves
        allPositions = scanl moveAll [(Position 0 0), (Position 0 0)] splitMoves
    in concat allPositions

moveAll :: [Position] -> [Char] -> [Position]
moveAll [santaPosition, robotPosition] [santaMove, robotMove] = [move santaPosition santaMove, move robotPosition robotMove]
moveAll [santaPosition, robotPosition] _ = [santaPosition, robotPosition]
moveAll _ _ = undefined

move :: Position -> Char -> Position
move (Position x y) '>' = Position (x + 1) y
move (Position x y) '^' = Position x (y + 1)
move (Position x y) 'v' = Position x (y - 1)
move (Position x y) '<' = Position (x - 1) y
move position _ = position
