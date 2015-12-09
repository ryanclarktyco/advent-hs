import System.Environment
import Data.List.Split

main :: IO ()
main = do
    (arg:_) <- getArgs
    contents <- readFile arg
    let boxesString = lines contents
    let boxStrings = map (splitOn "x") boxesString
    let areas = map boxArea boxStrings
    let totalArea = sum areas
    print totalArea

boxArea :: [String] -> Int
boxArea [lengths,widths,heights] =
    let length = read lengths
        width = read widths
        height = read heights
    in 2 * (sideArea [width, height]) + 2 * (frontArea [length, height]) + 2 * (topArea [length, width]) + slackArea [length, width, height]
boxArea _ = undefined

sideArea :: [Int] -> Int
sideArea [width, height] = width * height
sideArea _ = undefined

frontArea :: [Int] -> Int
frontArea [length, height] = length * height
frontArea _ = undefined

topArea :: [Int] -> Int
topArea [length, width] = length * width
topArea _ = undefined

slackArea :: [Int] -> Int
slackArea [length, width, height]
    | length <= width && height <= width = length * height
    | length <= height && width <= height = length * width
    | height <= length && width <= length = height * width
slackArea _ = undefined
