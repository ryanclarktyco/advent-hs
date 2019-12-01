import System.Environment
import Data.List.Split

main :: IO ()
main = do
    (arg:_) <- getArgs
    contents <- readFile arg
    let totalLength = computeLength contents
    print totalLength
    

computeLength :: String -> Int
computeLength contents =
    let boxesString = lines contents
        boxStrings = map (splitOn "x") boxesString
        boxes = map (map read) boxStrings
        ribbonLengths = map ribbonLength boxes
    in sum ribbonLengths

ribbonLength :: [Int] -> Int
ribbonLength [l,w,h] = volume [l, w, h] + smallestPerimeter [l, w, h]
ribbonLength _ = undefined

smallestPerimeter :: [Int] -> Int
smallestPerimeter [l,w,h] = minimum [sidePerimeter [l, w], sidePerimeter [l, h], sidePerimeter [w, h]]
smallestPerimeter _ = undefined

sidePerimeter :: [Int] -> Int
sidePerimeter [side1, side2] = 2 * side1 + 2 * side2
sidePerimeter _ = undefined

volume :: [Int] -> Int
volume [l,w,h] = l * w * h
volume _ = undefined
