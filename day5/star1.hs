import System.Environment
import Data.List

main :: IO ()
main = do
    (filename:_) <- getArgs
    contents <- readFile filename
    let strings = lines contents
    print (computeNiceCount strings)

computeNiceCount :: [String] -> Int
computeNiceCount strings = length (filter isNice strings)

isNice :: String -> Bool
isNice string = (not $ hasBadDigraphs string) && hasDoubleDigraph string && hasThreeVowels string

hasBadDigraphs :: String -> Bool
hasBadDigraphs string =
    let badStrings = ["ab", "cd", "pq", "xy"]
        containsBadString = map (\x -> isInfixOf x string) badStrings
    in foldl (||) False containsBadString

hasDoubleDigraph :: String -> Bool
hasDoubleDigraph (x:xs)
    | xs == [] = False
    | x == (head xs) = True
    | otherwise = hasDoubleDigraph xs
hasDoubleDigraph [] = False

hasThreeVowels :: String -> Bool
hasThreeVowels string =
    let vowels = "aeiou"
    in (length (filter (`elem` vowels) string)) >= 3

convertBoolToInt :: Bool -> Int
convertBoolToInt True = 1
convertBoolToInt False = 0
