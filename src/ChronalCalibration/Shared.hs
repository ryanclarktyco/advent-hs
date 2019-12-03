module ChronalCalibration.Shared (massageData) where

massageData :: String -> [Int]
massageData input = map parseLine (lines input)

parseLine :: String -> Int
parseLine x
  | head x == '+' = read (tail x)
  | head x == '-' = -1 * read (tail x)
