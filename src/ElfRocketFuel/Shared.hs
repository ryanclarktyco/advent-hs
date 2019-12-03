module ElfRocketFuel.Shared (massageData) where

massageData :: String -> [Int]
massageData input = map read (lines input)
