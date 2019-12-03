module ElfTuringMachine.Star1 ( main ) where

import ElfTuringMachine.Shared

main :: String -> Int
main input = runComputer $ massageData input

hotFixInstructions :: [Int] -> [Int]
hotFixInstructions l = replaceValueAtPosition (replaceValueAtPosition l 1 12) 2 2

runComputer :: [Int] -> Int
runComputer l = head (runComputerAtPosition (hotFixInstructions l) 0)
