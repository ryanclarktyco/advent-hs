module ElfTuringMachine.Star1 ( main ) where

import ElfTuringMachine.Shared

main :: String -> IO Int
main input = do
  let result = runComputer $ massageData input
  return result
  

hotFixInstructions :: [Int] -> [Int]
hotFixInstructions l = replaceValueAtPosition (replaceValueAtPosition l 1 12) 2 2

runComputer :: [Int] -> Int
runComputer l = head (runComputerAtPosition (hotFixInstructions l) 0)
