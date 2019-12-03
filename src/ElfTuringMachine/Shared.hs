module ElfTuringMachine.Shared (massageData, replaceValueAtPosition, runComputerAtPosition) where

import Data.List.Split

data Operation = Operation Int Int Int Int

massageData :: String -> [Int]
massageData input = map read $ splitOn "," input

runComputerAtPosition :: [Int] -> Int -> [Int]
runComputerAtPosition l i 
  | i >= (length l) = l
  | l !! i == 99 = l
  | otherwise = runComputerAtPosition (applyMutation l (compute (getOperationForPosition l i))) (i + 4)

compute :: Operation -> (Int, Int)
compute (Operation 1 operand1 operand2 dest) = (operand1 + operand2, dest)
compute (Operation 2 operand1 operand2 dest) = (operand1 * operand2, dest)


applyMutation :: [Int] -> (Int, Int) -> [Int]
applyMutation l (newValue, newPosition) = replaceValueAtPosition l newPosition newValue

getOperationForPosition :: [Int] -> Int -> Operation
getOperationForPosition l i = Operation (l !! i) (loadValueFromPosition l (i + 1)) (loadValueFromPosition l (i + 2)) (l !! (i + 3))

loadValueFromPosition :: [Int] -> Int -> Int
loadValueFromPosition l i =  l !! (l !! i)

replaceValueAtPosition :: [a] -> Int -> a -> [a]
replaceValueAtPosition (x:xs) 0 y = y : xs
replaceValueAtPosition (x:xs) i y = [x] ++ replaceValueAtPosition xs (i - 1) y
