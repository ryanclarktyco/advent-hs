module ElfTuringMachine.Shared (massageData, replaceValueAtPosition, runComputerAtPosition) where

import Data.List.Split
import Control.Monad.IO.Class

data Operation = ThreeOpcode Int Int Int Int | OneOpcode Int Int

massageData :: String -> [Int]
massageData input = map read $ splitOn "," input

runComputerAtPosition :: [Int] -> Int -> [Int]
runComputerAtPosition l i 
  | i >= (length l) = l
  | l !! i == 99 = l
  | otherwise = runComputerAtPosition tapeAfterMutation nextPosition
  where operation = getOperationForPosition l i
        nextPosition = i + getNextOffset operation
        mutation = compute operation
        tapeAfterMutation = applyMutation l mutation

compute :: Operation -> IO (Int, Int)
compute (ThreeOpcode 1 operand1 operand2 dest) = do
  return (operand1 + operand2, dest)
compute (ThreeOpcode 2 operand1 operand2 dest) = do
  return (operand1 * operand2, dest)
compute (OneOpcode 3 dest) = do
  newValue <- readInt
  return (newValue, dest)

readInt :: IO Int
readInt = readLn

getNextOffset :: Operation -> Int
getNextOffset (ThreeOpcode _ _ _ _) = 4
getNextOffset (OneOpcode _ _) = 2

applyMutation :: [Int] -> (Int, Int) -> [Int]
applyMutation l mutation = replaceValueAtPosition l (fst mutation) (snd mutation)

getOperationForPosition :: [Int] -> Int -> Operation
getOperationForPosition l i 
  | operation `elem` [1..2] = ThreeOpcode operation (getValueForMode l (i + 1) operand1Mode) (getValueForMode l (i + 2) operand2Mode) (l !! (i + 3))
  | operation `elem` [3..4] = OneOpcode operation (l !! (i + 1))
  where opcode = l !! i
        operation = getDigitForOpcode opcode 0
        operand1Mode = getDigitForOpcode opcode 2
        operand2Mode = getDigitForOpcode opcode 3

getValueForMode :: [Int] -> Int -> Int -> Int
getValueForMode l position mode
  | mode == 1 = l !! position
  | otherwise = loadValueFromPosition l position

loadValueFromPosition :: [Int] -> Int -> Int
loadValueFromPosition l i =  l !! (l !! i)

replaceValueAtPosition :: [a] -> Int -> a -> [a]
replaceValueAtPosition (x:xs) 0 y = y : xs
replaceValueAtPosition (x:xs) i y = [x] ++ replaceValueAtPosition xs (i - 1) y

digits :: Int -> [Int]
digits x = map ((read::String->Int) . (\x -> [x])) $ show x

getDigitForOpcode :: Int -> Int -> Int
getDigitForOpcode opcode digit = ((reverse $ digits opcode) ++ [0,0..]) !! digit
