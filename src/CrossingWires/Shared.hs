module CrossingWires.Shared (massageData, Move, Point(Point, x, y), generateNewPoint, generatePointsCrossed, manhattanDistance) where

import qualified Data.Set as Set
import Data.List.Split
import Prelude hiding (Left, Right)

massageData :: String -> [[Move]]
massageData input = map parseLine (lines input)

data Direction = Up | Down | Left | Right deriving (Eq, Show, Enum)
data Move = Move { direction :: Direction, speed :: Int } deriving (Show)

data Point = Point { x :: Int, y :: Int } deriving (Show, Eq, Ord)

readMove :: String -> Move
readMove (x:xs) = Move { speed = read xs, direction = case x of 
                                                    'U' -> Up
                                                    'L' -> Left
                                                    'R' -> Right
                                                    'D' -> Down
  }

parseLine :: String -> [Move]
parseLine x = map readMove (splitOn "," x)

manhattanDistance :: Point -> Point -> Int
manhattanDistance a b = abs (x a - x b) + abs (y a - y b)

generateNewPoint :: Point -> Move -> Point
generateNewPoint point move = case direction move of
                    Up -> Point { x = x point, y = y point + speed move }
                    Down -> Point { x = x point, y = y point - speed move }
                    Left -> Point { x = x point - speed move, y = y point }
                    Right -> Point { x = x point + speed move, y = y point }


generatePointsCrossed :: Point -> Move -> Set.Set Point
generatePointsCrossed point move = Set.fromList(do
  n <- [1..(speed move)]
  let newPoint = (case direction move of
                    Up -> Point { x = x point, y = y point + n }
                    Down -> Point { x = x point, y = y point - n }
                    Left -> Point { x = x point - n, y = y point }
                    Right -> Point { x = x point + n, y = y point })
  return newPoint
                                           )
