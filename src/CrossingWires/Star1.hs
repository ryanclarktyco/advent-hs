module CrossingWires.Star1 (main) where

import Data.Set
import CrossingWires.Shared

main :: String -> Int
main input = findMinimumDistance $ Data.Set.filter (>0) $ calculateDistances $ intersectPaths $ generatePathSets moveSets
  where moveSets = massageData input

findMinimumDistance :: Set Int -> Int
findMinimumDistance set = findMin set

calculateDistances :: Set Point -> Set Int
calculateDistances points = Data.Set.map (manhattanDistance Point { x = 0, y = 0 }) points

intersectPaths :: [Set Point] -> Set Point
intersectPaths (x:xs) =  Prelude.foldl (intersection) x xs

generatePathSets :: [[Move]] -> [Set Point]
generatePathSets = Prelude.map generatePathSet

generatePathSet :: [Move] -> Set Point
generatePathSet moves = fst $ Prelude.foldl (unionNewPointsWithMove) (empty, Point { x = 0, y = 0 }) moves

unionNewPointsWithMove :: (Set Point, Point) -> Move -> (Set Point, Point)
unionNewPointsWithMove (set, origin) move = (set `union` (generatePointsCrossed origin move), generateNewPoint origin move)
