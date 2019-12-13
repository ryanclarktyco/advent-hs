module ChronalCalibration.Star1 (main) where

import ChronalCalibration.Shared

main :: String -> IO Int
main input = do
  return $ sum (massageData input)
