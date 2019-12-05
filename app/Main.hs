module Main where

import Prelude hiding (lookup)
import Data.Maybe
import Data.Map
import System.Environment
import qualified ChronalCalibration.Star1
import qualified ChronalCalibration.Star2
import qualified ElfRocketFuel.Star1
import qualified ElfRocketFuel.Star2
import qualified ElfTuringMachine.Star1
import qualified ElfTuringMachine.Star2
import qualified CrossingWires.Star1

main :: IO ()
main = do 
  (year:day:star:filename:args) <- getArgs
  contents <- readFile filename
  let solver = getSolver (read year) (read day) (read star)
  if isJust solver
     then print $ fromJust solver contents
  else do
    print "No matching solver"

getSolver :: Int -> Int -> Int -> Maybe (String -> Int)
getSolver year day star = do
  yearSolver <- lookup year solverMap
  daySolver <- lookup day yearSolver
  starSolver <- lookup star daySolver
  return starSolver

solverMap :: Map Int (Map Int (Map Int (String -> Int)))
solverMap = fromList ([
  (2018,
  fromList([
  (1, fromList([
  (1, ChronalCalibration.Star1.main),
  (2, ChronalCalibration.Star2.main)
             ]))
           ])),
  (2019,
  fromList([
  (1, fromList([
  (1, ElfRocketFuel.Star1.main),
  (2, ElfRocketFuel.Star2.main)
             ])),
  (2, fromList([
  (1, ElfTuringMachine.Star1.main),
  (2, ElfTuringMachine.Star2.main)
             ])),
  (3, fromList([
  (1, CrossingWires.Star1.main)
             ]))
           ]))])
