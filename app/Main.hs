module Main where

import Control.Monad (when)
import Data.Maybe (fromJust, isNothing)
import Lib (runAdventOfCode)
import System.Environment (getArgs)
import System.Exit (die)
import Text.Printf (printf)
import Text.Read (readMaybe)

main :: IO ()
main = do
  args <- getArgs
  when (null args) $ die "Not enough arguments, need a day number!"
  let day = readMaybe (head args) :: Maybe Int
  when (isNothing day) $ die (printf "Invalid argument %s, must be a number" (head args))
  let dayNum = fromJust day
  runAdventOfCode dayNum
