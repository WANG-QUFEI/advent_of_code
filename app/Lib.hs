{-# LANGUAGE OverloadedLists #-}

module Lib
  ( runAdventOfCode
  ) where

import Text.Printf (printf)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isNothing, fromJust)
import Control.Monad (when, unless)
import System.Exit (exitFailure)
import System.Directory (doesFileExist)
import qualified Data.Text.IO as TIO
import qualified Day1 as D1
import qualified Day2 as D2
import qualified Day3 as D3

newtype Solution = Solution (T.Text -> Int -> T.Text)

runAdventOfCode :: Int -> IO ()
runAdventOfCode day = do
  let ms = getSolutionByDay day
  when (isNothing ms) $ putStr (printf "day %d solution is not available!" day) >> exitFailure
  ready <- checkInputFiles day
  unless ready $ putStr (printf "input for %d under 'data' directry is not ready!\n" day) >> exitFailure
  -- run solution on sample input
  sample <- TIO.readFile $ printf "data/day%d_sample.txt" day
  when (T.null sample) $ putStrLn "sample file is empty, skip it"
  unless (T.null sample) $ do {
    putStrLn "running solution on sample file, for part 1, the result is:";
    runSolution (fromJust ms) sample 1;
    putStrLn "running solution on sample file, for part 2, the result is:";
    runSolution (fromJust ms) sample 2;
  }
  -- run solution on actual input
  actual <- TIO.readFile $ printf "data/day%d.txt" day
  when (T.null actual) $ putStrLn "actual file is empty, skip it"
  unless (T.null actual) $ do {
    putStrLn "running solution on actual file, for part 1, the result is:";
    runSolution (fromJust ms) actual 1;
    putStrLn "running solution on actual file, for part 2, the result is:";
    runSolution (fromJust ms) actual 2;
    }

runSolution :: Solution -> T.Text -> Int -> IO ()
runSolution (Solution s) content part = do
  let r = T.unpack $ s content part
  putStrLn r


checkInputFiles :: Int -> IO Bool
checkInputFiles day = do
  let sample = printf "data/day%d_sample.txt" day
      actual = printf "data/day%d.txt" day
  b1 <- doesFileExist sample
  b2 <- doesFileExist actual
  return (b1 && b2)


getSolutionByDay :: Int -> Maybe Solution
getSolutionByDay day = Map.lookup day allSolutions
  where  allSolutions :: Map Int Solution
         allSolutions = [
           (1, Solution D1.solution),
           (2, Solution D2.solution),
           (3, Solution D3.solution)
           ]
