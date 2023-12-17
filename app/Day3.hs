module Day3 (solution) where

import Data.Char (isDigit)
import qualified Data.Text as T

data State = State {rowNum :: Int, colNum :: Int, accu :: String, adjacent :: Bool, partNum :: [Int]}

solution :: T.Text -> Int -> T.Text
solution input _ =
  let ts = T.lines input
      rows = length ts
      cols = T.length (head ts)
      ns = partNumbers rows cols ts
   in T.pack (show (sum ns))

partNumbers :: Int -> Int -> [T.Text] -> [Int]
partNumbers totalNumOfRows totalNumOfCols ts =
  let s = foldr rowIter (State (totalNumOfRows - 1) (-1) "" False []) ts in partNum s
  where
    rowIter :: T.Text -> State -> State
    rowIter t s = let s' = foldr colIter s {colNum = totalNumOfCols - 1, accu = "", adjacent = False} (T.unpack t) in s' {rowNum = rowNum s' - 1}
    colIter :: Char -> State -> State
    colIter c s
      | isDigit c =
          let s' =
                if adjacent s
                  then s {colNum = colNum s - 1, accu = c : accu s}
                  else s {colNum = colNum s - 1, accu = c : accu s, adjacent = adjacentToSymbol (rowNum s) (colNum s) (map T.unpack ts)}
           in if colNum s' == -1 && adjacent s'
                then s' {partNum = (read . accu $ s') : partNum s'}
                else s'
      | otherwise =
          if null (accu s)
            then s {colNum = colNum s - 1}
            else
              if adjacent s
                then s {colNum = colNum s - 1, accu = "", adjacent = False, partNum = (read . accu $ s) : partNum s}
                else s {colNum = colNum s - 1, accu = ""}

adjacentToSymbol :: Int -> Int -> [String] -> Bool
adjacentToSymbol row col ts =
  let fs = [checkBefore, checkBeforeUp, checkBeforeDown, checkAfter, checkAfterUp, checkAfterDown, checkTop, checkBottom]
   in any (\f -> f (row, col) ts) fs
  where
    checkBefore (r, c) = checkIsSymbol (r, c - 1)
    checkBeforeUp (r, c) = checkIsSymbol (r - 1, c - 1)
    checkBeforeDown (r, c) = checkIsSymbol (r + 1, c - 1)
    checkAfter (r, c) = checkIsSymbol (r, c + 1)
    checkAfterUp (r, c) = checkIsSymbol (r - 1, c + 1)
    checkAfterDown (r, c) = checkIsSymbol (r + 1, c + 1)
    checkTop (r, c) = checkIsSymbol (r - 1, c)
    checkBottom (r, c) = checkIsSymbol (r + 1, c)

checkIsSymbol :: (Int, Int) -> [String] -> Bool
checkIsSymbol (row, col) ts =
  withinScope (row, col) ts
    && ( let line = ts !! row
             char = line !! col
          in not ((char == '.') || isDigit char)
       )

withinScope :: (Int, Int) -> [String] -> Bool
withinScope (row, col) ss
  | row < 0 = False
  | col < 0 = False
  | row >= length ss = False
  | col >= length (head ss) = False
  | otherwise = True
