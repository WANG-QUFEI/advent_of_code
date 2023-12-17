module Day1 (solution) where

import Data.Char (isDigit)
import qualified Data.Text as T
import Debug.Trace (trace)

solution :: T.Text -> Int -> T.Text
solution content _ =
  let ls = T.lines content
      ls' = map (keepOnlyDigits . substitute) ls
      ns = map (\t -> read [T.head t, T.last t] :: Int) ls'
      nsum = sum ns
   in trace (T.unpack . T.unlines $ ls') T.pack $ show nsum

substitute :: T.Text -> T.Text
substitute = T.foldl sub T.empty
  where
    sub :: T.Text -> Char -> T.Text
    sub t c =
      let t' = T.snoc t c
       in foldr (\(w, n) t'' -> if T.isSuffixOf (T.pack w) t'' then T.replace (T.pack w) (T.pack n) t'' else t'') t' wordsToNums

keepOnlyDigits :: T.Text -> T.Text
keepOnlyDigits = T.filter isDigit

wordsToNums :: [(String, String)]
wordsToNums =
  [ ("one", "1"),
    ("two", "2"),
    ("three", "3"),
    ("four", "4"),
    ("five", "5"),
    ("six", "6"),
    ("seven", "7"),
    ("eight", "8"),
    ("nine", "9")
  ]
