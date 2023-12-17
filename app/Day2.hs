{-# LANGUAGE OverloadedStrings #-}

module Day2 (solution) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

solution :: T.Text -> Int -> T.Text
solution t part =
  let ts = T.lines t
      gs = map parseLine ts
      n = f bag gs -- for part 1
      ps = map power gs -- for part 2
   in if part == 1 then T.pack (show n) else T.pack (show (sum ps))
  where
    f :: CubeItemList -> [Game] -> Int
    f cis = foldr (g cis) 0
    g :: CubeItemList -> Game -> Int -> Int
    g cis (Game n ciss) n' = if all (contains cis) ciss then n + n' else n'
    bag :: CubeItemList
    bag = [("red", 12), ("green", 13), ("blue", 14)]

type Color = T.Text

type CubeItem = (Color, Int)

type CubeItemList = [CubeItem]

data Game = Game Int [CubeItemList]

parseLine :: T.Text -> Game
parseLine t =
  let ts = T.splitOn ":" t
      hts = T.splitOn " " (head ts)
      tts = T.splitOn ";" (T.dropWhile (== ' ') $ last ts)
      n = read (T.unpack $ last hts) :: Int
      pis = map f tts
   in Game n pis
  where
    f :: T.Text -> CubeItemList
    f t' = map g (T.splitOn "," t')
    g :: T.Text -> CubeItem
    g t' =
      let t'' = T.dropWhile (== ' ') t'
          ts' = T.splitOn " " t''
       in (last ts', read (T.unpack $ head ts'))

cubeItemListToMap :: CubeItemList -> Map String Int
cubeItemListToMap = foldr f Map.empty
  where
    f :: CubeItem -> Map String Int -> Map String Int
    f (color, n) m = case Map.lookup (T.unpack color) m of
      Nothing -> Map.insert (T.unpack color) n m
      Just k -> if n > k then Map.insert (T.unpack color) n m else m

-- if the 1st CubeItemList contains the 2nd
contains :: CubeItemList -> CubeItemList -> Bool
contains c1 c2 = let m = cubeItemListToMap c1 in foldr (f m) True (filter (\(_, n) -> n > 0) c2)
  where
    f :: Map String Int -> CubeItem -> Bool -> Bool
    f m (color, n) b = case Map.lookup (T.unpack color) m of
      Nothing -> False
      Just k -> n <= k && b

power :: Game -> Int
power (Game _ ciss) =
  let ms = map cubeItemListToMap ciss
      (r, g, b) = foldr f (0, 0, 0) ms
   in r * g * b
  where
    f :: Map String Int -> (Int, Int, Int) -> (Int, Int, Int)
    f m (r, g, b) =
      let r' = f' m "red" r
          g' = f' m "green" g
          b' = f' m "blue" b
       in (r', g', b')
    f' :: Map String Int -> String -> Int -> Int
    f' m s n = case Map.lookup s m of
      Nothing -> n
      Just n' -> max n' n
