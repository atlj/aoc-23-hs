{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedStringFile)
import Test.HUnit

input :: String
input = $(embedStringFile "input/day9.txt")

calculateDx :: [Int] -> [Int]
calculateDx l = case l of
  [x1, x2] -> [x1 - x2]
  x1 : x2 : xs -> x1 - x2 : calculateDx (x2 : xs)

findPrevious :: [Int] -> Int
findPrevious l = case head l of
  0 -> 0
  x -> head l + findPrevious (calculateDx l)

parseLine :: String -> [Int]
parseLine s = map read $ words s

main :: IO ()
main = do
  print $ sum . map (findPrevious . reverse . parseLine) $ lines input
  print $ sum . map (findPrevious . parseLine) $ lines input
