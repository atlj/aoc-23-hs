{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedStringFile)
import Data.Ix
import Data.List
import Test.HUnit

input :: String
input = $(embedStringFile "input/day11.txt")

type Pos = (Int, Int)

merge :: [[a]] -> [a]
merge = concat . transpose

getIndicesToExpand :: Int -> [Int] -> [String] -> [Int]
getIndicesToExpand _ indices [] = reverse indices
getIndicesToExpand currIndex indicies' (x : xs) = getIndicesToExpand (currIndex + 1) indices xs
  where
    indices = if '#' `elem` x then indicies' else currIndex : indicies'

galaxyPositions :: [String] -> [Pos]
galaxyPositions lines = intercalate [] [[(x, y) | (x, char) <- zip [0 ..] line, char == '#'] | (y, line) <- zip [0 ..] lines]

comparePos :: Pos -> Pos -> Ordering
comparePos (x1, y1) (x2, y2) = case compare y1 y2 of
  EQ -> compare x1 x2
  notEqual -> notEqual

createPairs :: [(a, a)] -> [a] -> [(a, a)]
createPairs stack [] = stack
createPairs stack' (x : xs) = createPairs stack xs
  where
    stack = [(x, y) | y <- xs] ++ stack'

diffInPair :: Pos -> Pos -> [Int] -> [Int] -> Int
diffInPair (x1, y1) (x2, y2) xIndicesToExpand yIndicesToExpand = abs (x1 - x2) + abs (y1 - y2) + xExpanded + yExpanded
  where
    xExpanded = expand xIndicesToExpand x1 x2
    yExpanded = expand yIndicesToExpand y1 y2

expand :: [Int] -> Int -> Int -> Int
expand indices x1 x2 = 999999 * length [x | x <- indices, inRange (min x1 x2, max x1 x2) x]

main' = do
  let columns = transpose . reverse $ lines input
  let horizontalIndicesToExpand = getIndicesToExpand 0 [] $ lines input
  let verticalIndicesToExpand = getIndicesToExpand 0 [] columns
  let positions = galaxyPositions $ lines input
  print $ sum . map (\(pos1, pos2) -> diffInPair pos1 pos2 verticalIndicesToExpand horizontalIndicesToExpand) $ createPairs [] positions

main :: IO ()
main = do
  result <- runTestTT $ TestList []
  main'
