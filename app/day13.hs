{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedStringFile)
import Data.List
import Data.List.Split
import Data.Maybe
import Test.HUnit

input :: String
input = $(embedStringFile "input/day13.txt")

testRockPositions = TestCase $ do
  assertEqual "can get rock positins" [0, 2, 3, 6, 7] (rockIndices "#.##..##.")

rockIndices :: String -> [Int]
rockIndices s = [pos | (pos, tile) <- zip [0 ..] s, tile == '#']

areLinesSame :: [String] -> [String] -> Int -> Bool
areLinesSame [] _ smudgeCount = smudgeCount == 1
areLinesSame _ [] smudgeCount = smudgeCount == 1
areLinesSame (a : as) (b : bs) smudgeCount' = if smudgeCount > 1 then False else areLinesSame as bs (smudgeCount + smudgeCount')
  where
    smudgeCount = length $ filter (uncurry (/=)) $ zip a b

getMirrorIndex :: Int -> [String] -> Maybe Int
getMirrorIndex index s
  | index == length s = Nothing
  | otherwise = if areLinesSame (reverse a) b 0 then Just index else getMirrorIndex (index + 1) s
  where
    (a, b) = splitAt index s

main' = do
  let patterns = splitOn "\n\n" input
  let horizontalMirrors = map (getMirrorIndex 1 . lines) patterns
  let verticalMirrors = map (getMirrorIndex 1 . transpose . reverse . lines) patterns
  let horizontalSum = sum . catMaybes $ horizontalMirrors
  let verticalSum = sum . catMaybes $ verticalMirrors
  print horizontalMirrors
  print $ (horizontalSum * 100) + verticalSum

main :: IO ()
main = do
  result <- runTestTT $ TestList [testRockPositions]
  main'
