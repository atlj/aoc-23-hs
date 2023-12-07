{-# LANGUAGE TemplateHaskell #-}

import Data.Char
import Data.FileEmbed (embedStringFile)
import Data.List
import Data.Ord
import Debug.Trace
import Test.HUnit

input :: String
input = $(embedStringFile "input/day7.txt")

getPoint :: Char -> Int
getPoint c
  | c == 'A' = 14
  | c == 'K' = 13
  | c == 'Q' = 12
  | c == 'T' = 10
  | c == 'J' = 1
  | isDigit c = digitToInt c

testUniques = TestCase $ do
  assertEqual "can group uniques" [5] (uniques "AAAAA")
  assertEqual "can group uniques" [5] (uniques "JJJJJ")
  assertEqual "can group uniques" [5] (uniques "AJJJJJ")
  assertEqual "can group uniques" [5] (uniques "JTTJT")
  assertEqual "can group uniques" [5] (uniques "AAAAJ")
  assertEqual "can group uniques" [4, 1] (uniques "KTJJT")
  assertEqual "can group uniques" [4, 1] (uniques "AA8AA")
  assertEqual "can group uniques" [4, 1] (uniques "AA8AJ")
  assertEqual "can group uniques" [3, 2] (uniques "23332")
  assertEqual "can group uniques" [3, 1, 1] (uniques "TTT98")
  assertEqual "can group uniques" [2, 2, 1] (uniques "23432")
  assertEqual "can group uniques" [2, 1, 1, 1] (uniques "A23A4")
  assertEqual "can group uniques" [1, 1, 1, 1, 1] (uniques "23456")

uniques :: String -> [Int]
uniques s = cardCounts
  where
    filteredJesters = filter (/= 'J') s
    jesterCount = length s - length filteredJesters
    cardCounts' = sortBy (comparing Data.Ord.Down) $ map length $ group $ sort filteredJesters
    cardCounts = if jesterCount == 5 then [5] else head cardCounts' + jesterCount : tail cardCounts'

parseHand :: String -> Int
parseHand s = case uniques s of
  [5] -> 8
  [4, 1] -> 7
  [3, 2] -> 6
  [3, 1, 1] -> 5
  [2, 2, 1] -> 4
  [2, 1, 1, 1] -> 3
  [1, 1, 1, 1, 1] -> 2

testCompareHands = TestCase $ do
  assertEqual "compares hands with jester" GT (compareHandsByPoints "TJTJJ" "J5555")

compareHandsByPoints :: String -> String -> Ordering
compareHandsByPoints s1 s2 = case compare (getPoint $ head s1) (getPoint $ head s2) of
  EQ -> compareHandsByPoints (tail s1) (tail s2)
  other -> other

compareHands :: String -> String -> Ordering
compareHands s1 s2 = case compare (parseHand s1) (parseHand s2) of
  EQ -> compareHandsByPoints s1 s2
  other -> other

parseInput :: String -> [(String, Int)]
parseInput s = map ((\[hand, multiplier] -> (hand, read multiplier)) . words) (lines s)

main' = do
  let sortedHands = sortBy (\(hand1, _) (hand2, _) -> compareHands hand1 hand2) $ parseInput input
  print . foldr (\((_, multiplier), rank) acc -> acc + rank * multiplier) 0 $ zip sortedHands [1 ..]

main :: IO ()
main = do
  result <- runTestTT $ TestList [testUniques, testCompareHands]
  main'
