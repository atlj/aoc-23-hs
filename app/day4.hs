{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedStringFile)
import Data.List.Split
import Test.HUnit
import Prelude hiding (id)

input :: String
input = $(embedStringFile "input/day4.txt")

data Card = Card
  { id :: Int,
    wNumbers :: [Int],
    numbers :: [Int]
  }
  deriving (Eq, Show)

testParseNumbers = TestCase $ do
  assertEqual "Can parse numbers" [83, 86, 6, 31, 17, 9, 48, 53] (parseNumbers "83 86  6 31 17  9 48 53" [])

parseNumbers :: String -> [Int] -> [Int]
parseNumbers (x1 : x2 : x3 : xs) output = parseNumbers xs (output ++ [read [x1, x2, x3]])
parseNumbers (x1 : x2 : xs) output = parseNumbers xs (output ++ [read [x1, x2]])
parseNumbers [] output = output

testParseCard = TestCase $ do
  assertEqual "can parse card" (Card {id = 391, wNumbers = [41, 48, 83, 86, 17], numbers = [83, 86, 6, 31, 17, 9, 48, 53]}) (parseCard "Card 391: 41 48 83 86 17 | 83 86  6 31 17  9 48 53")

parseCard :: String -> Card
parseCard line =
  let [cardRow, numbers'] = splitOn ": " line
      [wNumbers, numbers] = splitOn " | " numbers'
      [_, gameNum] = splitOn "Card" cardRow
   in Card {id = read gameNum :: Int, wNumbers = parseNumbers wNumbers [], numbers = parseNumbers numbers []}

testCalculatePoint = TestCase $ do
  assertEqual "calculate point" 8 (calculatePoint (Card {id = 391, wNumbers = [41, 48, 83, 86, 17], numbers = [83, 86, 6, 31, 17, 9, 48, 53]}))

calculatePoint :: Card -> Int
calculatePoint card =
  let wNum = wNumbers card
      num = numbers card
      result = filter (`elem` wNum) num
   in case length result of
        len | len > 0 -> 2 ^ (len - 1)
        _ -> 0

calculateMatching :: Card -> Int
calculateMatching card = length . filter (`elem` wNumbers card) $ numbers card

iterateCopyCount :: Int -> Int -> Int -> [Int] -> [Int]
iterateCopyCount 0 _ _ copyCount = copyCount
iterateCopyCount remainingIterations currentIndex times copyCount = iterateCopyCount (remainingIterations - 1) (currentIndex + 1) times [if (snd x) == currentIndex then (fst x + times) else (fst x) | x <- zip copyCount [0 ..]]

parseWithMultipliers :: [String] -> [Int] -> Int
parseWithMultipliers [] copyCount = sum copyCount
parseWithMultipliers (line : lines) copyCount' =
  let card = parseCard line
      count = copyCount' !! (id card - 1)
      copyCount = iterateCopyCount (calculateMatching card) (id card) count copyCount'
   in parseWithMultipliers lines copyCount

main' = do
  print . sum $ calculatePoint . parseCard <$> lines input
  print $ parseWithMultipliers (lines input) [1 | _ <- lines input]

main :: IO ()
main = do
  result <- runTestTT $ TestList [testParseCard, testParseNumbers, testCalculatePoint]
  main'
