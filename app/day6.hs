{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedStringFile)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (isJust)
import Test.HUnit

input :: String
input = $(embedStringFile "input/day6.txt")

data Race = Race
  { time :: Int,
    distance :: Int
  }
  deriving (Show)

parseRaces :: [String] -> [Race]
parseRaces [timeRow', distanceRow'] =
  let [_, times'] = splitOn "Time:" timeRow'
      time = [read $ intercalate "" $ filter (/= "") $ splitOn " " times']
      [_, distances'] = splitOn "Distance:" distanceRow'
      distances = [read $ intercalate "" $ filter (/= "") $ splitOn " " distances']
   in [Race {time = fst x, distance = snd x} | x <- zip time distances]

testWins = TestCase $ do
  assertEqual "calculate wis" 4 (wins 7 7 9)
  assertEqual "calculate wins" 8 (wins 15 15 40)
  assertEqual "calculate wins" 9 (wins 30 30 200)

calculateTotal :: Int -> Int -> Int
calculateTotal maximum total = abs (total - (2 * maximum)) + 1

wins :: Int -> Int -> Int -> Int
wins setupTime totalTime goal
  | (totalTime - setupTime) * setupTime > goal = calculateTotal setupTime totalTime
  | otherwise = wins (setupTime - 1) totalTime goal

main' = do
  print . foldr (\x acc -> acc * x) 1 $ map (\race -> wins (time race) (time race) (distance race)) $ parseRaces $ lines input

main :: IO ()
main = do
  result <- runTestTT $ TestList [testWins]
  main'
