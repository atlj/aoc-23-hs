{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedStringFile)
import Data.Ix
import Data.List
import Data.List.Split
import Debug.Trace
import Test.HUnit

input :: String
input = $(embedStringFile "input/day5.txt")

-- enum
data Item = Seed | Soil | Fertilizer | Water | Light | Temperature | Humidity | Location

data Mapping = Mapping
  { -- from :: Item,
    -- to :: Item,
    customRanges :: [Range]
  }
  deriving (Show, Eq)

data Range = Range {from :: (Int, Int), to :: Int} deriving (Show, Eq)

testParseRange = TestCase $ do
  assertEqual "parse range" (Range (98, 99) 50) (parseRange "50 98 2")
  assertEqual "parse range 2" (Range (50, 97) 52) (parseRange "52 50 48")

parseRange :: String -> Range
parseRange s = Range (fromStart, fromEnd) toStart
  where
    [fromStart, toStart, count] = map read $ words s
    fromEnd = fromStart + count - 1

testParseMapping = TestCase $ do
  assertEqual "parse mapping" (Mapping [Range (98, 99) 50, Range (50, 97) 52]) (parseMapping "seed-to-soil map:\n50 98 2\n52 50 48")

parseMapping :: String -> Mapping
parseMapping s = Mapping $ map parseRange $ tail $ lines s

-- also get the seeds line: seeds: 79 14 55 13
parseInput :: String -> ([(Int, Int)], [Mapping])
parseInput s = (seeds, mappings)
  where
    (seedsLine : _ : mappingsLines') = lines s
    mappingLines = splitOn "\n\n" $ intercalate "\n" mappingsLines'
    -- Seeds are actually pairs. So a seed line would be: seeds: 79 14 55 13
    -- this would mean two ranges 79 to 92 and 55 to 67
    seeds = map (\[start, count] -> (start, start + count - 1)) $ chunksOf 2 (map read $ tail $ words seedsLine)
    mappings = map parseMapping mappingLines

testApplyMapping = TestCase $ do
  let mapping = Mapping [Range (98, 99) 50, Range (50, 97) 52]
  assertEqual "apply mappig" 50 (applyMapping (mapping) 98)
  assertEqual "apply mappng 2" 81 (applyMapping (mapping) 79)
  assertEqual "apply mappng kee" 13 (applyMapping (mapping) 13)

testApplyMappings = TestCase $ do
  let mappings = [Mapping [Range (98, 99) 50, Range (50, 97) 52], Mapping {customRanges = [Range {from = (15, 51), to = 0}, Range {from = (52, 53), to = 37}, Range {from = (0, 14), to = 39}]}]
  assertEqual "mapping works" 81 (applyMappings mappings 79)

applyMappings :: [Mapping] -> Int -> Int
applyMappings mappings seed = foldr applyMapping seed mappings

-- If the seed is in the ranges of mapping's customRanges, then apply the mapping
-- Else return the seed
applyMapping :: Mapping -> Int -> Int
applyMapping (Mapping {customRanges}) seed = case filter (\range -> inRange (from range) seed) customRanges of
  [] -> seed
  [Range {from = (fromStart, fromEnd), to}] -> (seed - fromStart) + to
  _ -> error "Multiple ranges    found"

isElementInSeedsRange :: [(Int, Int)] -> Int -> Bool
isElementInSeedsRange seeds seed = any (\(start, end) -> inRange (start, end) seed) seeds

main' = do
  let (seeds, mappings) = parseInput input
  print $ find (\seed -> (isElementInSeedsRange seeds $ applyMappings mappings $ seed)) [0 ..]

-- print seeds

main :: IO ()
main = do
  -- result <- runTestTT $ TestList [testParseRange, testApplyMapping, testParseMapping, testApplyMappings]
  main'
