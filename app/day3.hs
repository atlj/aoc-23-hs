{-# LANGUAGE TemplateHaskell #-}

import Data.Char
import Data.FileEmbed (embedStringFile)
import Data.List
import Data.List.GroupBy ()
import Data.Maybe
import Debug.Trace
import Test.HUnit

input :: String
input = $(embedStringFile "input/day3.txt")

type Maze = [String]

type Pos = (Int, Int) -- x, y

type Visited = [Pos]

testGetCell = TestCase $ do
  assertEqual "Can get valid cell" (Just 'b') (getCell (1, 0) ["abc", "def", "ghi"])
  assertEqual "Return nothing on negative y" Nothing (getCell (-1, 0) ["abc", "def", "ghi"])
  assertEqual "Return nothing on negative x" Nothing (getCell (0, -1) ["abc", "def", "ghi"])
  assertEqual "Return nothing on out of bounds x" Nothing (getCell (3, 0) ["abc", "def", "ghi"])
  assertEqual "Return nothing on out of bounds y" Nothing (getCell (0, 4) ["abc", "def", "ghi"])

getCell :: Pos -> Maze -> Maybe Char
getCell pos maze
  | fst pos >= 0 && snd pos >= 0 && fst pos < length (head maze) && snd pos < length maze = Just $ maze !! snd pos !! fst pos
  | otherwise = Nothing

neighbourMap :: [Pos]
neighbourMap = [(1, 0), (-1, 0), (0, 1), (0, -1), (-1, 1), (1, -1), (1, 1), (-1, -1)]

testGetNeighboringCells = TestCase $ do
  assertEqual "Can get neighboring cells" [(6, 5), (4, 5), (5, 6), (5, 4), (4, 6), (6, 4), (6, 6), (4, 4)] (getNeighboringCells (5, 5))

getNeighboringCells :: Pos -> [Pos]
getNeighboringCells pos = (\p -> (fst p + fst pos, snd p + snd pos)) <$> neighbourMap

getValueToSort :: Pos -> Int
getValueToSort pos = fst pos * 10 + snd pos

testGetNextCell = TestCase $ do
  assertEqual "get next x" (1, 0) (getNextCell ["...", "..."] (0, 0))
  assertEqual "get next y" (0, 1) (getNextCell ["...", "..."] (2, 0))

getNextCell :: Maze -> Pos -> Pos
getNextCell maze pos
  | fst pos < length (head maze) - 1 = (fst pos + 1, snd pos)
  | otherwise = (0, snd pos + 1)

testGetPreviousCell = TestCase $ do
  assertEqual "get previous x" (0, 0) (getPreviousCell ["...", "..."] (1, 0))
  assertEqual "get previous y" (2, 0) (getPreviousCell ["...", "..."] (0, 1))

getPreviousCell :: Maze -> Pos -> Pos
getPreviousCell maze pos
  | fst pos > 0 = (fst pos - 1, snd pos)
  | otherwise = (length (head maze) - 1, snd pos - 1)

isIslandValid :: Maze -> Visited -> Pos -> Maybe Pos
isIslandValid maze visited pos =
  case getCell pos maze of
    Just currentCell | pos `elem` visited -> Nothing
    Just currentCell
      | isDigit currentCell ->
          let positions = map ((isIslandValid maze (pos : visited))) (getNeighboringCells pos)
              result = filter (isJust) positions
           in if not (null result) then head result else Nothing
    Just '*' -> Just pos
    _ -> Nothing

testJoinDecimal = TestCase $ do
  assertEqual "Basic decimal jon" 7000 (joinDecimal [7, 0, 0, 0])
  assertEqual "Basic decimal join" 492 (joinDecimal [4, 9, 2])

joinDecimal :: [Int] -> Int
joinDecimal input = foldr (\(digit, power) acc -> acc + (digit * (10 ^ power))) 0 $ zip (reverse input) [0 ..]

searchValidIslands :: Maze -> Pos -> [Int] -> [(Int, Pos)] -> [(Int, Pos)]
searchValidIslands maze pos currentStack resultStack =
  case getCell pos maze of
    Just currentCell
      | isDigit currentCell && (fst pos == length (head maze) - 1) ->
          let isValid = isIslandValid maze [] (getPreviousCell maze pos)
           in case isValid of
                Just asterixPos -> searchValidIslands maze (getNextCell maze pos) [] ((joinDecimal (currentStack ++ [digitToInt currentCell]), asterixPos) : resultStack)
                Nothing -> searchValidIslands maze (getNextCell maze pos) [] resultStack
    Just currentCell | isDigit currentCell -> searchValidIslands maze (getNextCell maze pos) (currentStack ++ [digitToInt currentCell]) resultStack
    Just currentCell ->
      let isValid = isIslandValid maze [] (getPreviousCell maze pos)
       in case isValid of
            Just asterixPos -> searchValidIslands maze (getNextCell maze pos) [] ((joinDecimal currentStack, asterixPos) : resultStack) -- Symbol
            Nothing -> searchValidIslands maze (getNextCell maze pos) [] resultStack
    Nothing -> resultStack -- Traveled the whole list

main' = do
  print . sum . map (\[a, b] -> (fst a) * (fst b)) $ filter (\islandGroup -> length islandGroup > 1) $ groupBy (\(_, posA) (_, posB) -> (fst posA == fst posB) && (snd posA == snd posB)) $ filter (\pos -> fst pos /= 0) $ sortBy (\(_, posA) (_, posB) -> compare (getValueToSort posA) (getValueToSort posB)) $ searchValidIslands (lines input) (0, 0) [] []

main :: IO ()
main = do
  result <- runTestTT $ TestList [testGetCell, testGetNeighboringCells, testGetNextCell, testJoinDecimal, testGetPreviousCell]
  main'
