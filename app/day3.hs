{-# LANGUAGE TemplateHaskell #-}

import Data.Char
import Data.FileEmbed (embedStringFile)
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

testGetNextCell = TestCase $ do
  assertEqual "get next x" (1, 0) (getNextCell ["...", "..."] (0, 0))
  assertEqual "get next y" (0, 1) (getNextCell ["...", "..."] (2, 0))

getNextCell :: Maze -> Pos -> Pos
getNextCell maze pos
  | fst pos < length (head maze) - 1 = (fst pos + 1, snd pos)
  | otherwise = (0, snd pos + 1)

testIsIslandValid = TestCase $ do
  assertEqual "Can detect valid island" True (isIslandValid ["123..", ".*..."] [] (0, 0))
  assertEqual "Can detect invalid island" False (isIslandValid ["123..", "....."] [] (0, 0))
  assertEqual "Can detect invalid iland with far symbol" False (isIslandValid ["123..", "....*"] [] (0, 0))
  assertEqual "Can detect diagonal symbol" True (isIslandValid ["123..", "...*."] [] (0, 0))

isIslandValid :: Maze -> Visited -> Pos -> Bool
isIslandValid maze visited pos =
  case getCell pos maze of
    Just currentCell | pos `elem` visited -> False
    Just currentCell | isDigit currentCell -> any (isIslandValid maze (pos : visited)) (getNeighboringCells pos)
    Just '.' -> False
    Just currentCell -> True -- Symbol, means current island is valid
    Nothing -> False

testJoinDecimal = TestCase $ do
  assertEqual "Basic decimal join" 7000 (joinDecimal [7, 0, 0, 0])
  assertEqual "Basic decimal join" 492 (joinDecimal [4, 9, 2])

joinDecimal :: [Int] -> Int
joinDecimal input = foldr (\(digit, power) acc -> acc + (digit * (10 ^ power))) 0 $ zip (reverse input) [0 ..]

searchValidIslands :: Maze -> Pos -> [Int] -> [Int] -> [Int]
searchValidIslands maze pos currentStack resultStack =
  case getCell pos maze of
    Just currentCell | isDigit currentCell && (fst pos == length (head maze) - 1) -> searchValidIslands maze (getNextCell maze pos) [] ((if isIslandValid maze [] pos then joinDecimal (currentStack ++ [digitToInt currentCell]) else 0) : resultStack)
    Just currentCell | isDigit currentCell -> searchValidIslands maze (getNextCell maze pos) (currentStack ++ [digitToInt currentCell]) resultStack
    Just currentCell -> searchValidIslands maze (getNextCell maze pos) [] ((if isIslandValid maze [] (fst pos - 1, snd pos) then joinDecimal currentStack else 0) : resultStack) -- Symbol
    Nothing -> (if not (null currentStack) then joinDecimal currentStack else 0) : resultStack -- Traveled the whole list

main' = do
  print . sum . filter (/= 0) $ searchValidIslands (lines input) (0, 0) [] []

main :: IO ()
main = do
  result <- runTestTT $ TestList [testGetCell, testGetNeighboringCells, testIsIslandValid, testGetNextCell, testJoinDecimal]
  main'
