{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedStringFile)
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe
import Debug.Trace
import Test.HUnit

input :: String
input = $(embedStringFile "input/day8.txt")

testHello = TestCase $ do
  assertEqual "can say hello to John Do" "Hello John Doe" (hello "John Doe")

hello :: String -> String
hello name = "Hello " ++ name

type ListNode = (String, (String, String))

type NodeMap = Map.Map String (String, String)

parseNode :: String -> ListNode
parseNode s = (name, (left, right))
  where
    [name, connections] = splitOn " = " s
    [left', right'] = splitOn ", " connections
    left = tail left'
    right = init right'

parseInput :: String -> (String, [ListNode])
parseInput s = (instructions, nodes)
  where
    instructions : _ : nodes' = lines s
    nodes = map parseNode nodes'

walkNodes :: String -> String -> String -> String -> NodeMap -> Int -> Int
walkNodes instructions currInstructions start needle nodes currSteps
  | start == needle = currSteps
  | otherwise = walkNodes instructions nextInstructions source needle nodes (currSteps + 1)
  where
    nextInstructions = if length currInstructions == 1 then instructions else tail currInstructions
    currNode = fromJust $ Map.lookup start nodes
    source = if head currInstructions == 'L' then fst currNode else snd currNode

walkNodes2 :: String -> String -> String -> NodeMap -> Int -> Int
walkNodes2 instructions currInstructions start nodes currSteps
  | last start == 'Z' = currSteps
  | otherwise = walkNodes2 instructions nextInstructions source nodes (currSteps + 1)
  where
    nextInstructions = if length currInstructions == 1 then instructions else tail currInstructions
    currNode = fromJust $ Map.lookup start nodes
    source = if head currInstructions == 'L' then fst currNode else snd currNode

main' = do
  let (instructions, nodes') = parseInput input
  let nodes = Map.fromList nodes'
  let starts = filter (\node -> last node == 'A') $ map fst nodes'
  let solutions = map (\start -> walkNodes2 instructions instructions start nodes 0) starts
  print solutions -- Just used an online calculator to get their least common multiplier
  -- print $ walkNodes instructions instructions "AAA" "ZZZ" nodes 0
  print "hi mom"

main :: IO ()
main = do
  result <- runTestTT $ TestList [testHello]
  main'
