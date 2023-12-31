{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedStringFile)
import Data.List
import Data.Maybe (fromJust, fromMaybe, isJust)
import Debug.Trace
import Test.HUnit

data Dir = W | E | N | S deriving (Show, Eq)

data Pipe = NS | WE | NE | NW | SW | SE deriving (Show, Eq)

data Tile = Pipe Pipe | Start deriving (Show, Eq)

type Pos = (Int, Int)

type Maze = [[Maybe Tile]]

parseMaze :: [[Char]] -> Maze
parseMaze = map (map parseTile)

applyPipe :: Dir -> Pipe -> Maybe Dir
applyPipe yourRelativeDir pipe = case (yourRelativeDir, pipe) of
  (N, NS) -> Just S
  (S, NS) -> Just N
  --
  (W, WE) -> Just E
  (E, WE) -> Just W
  --
  (N, NE) -> Just E
  (E, NE) -> Just N
  --
  (N, NW) -> Just W
  (W, NW) -> Just N
  --
  (S, SW) -> Just W
  (W, SW) -> Just S
  --
  (S, SE) -> Just E
  (E, SE) -> Just S
  --
  _ -> Nothing

oppositeDir :: Dir -> Dir
oppositeDir N = S
oppositeDir S = N
oppositeDir W = E
oppositeDir E = W

applyDir :: Pos -> Dir -> Pos
applyDir pos dir = case dir of
  W -> (fst pos - 1, snd pos)
  E -> (fst pos + 1, snd pos)
  N -> (fst pos, snd pos - 1)
  S -> (fst pos, snd pos + 1)

getTile :: Maze -> Pos -> Maybe Tile
getTile maze pos = (maze !! snd pos) !! fst pos

getStart :: Maze -> Int -> Pos
getStart maze currentY = case elemIndex (Just Start) (head maze) of
  Just x -> (x, currentY)
  _ -> getStart (tail maze) (currentY + 1)

parseTile :: Char -> Maybe Tile
parseTile 'S' = Just Start
parseTile c = case parsePipe c of
  Just pipe -> Just $ Pipe pipe
  Nothing -> Nothing

parsePipe :: Char -> Maybe Pipe
parsePipe c = case c of
  '|' -> Just NS
  '-' -> Just WE
  'L' -> Just NE
  'J' -> Just NW
  '7' -> Just SW
  'F' -> Just SE
  _ -> Nothing

getLoop :: Maze -> Pos -> [Tile] -> [Tile]
getLoop maze pos tilesStack = tilesStack

validTilesNextToStart :: Maze -> Pos -> [(Dir, Pos)]
validTilesNextToStart maze startPos = [(dir, applyDir startPos dir) | dir <- [N, S, E, W], isJust $ getTile maze (applyDir startPos dir)]

input :: String
input = $(embedStringFile "input/day10.txt")

travel :: Maze -> Dir -> Pos -> [(Pos, Pipe)] -> [(Pos, Pipe)]
travel maze previousDir currPos stack = case getTile maze currPos of
  Just Start -> reverse ((currPos, SE) : stack) -- Start was like this on my data, deal with it
  Just (Pipe pipe) -> travel maze dir (applyDir currPos dir) ((currPos, pipe) : stack)
    where
      dir = fromJust (applyPipe (oppositeDir previousDir) pipe)
  Nothing -> error "invalid til"

edgePipes :: [Pipe]
edgePipes = [NE, NW, SW, SE]

edges :: [(Pos, Pipe)] -> [Pos]
edges input = map fst $ filter (\(_, pipe) -> pipe `elem` edgePipes) input

pairs [] = []
pairs xs = zip xs (tail xs)

shoelace :: [Pos] -> Int
shoelace positions = abs $ sum [(x1 * y2) - (x2 * y1) | ((x1, y1), (x2, y2)) <- pairs (positions ++ [head positions])] `div` 2

main' = do
  let maze = parseMaze $ lines input
  let start = getStart maze 0
  let (startDir, startPos) = head $ validTilesNextToStart maze start
  let loop = travel maze startDir startPos []
  let travelLength = length loop
  let edgeList = edges loop
  print $ shoelace edgeList - div travelLength 2 + 1 -- No idea why, it finds the correct answer when you pass + 1

main :: IO ()
main = do
  result <- runTestTT $ TestList []
  main'
