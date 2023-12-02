{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed (embedStringFile)
import Data.List.Split (splitOn)
import Prelude hiding (id)

data Round = Round
  { id :: Int,
    cubeSets :: [CubeSet]
  }
  deriving (Show)

data CubeSet = Cubeset
  { red :: Int,
    green :: Int,
    blue :: Int
  }
  deriving (Show)

zeroCubes = Cubeset {red = 0, green = 0, blue = 0}

add :: CubeSet -> CubeSet -> CubeSet
add cubesA cubesB =
  Cubeset
    { red = red cubesA + red cubesB,
      green = green cubesA + green cubesB,
      blue = blue cubesA + blue cubesB
    }

parseRound :: String -> Round
parseRound input =
  let [idPart, cubeSets'] = splitOn ": " input
      [_, id'] = splitOn " " idPart
      id = read id'
      cubeSets = map sumCubesets (splitOn "; " cubeSets')
   in Round
        { id,
          cubeSets
        }

sumCubesets :: String -> CubeSet
sumCubesets input =
  let cubes = map parseCubeSet $ splitOn ", " input
   in foldr add zeroCubes cubes

parseCubeSet :: String -> CubeSet
parseCubeSet input =
  let [count', color] = splitOn " " input
      count = read count'
   in case color of
        "red" -> Cubeset {red = count, green = 0, blue = 0}
        "green" -> Cubeset {red = 0, green = count, blue = 0}
        "blue" -> Cubeset {red = 0, green = 0, blue = count}
        _ -> error "Unknown color"

cubeQuoata = Cubeset {red = 12, green = 13, blue = 14}

haveEnoughCubes :: CubeSet -> Bool
haveEnoughCubes cubes = red cubes <= red cubeQuoata && green cubes <= green cubeQuoata && blue cubes <= blue cubeQuoata

findMaxCubeSet :: [CubeSet] -> CubeSet
findMaxCubeSet =
  foldr
    ( \curr acc ->
        Cubeset
          { red = max (red curr) (red acc),
            green = max (green curr) (green acc),
            blue = max (blue curr) (blue acc)
          }
    )
    zeroCubes

power :: CubeSet -> Int
power Cubeset {red, green, blue} = red * green * blue

input :: String
input = $(embedStringFile "input/day2.txt")

main :: IO ()
main = do
  putStrLn $ "Part1: " ++ show (foldr (\round b -> b + id round) 0 . filter (all haveEnoughCubes . cubeSets) . map parseRound $ lines input)
  putStrLn $ "Part2: " ++ show (foldr (\round b -> b + (power . findMaxCubeSet $ cubeSets round)) 0 . map parseRound $ lines input)
